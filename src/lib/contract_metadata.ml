(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 TQ Tezos <contact@tqtezos.com>                         *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open! Base
open Import

module Uri = struct
  let validate uri_code =
    let open Metadata_uri in
    let errors = ref [] in
    let error w src e = errors := (w, src, e) :: !errors in
    let validate_kt1_address s =
      ( try ignore (B58_hashes.check_b58_kt1_hash s) with
      | Failure f -> error `Address s f
      | e -> Fmt.kstr (error `Address s) "%a" Exn.pp e ) ;
      Ok () in
    let validate_network = function
      | "mainnet" | "carthagenet" | "delphinet" | "dalphanet" | "zeronet" ->
          Ok ()
      | s ->
          ( try ignore (B58_hashes.check_b58_chain_id_hash s) with
          | Failure f -> error `Network s f
          | e -> Fmt.kstr (error `Network s) "%a" Exn.pp e ) ;
          Ok () in
    let uri =
      Uri.of_string uri_code |> of_uri ~validate_kt1_address ~validate_network
    in
    (uri, List.rev !errors)

  module Fetcher = struct
    type gateway = {main: string; alternate: string}
    type t = {gateway: gateway}

    let create () =
      let main = "https://gateway.ipfs.io/ipfs/" in
      let alternate = "https://dweb.link/ipfs/" in
      {gateway= {main; alternate}}

    let get (ctxt : < fetcher: t ; .. > Context.t) = ctxt#fetcher
    let gateway ctxt = (get ctxt).gateway
  end

  let rec needs_context_address =
    let open Metadata_uri in
    function
    | Storage {address= None; _} -> true
    | Web _ | Storage _ | Ipfs _ -> false
    | Hash {target; _} -> needs_context_address target

  let to_ipfs_gateway ?(alt_gateway = false) ctxt ~cid ~path =
    let gateway =
      if alt_gateway then (Fetcher.gateway ctxt).alternate
      else (Fetcher.gateway ctxt).main in
    Fmt.str "%s%s%s" gateway cid path

  let fetch ?limit_bytes ?prefix ctxt uri ~current_contract =
    let log =
      match prefix with
      | None -> dbgf ctxt#formatter "Uri.fetch.log: %s"
      | Some prefix -> dbgf ctxt#formatter "%s: %s" prefix in
    let open Lwt.Infix in
    let logf fmt = Fmt.kstr (fun s -> log s) fmt in
    let not_implemented s = Fmt.failwith "Not Implemented: %s" s in
    dbgf ctxt#formatter "Fetching ============== " ;
    let rec resolve =
      let open Metadata_uri in
      function
      | Web http_uri ->
          logf "HTTP %S" http_uri ;
          ctxt#http_get ?limit_bytes http_uri
      | Ipfs {cid; path} ->
          logf "IPFS CID %S path %S" cid path ;
          let gatewayed = to_ipfs_gateway ctxt ~cid ~path in
          (* resolve (Web gatewayed) *)
          Lwt.catch
            (fun () -> resolve (Web gatewayed))
            (fun e ->
              dbgf ctxt#formatter
                "Trying alternate IPFS gateway due to exception: %s"
                (Exn.to_string e) ;
              let gatewayed_alt =
                to_ipfs_gateway ctxt ~alt_gateway:true ~cid ~path in
              resolve (Web gatewayed_alt) )
      | Storage {network= None; address; key} ->
          let addr =
            match address with
            | Some s -> s
            | None -> (
              match current_contract with
              | None -> Fmt.failwith "Missing current contract"
              | Some s -> s ) in
          logf "Using address %S (key = %S)" addr key ;
          Query_nodes.metadata_value ctxt ~address:addr ~key ~log
      | Storage {network= Some network; address; key} ->
          logf "storage %s %a %S" network Fmt.Dump.(option string) address key ;
          Fmt.kstr not_implemented "storage uri with network = %s" network
      | Hash {kind= `Sha256; value; target} -> (
          let expected =
            match Digestif.of_raw_string_opt Digestif.sha256 value with
            | Some s -> s
            | None ->
                Fmt.failwith "%a is not a valid SHA256 hash" Hex.pp
                  (Hex.of_string value) in
          logf "sha256: %a" (Digestif.pp Digestif.sha256) expected ;
          resolve target
          >>= fun content ->
          let obtained = Digestif.digest_string Digestif.sha256 content in
          logf "hash of content: %a" (Digestif.pp Digestif.sha256) obtained ;
          match Digestif.unsafe_compare Digestif.sha256 expected obtained with
          | 0 -> Lwt.return content
          | _ ->
              Fmt.failwith "Hash of content %a is different from expected %a"
                (Digestif.pp Digestif.sha256)
                obtained
                (Digestif.pp Digestif.sha256)
                expected ) in
    resolve uri
end

module Content = struct
  let of_json s =
    try
      let warnings = ref [] in
      let jsonm =
        let j = Ezjsonm.value_from_string s in
        let rec fix = function
          | (`String _ | `Float _ | `Bool _ | `Null) as v -> v
          | `A l -> `A (List.map l ~f:fix)
          | `O kvl ->
              let f (k, v) =
                let fix_warn o k =
                  ( match
                      List.exists !warnings ~f:(function
                          | `Fixed_legacy (a, _) -> String.equal a o )
                    with
                  | true -> ()
                  | false -> warnings := `Fixed_legacy (o, k) :: !warnings ) ;
                  (k, fix v) in
                match k with
                | "michelson-storage-view" -> fix_warn k "michelsonStorageView"
                | "return-type" -> fix_warn k "returnType"
                | other -> (other, fix v) in
              `O (List.map kvl ~f) in
        fix j in
      let contents = Json_encoding.destruct Metadata_contents.encoding jsonm in
      Ok (!warnings, contents)
    with e -> Tezos_error_monad.Error_monad.error_exn e

  let rec find_token_metadata_big_map ~storage_node ~type_node =
    let open Tezos_micheline.Micheline in
    let go (storage_node, type_node) =
      find_token_metadata_big_map ~storage_node ~type_node in
    let check_annots annotations node =
      if List.mem annotations "%token_metadata" ~equal:String.equal then
        Decorate_error.raise
          Message.(
            text "Wrong %token_metadata annotation:"
            %% kpp inline_code Micheline_helpers.pp_arbitrary_micheline node)
    in
    match (storage_node, type_node) with
    | Prim (_, "Pair", [l; r], ans), Prim (_, "pair", [lt; rt], ant) ->
        check_annots ans storage_node ;
        check_annots ant type_node ;
        go (l, lt) @ go (r, rt)
    | ( Int (_, z)
      , Prim
          ( _
          , "big_map"
          , [ Prim (_, "nat", [], _)
            ; Prim
                ( _
                , "pair"
                , [ Prim (_, "nat", [], _)
                  ; Prim
                      ( _
                      , "map"
                      , [Prim (_, "string", [], _); Prim (_, "bytes", [], _)]
                      , _ ) ]
                , _ ) ]
          , annotations ) )
      when List.mem annotations "%token_metadata" ~equal:String.equal ->
        [z]
    | Int (_, _z), Prim (_, "big_map", _, annots) ->
        check_annots annots type_node ;
        []
    | Int (_, _z), _ -> []
    | String (_, _s), _ -> []
    | Bytes (_, _b), _ -> []
    | Prim (_, _prim, _args, annot), _t ->
        check_annots annot storage_node ;
        []
    | Seq (_, _l), _t -> []

  let token_metadata_value ctxt ~address ~(log : string -> unit) =
    let open Lwt in
    let open Query_nodes in
    let logf f = Fmt.kstr log f in
    find_node_with_contract ctxt address
    >>= fun node ->
    dbgf ctxt#formatter "Found contract with node %S" node.Node.name ;
    Node.metadata_big_map ctxt node ~address ~log
    >>= fun metacontract ->
    let Node.Contract.{storage_node; type_node; _} = metacontract in
    let tmbm_id =
      match find_token_metadata_big_map ~storage_node ~type_node with
      | [one] -> one
      | other ->
          Decorate_error.raise
            Message.(
              text "Wrong number of %token_metadata big-maps:"
              %% int inline_code (List.length other)) in
    logf "Token-Metadata big-map: %s" (Z.to_string tmbm_id) ;
    Lwt.return tmbm_id
end
