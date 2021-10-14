(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 TQ Tezos <contact@tqtezos.com>                         *)
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

open Cmdliner
open Tezos_contract_metadata
open Tezos_contract_metadata.Import
open Stdio

(*
open Protocol
open Tezos_micheline
open Client_proto_context
open Client_proto_contracts
open Tezos_contract_metadata

module Log = Internal_event.Legacy_logging.Make (struct
  let name = Printf.sprintf "client.%s.metadata_commands" Protocol.name
end)

let get_big_map_string_at_string (cctxt : #Protocol_client_context.full) bm
    ~key =
  let (data, typ) =
    let open Alpha_context.Script in
    let open Micheline in
    ( strip_locations (String (0, key)),
      strip_locations (Prim (0, T_string, [], [])) )
  in
  Alpha_services.Helpers.Scripts.pack_data
    cctxt
    (cctxt#chain, cctxt#block)
    ?gas:None
    ~data
    ~ty:typ
  >>=? fun (bytes, _remaining_gas) ->
  get_big_map_value
    cctxt
    ~chain:cctxt#chain
    ~block:cctxt#block
    bm
    (Script_expr_hash.hash_bytes [bytes])
  >>=? fun expr ->
  match Michelson_v1_primitives.strings_of_prims expr |> Micheline.root with
  | Bytes (_, s) ->
      return (Bytes.to_string s)
  | _ ->
      cctxt#error
        "Value at %S is not a string: %a"
        key
        Michelson_v1_printer.print_expr_unwrapped
        expr

let get_storage_and_type_micheline (cctxt : #Protocol_client_context.full)
    ~chain ~block contract =
  get_script cctxt ~chain ~block contract
  >>=? (function
         | None ->
             cctxt#error "This is not a smart contract."
         | Some script ->
             return script)
  >>=? fun script ->
  ( match script.code |> Data_encoding.force_decode with
  | None ->
      cctxt#error "Cannot decode storage."
  | Some s ->
      return s )
  >>=? fun code ->
  get_storage cctxt ~chain:cctxt#chain ~block:cctxt#block contract
  >>=? function
  | None ->
      cctxt#error "This is not a smart contract."
  | Some storage ->
      let storage_root_node =
        let string_micheline : string Tezos_micheline.Micheline.canonical =
          Michelson_v1_primitives.strings_of_prims storage
        in
        Micheline.root string_micheline
      in
      let (type_root_node, parameter_root_node) =
        let string_micheline : string Tezos_micheline.Micheline.canonical =
          Michelson_v1_primitives.strings_of_prims code
        in
        ( Micheline_helpers.get_storage_type_exn string_micheline,
          Micheline_helpers.get_parameter_type_exn string_micheline )
      in
      return (storage_root_node, type_root_node, parameter_root_node)

let get_metadata_big_map_value (cctxt : #Protocol_client_context.full) ~chain
    ~block contract ~key =
  get_storage_and_type_micheline cctxt ~chain ~block contract
  >>=? fun (storage_node, type_node, _) ->
  match Micheline_helpers.find_metadata_big_maps ~storage_node ~type_node with
  | [] ->
      cctxt#error
        "Cannot find metadata big-map within@ '%a'@ of@ type@ '%a'"
        Micheline_helpers.pp_arbitrary_micheline
        storage_node
        Micheline_helpers.pp_arbitrary_micheline
        type_node
  | [one] ->
      (* dbg "TODO: explore %a" Z.pp_print one ;  *)
      get_big_map_string_at_string
        cctxt
        (Protocol.Alpha_context.Big_map.Id.parse_z one)
        ~key
  | more ->
      cctxt#error
        "Found too many (= %d) metadata big-maps within@ '%a'@ of@ type@ '%a'"
        (List.length more)
        Micheline_helpers.pp_arbitrary_micheline
        storage_node
        Micheline_helpers.pp_arbitrary_micheline
        type_node

let get_metadata_of_contract ?(strict = true) cctxt ~contract =
  get_metadata_big_map_value
    cctxt
    ~chain:cctxt#chain
    ~block:cctxt#block
    contract
    ~key:""
  >>=? fun value ->
  let uri = Uri.of_string value in
  let (validate_network, validate_kt1_address) =
    if strict then
      ( Some
          (function
          | "mainnet" | "carthagenet" | "delphinet" | "dalphanet" ->
              ok ()
          | chain_id ->
              Tezos_crypto.Chain_id.of_b58check chain_id >>? fun _ -> ok ()),
        Some
          (fun s ->
            match Protocol.Contract_hash.of_b58check_opt s with
            | Some _kt1 ->
                ok ()
            | None ->
                error (failure "%S is not a valid contract address" s)) )
    else (None, None)
  in
  Lwt.return (Metadata_uri.of_uri ?validate_network ?validate_kt1_address uri)
  >>=? fun parsed_uri ->
  fetch_uri_contents cctxt ~current_contract:contract parsed_uri

let group =
  {
    Clic.name = "contract-metadata";
    title =
      "Block contextual commands related to the Contract Metadata (TZIP-16)";
  }

module Output_path = struct
  let arg =
    Clic.(
      arg
        ~doc:"Output path"
        ~long:"output"
        ~placeholder:"PATH"
        (parameter (fun _ s -> return s)))

  let output ?(none = fun _ -> return ()) output_path_opt content =
    match output_path_opt with
    | None ->
        none content
    | Some s ->
        Lwt_io.with_file ~mode:Lwt_io.output s (fun ochan ->
            Lwt_io.fprint ochan content)
        >>= fun () -> return_unit
end

let all =
  let open Clic in
  [ command
      ~group
      ~desc:"Get the JSON-Schema and example of TZIP-016 metadata."
      (args2
         (switch ~doc:"Also output examples." ~long:"with-examples" ())
         (default_arg
            ~doc:"Output path"
            ~long:"output"
            ~placeholder:"PATH"
            ~default:"/tmp/tzip-16-metadata"
            (parameter (fun _ s -> return s))))
      (fixed ["get"; "contract"; "metadata"; "schema"])
      (fun (with_examples, output_path) (cctxt : Protocol_client_context.full) ->
        cctxt#message
          "Outputting to %S with%s examples."
          output_path
          (if with_examples then "" else "out")
        >>= fun () ->
        Lwt_utils_unix.create_dir output_path
        >>= fun () ->
        let schema = Json_encoding.schema Metadata_contents.encoding in
        let path = Filename.(concat output_path "schema.json") in
        cctxt#message "Outputting schema to %S." path
        >>= fun () ->
        Lwt_io.with_file ~mode:Lwt_io.output path (fun o ->
            Lwt_io.fprintl
              o
              ( Json_schema.to_json schema
              |> Ezjsonm.value_to_string ~minify:false ))
        >>= fun () ->
        cctxt#message "Outputting examples."
        >>= fun () ->
        if with_examples then
          Lwt_list.iteri_s
            (fun ith example ->
              let path =
                Filename.(concat output_path (Fmt.str "example-%03d.json" ith))
              in
              cctxt#message
                "@[<2>Outputting example #%d@ to %S:@ %a@]"
                ith
                path
                Metadata_contents.pp
                example
              >>= fun () ->
              Lwt_io.with_file ~mode:Lwt_io.output path (fun o ->
                  let jzon =
                    Json_encoding.construct Metadata_contents.encoding example
                    |> Ezjsonm.value_to_string ~minify:false
                  in
                  Lwt_io.fprintl o jzon))
            (Metadata_contents.Example.all ())
          >>= fun () -> return_unit
        else return_unit);
    command
      ~group
      ~desc:"Fetch and output the metadata from a KT1 contract."
      (args3
         (default_arg
            ~doc:
              "Output format: 'text', 'text:short', 'text:full', 'raw', or \
               'json'."
            ~long:"format"
            ~placeholder:"FORMAT"
            ~default:"JSON"
            (parameter (fun _ s ->
                 match String.lowercase_ascii s with
                 | "text" ->
                     return (`Text `Full)
                 | "text:full" ->
                     return (`Text `Full)
                 | "text:short" ->
                     return (`Text `Short)
                 | "raw" ->
                     return `Raw
                 | "json" ->
                     return `Json
                 | other ->
                     failwith "Output format unknown: %S" other)))
         Output_path.arg
         (arg
            ~doc:
              "Also display extra validation errors and warnings. Default: \
               true when output format is text, false otherwise."
            ~long:"validate"
            ~placeholder:"BOOL"
            (parameter (fun _ s ->
                 match String.lowercase_ascii s with
                 | "true" ->
                     return true
                 | "false" ->
                     return false
                 | other ->
                     failwith "Cannot understand boolean %S" other))))
      ( prefixes ["get"; "metadata"; "for"; "contract"]
      @@ ContractAlias.destination_param ~name:"src" ~desc:"source contract"
      @@ stop )
      (fun (output_format, output_path_opt, validation)
           (_, contract)
           (cctxt : Protocol_client_context.full) ->
        let should_validate =
          match (validation, output_format) with
          | (Some true, _) | (None, `Text `Full) ->
              true
          | _ ->
              false
        in
        get_metadata_of_contract cctxt ~contract ~strict:should_validate
        >>=? fun content ->
        let none s = cctxt#message "%s" s >>= fun () -> return_unit in
        let maybe_show_validation f =
          let protocol_hash_is_valid s =
            match Tezos_crypto.Protocol_hash.of_b58check_opt s with
            | None ->
                false
            | Some _ ->
                true
          in
          if should_validate then
            f (fun c ->
                return
                  (Metadata_contents.Validation.validate
                     ~protocol_hash_is_valid
                     c))
            >>=? fun errs_and_warns ->
            cctxt#message
              "@[<2>Validation result:@ %a@]"
              Metadata_contents.Validation.pp
              errs_and_warns
            >>= fun () ->
            match fst errs_and_warns with
            | [] ->
                return_unit
            | more ->
                failwith "There were %d validation errors." (List.length more)
          else return_unit
        in
        match output_format with
        | `Text style ->
            Lwt.return (Metadata_contents.of_json content)
            >>=? fun contents ->
            let meta_pp =
              match style with
              | `Full ->
                  Metadata_contents.pp
              | `Short ->
                  Metadata_contents.pp_short
            in
            Output_path.output
              ~none
              output_path_opt
              (Format.asprintf "%a" meta_pp contents)
            >>=? fun () ->
            maybe_show_validation (fun validate -> validate contents)
        | `Raw ->
            Output_path.output ~none output_path_opt content
            >>=? fun () ->
            maybe_show_validation (fun validate ->
                Lwt.return (Metadata_contents.of_json content)
                >>=? fun contents -> validate contents)
        | `Json ->
            Lwt.return (Metadata_contents.of_json content)
            >>=? fun contents ->
            Output_path.output
              ~none
              output_path_opt
              (Metadata_contents.to_json contents)
            >>=? fun () ->
            maybe_show_validation (fun validate -> validate contents));
    command
      ~group
      ~desc:"Get a value from a TZIP-016 %metadata big-map."
      no_options
      ( prefixes ["get"; "metadata"; "element"]
      @@ Clic.string ~name:"key" ~desc:"the string key to look for"
      @@ prefixes ["from"]
      @@ ContractAlias.destination_param ~name:"src" ~desc:"source contract"
      @@ stop )
      (fun () key (_, contract) (cctxt : Protocol_client_context.full) ->
        get_metadata_big_map_value
          cctxt
          ~chain:cctxt#chain
          ~block:cctxt#block
          contract
          ~key
        >>=? fun value -> cctxt#answer "%S" value >>= fun () -> return_unit);
    command
      ~group
      ~desc:"Call an off-chain-view from a TZIP-016-enabled contract."
      (args4
         (Client_proto_args.unparsing_mode_arg ~default:"Readable")
         Output_path.arg
         (switch
            ~doc:"Output JSON instead of concrete Micheline syntax."
            ~long:"json"
            ())
         (default_arg
            ~doc:"Michelson argument to pass to the view."
            ~long:"arg"
            ~placeholder:"MICHELSON"
            ~default:"Unit"
            (parameter (fun _ s ->
                 let open Micheline_parser in
                 Lwt.return (no_parsing_error (tokenize s))
                 >>=? fun tokens ->
                 Lwt.return (no_parsing_error (parse_expression tokens))
                 >>=? fun mich -> return (Micheline.strip_locations mich)))))
      ( prefixes ["query"; "off-chain-view"]
      @@ Clic.string ~name:"name" ~desc:"The view to call."
      @@ prefixes ["from"]
      @@ ContractAlias.destination_param ~name:"contract" ~desc:"KT1 contract"
      @@ stop )
      (fun (unparsing_mode, output_path_opt, json, input_michelson)
           name
           (_, contract)
           (cctxt : Protocol_client_context.full) ->
        get_metadata_of_contract ~strict:false cctxt ~contract
        >>=? fun content ->
        let open Metadata_contents in
        let open View in
        Lwt.return (of_json content)
        >>=? fun metadata ->
        ( match
            List.find_all (fun {name = n; _} -> n = name) metadata.views
          with
        | [one] ->
            return one
        | [] ->
            cctxt#error
              "View not found: %S, available views: %a"
              name
              Fmt.(vbox (one_of ~empty:(any "None") pp))
              metadata.views
        | one :: _more ->
            cctxt#warning
              "Too many views called %S going with %a"
              name
              (pp ~with_code:true)
              one
            >>= fun () -> return one )
        >>=? fun view ->
        Implementation.(
          match
            List.filter_map
              (function Michelson_storage s -> Some s | _ -> None)
              view.implementations
          with
          | [] ->
              cctxt#error
                "View %S does not have any michelson-storage implementation."
                name
          | [one] ->
              return one
          | one :: more ->
              cctxt#warning
                "Using the first michelson-storage implementation of %d"
                (List.length more + 1)
              >>= fun () -> return one)
        >>=? fun first_michelson_implementation ->
        get_storage_and_type_micheline
          cctxt
          ~chain:cctxt#chain
          ~block:cctxt#block
          contract
        >>=? fun (storage_node, type_node, parameter_node) ->
        get_balance cctxt ~chain:cctxt#chain ~block:cctxt#block contract
        >>=? fun contract_balance ->
        let (`Contract the_contract, `Input input_node, `Storage init_storage)
            =
          Micheline_helpers.build_off_chain_view_contract
            first_michelson_implementation
            ~contract_balance:
              (Alpha_context.Tez.to_mutez contract_balance |> Z.of_int64)
            ~contract_address:(Alpha_context.Contract.to_b58check contract)
            ~contract_storage_type:type_node
            ~contract_parameter_type:parameter_node
            ~view_parameters:(Micheline.root input_michelson)
            ~contract_storage:storage_node
        in
        (* We need to print and reparse+type-check the `string Micheline.node`s
           into the protocol's own Michelson in order to use the typed
           RPC `Client_proto_programs.run`: *)
        let program =
          Fmt.str "%a" Micheline_helpers.pp_arbitrary_micheline the_contract
        in
        let input =
          Fmt.str "%a" Micheline_helpers.pp_arbitrary_micheline input_node
        in
        let storage =
          Fmt.str "%a" Micheline_helpers.pp_arbitrary_micheline init_storage
        in
        let parse_expr expr =
          Lwt.return @@ Micheline_parser.no_parsing_error
          @@ Michelson_v1_parser.parse_expression expr
        in
        parse_expr program
        >>=? fun program ->
        parse_expr storage
        >>=? fun storage ->
        parse_expr input
        >>=? fun input ->
        Client_proto_programs.run
          cctxt
          ~chain:cctxt#chain
          ~block:cctxt#block
          ~unparsing_mode
          ~program
          ~storage
          ~input
          ~balance:Alpha_context.Tez.zero
          (* Balance has been fixed by build_off_chain_view_contract *)
          ()
        >>= function
        | Ok (storage_result, _, _) -> (
            let open Michelson_v1_primitives in
            let open Micheline in
            match root storage_result with
            | Prim (_, D_Some, [one], _) ->
                let result = strip_locations one in
                let to_output =
                  match json with
                  | false ->
                      Fmt.str "%a\n" Michelson_v1_printer.print_expr result
                  | true ->
                      let enc =
                        Micheline.canonical_encoding_v1
                          ~variant:"result"
                          Michelson_v1_primitives.prim_encoding
                      in
                      Data_encoding.Json.construct enc result
                      |> Data_encoding.Json.to_string
                           ~newline:true
                           ~minify:false
                in
                Output_path.output
                  ~none:(fun s ->
                    cctxt#message "%s" s >>= fun () -> return_unit)
                  output_path_opt
                  to_output
            | _ ->
                cctxt#error
                  "@[<v>@[%a:@]@,%a@]"
                  Format.pp_print_text
                  "Running the off-chain-view failed in an unexpected way, \
                   please report a bug"
                  Michelson_v1_printer.print_expr
                  storage_result )
        | Error el ->
            cctxt#error "Run failed: %a" Error_monad.pp_print_error el) ]

*)            



type text_length = Full | Short
type output_format = Text of text_length | Json | Raw

let string_of_output_format fmt = 
  match fmt with
  | Text Full -> "text:full"
  | Text Short -> "text:short"
  | Json -> "json"
  | Raw -> "raw"


let wrong_fetch src =
   Contract_metadata.Token.token_fetch ~address:src
(* this is wrong, because it fetches some particular token -- while that might be useful, it's not quite what we need.  We need the contract metadata *)

let validate_input input_value =
    match B58_hashes.check_b58_kt1_hash input_value with
    | _ -> `KT1 input_value
    | exception _ when String.is_prefix input_value ~prefix:"KT" ->
        `Error
          ( input_value
          , [Tezos_error_monad.Error_monad.failure "Invalid KT1 address"] )
    | exception _ -> (
      match Tezos_contract_metadata.Contract_metadata.Uri.validate input_value with
      | Ok uri, _ -> `Uri (input_value, uri)
      | Error e, _ -> `Error (input_value, e) )

module Context = struct
  type t = {
    nodes: Query_nodes.t
  }
  let make ~nodes = {nodes}
end
(*
let fetch src log = 
      let full_input = validate_input src in
      let logs prefix s = print_endline(prefix ^ " " ^ s) in
      let open Lwt in 
      let ctxt = Context.make ~nodes:4 in 
      match full_input with
          | `KT1 address -> (
              Query_nodes.metadata_value ctxt ~address ~key:""
                ~log:(logs "Getting URI g")
              >>= fun metadata_uri ->
              (*  probably don't need 
              Contract_metadata.Uri.Fetcher.set_current_contract ctxt address ;
              log result (Import.Message.text "Now going for: " %% Import.Message.inline_code metadata_uri) ;
              *)
              Lwt.catch
                (fun () ->
                  Contract_metadata.Content.token_metadata_value ctxt ~address
                    ~key:""
                    ~log:(logs "Getting Token Metadata")
                  >>= fun token_metadata -> Lwt.return_some token_metadata )
                (fun exn ->
                  log result
                    ( text "Attempt at getting a %token_metadata big-map failed:"
                    %% Errors_html.exception_html ctxt exn ) ;
                  return_none )
              >>= fun token_metadata_big_map ->
              match Contract_metadata.Uri.validate metadata_uri with
              | Ok uri, _ -> on_uri ctxt uri ?token_metadata_big_map
              | Error error, _ ->
                  raise
                    (mkexn
                       (uri_there_but_wrong ctxt ~uri_string:metadata_uri
                          ~full_input ~error ) ) )
          | `Uri (_, uri) ->
              if Contract_metadata.Uri.needs_context_address uri then
                Async_work.log result
                  (bt "This URI requires a context KT1 address …") ;
              System.slow_step ctxt >>= fun () -> on_uri ctxt uri
          | `Error (_, el) -> raise (mkexn (Tezos_html.error_trace ctxt el)))
*)

let show_metadata src format = 
  print_endline (src ^ string_of_output_format format)

(*
pseudocode, as I understand it: 
1. fetch (how?)

 get_metadata_of_contract cctxt ~contract ~strict:should_validate

2. validate
3. display

*)


(* let rec fetch_uri_contents ~current_contract uri =
  let open Tezos_contract_metadata.Metadata_uri in
  let not_implemented fmt = Error ("Not implemented: " ^ fmt) in
  let open Lwt.Infix in 
  let open Lwt in 
  let get url =
    Cohttp_lwt_unix.Client.get (Uri.of_string url)
    >>= fun (resp, body) ->
    match Cohttp.Response.status resp with
    | `OK ->
        Cohttp_lwt.Body.to_string body >>= fun content -> return content
    | _ ->
        fail_with (Printf.sprintf "Wrong HTTP status: %s" (Cohttp.Response.sexp_of_t resp |> Sexp.to_string_hum))
  in
  match uri with
  | Web url ->
      get url
  | Ipfs {cid; path} ->
      (* https://docs.ipfs.io/how-to/address-ipfs-on-web/#dweb-addressing-in-brief *)
      let url =
        let prefix = Uri.path cctxt#ipfs_gateway in
        Uri.with_path cctxt#ipfs_gateway (Filename.concat prefix (cid ^ path))
        |> Uri.to_string
      in
      Log.lwt_debug "IPFS URI transformed into %s" url >>= fun () -> get url
  | Storage {network = Some n; _} ->
      not_implemented ("Network = " ^ n ^" in storage URI")
  | Storage {network = None; address; key} ->
      ( match address with
      | None ->
          return current_contract
      | Some addr ->
          Lwt.return
            (Environment.wrap_tzresult
               (Alpha_context.Contract.of_b58check addr)) )
      >>=? fun contract ->
      get_metadata_big_map_value
        cctxt
        ~chain:cctxt#chain
        ~block:cctxt#block
        contract
        ~key
  | Hash {kind = `Sha256; value; target} ->
      fetch_uri_contents cctxt ~current_contract target
      >>=? fun content ->
      let real_hash =
        Hacl.Hash.SHA256.digest (Bytes.of_string content) |> Bytes.to_string
      in
      if real_hash <> value then
        cctxt#error
          "SHA256-hash 0x%a@ for@ metadata@ at@ %a@ does@ not@ match@ the@ \
           expected@ value:@ 0x%a"
          Hex.pp
          (Hex.of_string real_hash)
          Metadata_uri.pp
          target
          Hex.pp
          (Hex.of_string value)
      else return content
 *)



(* CLI *)
let metadata_format =
  let doc = "metadata format" in
  let format = Arg.enum ["text", Text Full; "text:full", Text Full; "text:short", Text Short; "raw", Raw; "json", Json] in
  Arg.(value & opt format ~vopt:(Text Full) (Text Full) & 
      info ["format"] ~docv:"FORMAT" ~doc)
  
let src =
    let doc = "source" in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"SRC" ~doc)

(* TODO : validate *)

let show_metadata_t = Term.(const show_metadata $ src $ metadata_format)

let info =
  let doc = "Show TZIP-16 metadata" in
  let man = [
    `S Manpage.s_bugs;
    `P "File bug reports at https://github.com/oxheadalpha/tezos-contract-metadata" ]
  in
  Term.info "tezos-contract-metadata" ~version:"%‌%VERSION%%" ~doc ~exits:Term.default_exits ~man

let () = Term.exit @@ Term.eval (show_metadata_t, info)

