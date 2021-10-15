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

open! Base
open Tezos_contract_metadata
open Tezos_contract_metadata.Import
open Cmdliner
open Stdio

type text_length = Full | Short
type output_format = Text of text_length | Json | Raw

let string_of_output_format fmt =
  match fmt with
  | Text Full -> "text:full"
  | Text Short -> "text:short"
  | Json -> "json"
  | Raw -> "raw"

let validate_address input_value =
  match B58_hashes.check_b58_kt1_hash input_value with
  | _ -> `KT1 input_value
  | exception _ when String.is_prefix input_value ~prefix:"KT" ->
      `Error
        ( input_value
        , [Tezos_error_monad.Error_monad.failure "Invalid KT1 address"] )
  | exception _ -> (
    match
      Tezos_contract_metadata.Contract_metadata.Uri.validate input_value
    with
    | Ok uri, _ -> `Uri (input_value, uri)
    | Error e, _ -> `Error (input_value, e) )

let on_uri ctxt ?token_metadata_big_map uri ~address =
  (* fixme Not sure what to do with token_metadata_big_map parameter -- it's just a (maybe big) number? *)
  let _ = token_metadata_big_map in
  let open Lwt in
  catch
    (fun () ->
      Contract_metadata.Uri.fetch ctxt uri ~prefix:"Fetching Metadata "
        ~current_contract:address )
    (fun e -> raise (Exn.reraise e "Failed to fetch metadata"))
  >>= fun json_code ->
  dbgf ctxt#formatter "before of-json" ;
  match Contract_metadata.Content.of_json json_code with
  | Ok (warnings, contents) ->
      (*
                Async_work.ok result
                  (uri_and_metadata_result ctxt ~full_input ~uri
                     ?token_metadata_big_map ~metadata:json_code ) ;
                Lwt.return ()
                *)
      return (Some (warnings, contents))
  | Error _ ->
      (*
                raise
                  (mkexn
                     (uri_ok_but_metadata_failure ctxt ~uri ~full_input
                        ~metadata_json:json_code ~error ) )*)
      (*return None *)
      fail_with "this error"

let fetch_contract_metadata ctxt log_exn src =
  let log = dbgf ctxt#formatter "%s" in
  let full_input = validate_address src in
  let logs prefix s = log (prefix ^ " " ^ s) in
  let open Lwt in
  match full_input with
  | `KT1 address -> (
      Query_nodes.metadata_value ctxt ~address ~key:""
        ~log:(logs "Getting URI g")
      >>= fun metadata_uri ->
      Lwt.catch
        (fun () ->
          Contract_metadata.Content.token_metadata_value ctxt ~address
            ~log:(logs "Getting Token Metadata")
          >>= fun token_metadata -> Lwt.return_some token_metadata )
        (fun exn ->
          log_exn "Attempt at getting a %token_metadata big-map failed (main):"
            exn ;
          return_none )
      >>= fun token_metadata_big_map ->
      match Contract_metadata.Uri.validate metadata_uri with
      | Ok uri, _ ->
          on_uri ctxt uri ?token_metadata_big_map ~address:(Some address)
      | Error _, _ -> fail_with "FIXME wrong uri "
      (* fixme
                    (mkexn
                       (uri_there_but_wrong ctxt ~uri_string:metadata_uri
                          ~full_input ~error ) ) )
      *) )
  | `Uri (_, uri) ->
      if Contract_metadata.Uri.needs_context_address uri then
        log "This URI requires a context KT1 address …" ;
      on_uri ctxt uri ~address:None
  | `Error (_, _) -> fail_with "wrong type?"
(* fixme raise (mkexn (Tezos_html.error_trace ctxt el))*)

let show_metadata src format debug =
  let ctxt =
    let system = System.create () in
    let nodes = Query_nodes.get_default_nodes () in
    let fetcher = Contract_metadata.Uri.Fetcher.create () in
    object
      method system = system
      method nodes = nodes
      method fetcher = fetcher
      method formatter = if debug then Fmt.stderr else Caml.Format.str_formatter
    end in
  let log_exn prefix exn =
    dbgf ctxt#formatter "%s: %s" prefix (Exn.to_string exn) in
  let open Lwt.Infix in
  Lwt_main.run
    ( fetch_contract_metadata ctxt log_exn src
    >>= fun result ->
    ( match result with
    | None -> print_endline "wrong"
    | Some (_, contents) -> (
      match format with
      | Text Full -> Metadata_contents.pp Caml.Format.std_formatter contents
      | Text Short ->
          Metadata_contents.pp_short Caml.Format.std_formatter contents
      | Raw -> () (* fixme *)
      | Json -> () (* fixme *) ) ) ;
    Lwt.return 0 )

(* CLI *)
let metadata_format =
  let doc = "metadata format" in
  let format =
    Arg.enum
      [ ("text", Text Full)
      ; ("text:full", Text Full)
      ; ("text:short", Text Short)
      ; ("raw", Raw)
      ; ("json", Json) ] in
  Arg.(
    value
    & opt format ~vopt:(Text Full) (Text Full)
    & info ["format"] ~docv:"FORMAT" ~doc)

let debug =
  let doc = "Debugging output to stderr." in
  let yes = (true, Arg.info ["debug"] ~doc) in
  Arg.(last & vflag_all [false] [yes])

let src =
  let doc = "source" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"SRC" ~doc)

let show_metadata_t = Term.(const show_metadata $ src $ metadata_format $ debug)

let info =
  let doc = "Show TZIP-16 metadata" in
  let man =
    [ `S Manpage.s_bugs
    ; `P
        "File bug reports at \
         https://github.com/oxheadalpha/tezos-contract-metadata" ] in
  Term.info "tezos-contract-metadata" ~version:"%‌%VERSION%%" ~doc
    ~exits:Term.default_exits ~man

let () = Term.exit @@ Term.eval (show_metadata_t, info)
