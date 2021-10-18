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
open! Import
open Tezos_micheline

let micheline_of_ezjsonm json =
  let enc =
    Micheline.canonical_encoding ~variant:"custom" Data_encoding.string in
  let mich = Data_encoding.Json.destruct enc json in
  Micheline.root mich

let micheline_of_json s =
  let json =
    match Ezjsonm.value_from_string s with
    | `O (("code", code) :: _) -> code
    | other -> other in
  micheline_of_ezjsonm json

let micheline_to_ezjsonm mich =
  let enc =
    Micheline.canonical_encoding ~variant:"custom" Data_encoding.string in
  let json = Data_encoding.Json.construct enc (Micheline.strip_locations mich) in
  json

let parse_micheline ~check_indentation ~check_primitives m =
  let rec primitive_check =
    let open Tezos_micheline.Micheline in
    function
    | Prim (_, s, args, _) ->
        ( match
            List.find Michelson_bytes.primitives ~f:(fun (p, _) ->
                String.equal p s )
          with
        | Some _ -> ()
        | None -> Fmt.failwith "Unknown primitive: %S" s ) ;
        List.iter args ~f:primitive_check
    | _ -> () in
  match Micheline_parser.tokenize m with
  | tokens, [] -> (
    match Micheline_parser.parse_expression ~check:check_indentation tokens with
    | node, [] -> (
      try
        if check_primitives then primitive_check node ;
        Ok node
      with e -> Error [Tezos_error_monad.Error_monad.Exn e] )
    | _, errs -> Error errs )
  | _, errs -> Error errs

let parse_micheline_exn ~check_indentation ~check_primitives m =
  match parse_micheline ~check_indentation ~check_primitives m with
  | Ok o -> o
  | Error e ->
      Fmt.failwith "parse_micheline: %a"
        Tezos_error_monad.Error_monad.pp_print_error e

let micheline_canonical_to_string c =
  Fmt.str "%a" Micheline_printer.print_expr
    (Micheline_printer.printable Base.Fn.id c)

let micheline_node_to_string node =
  micheline_canonical_to_string (Micheline.strip_locations node)

module Partial_type = struct
  module Structure = struct
    type type_kind =
      | Any
      | Nat
      | Mutez
      | Bytes
      | Address
      | Bool
      | String
      | List of type_kind
      | Map of type_kind * type_kind

    type leaf = string

    type t =
      | Leaf of
          { raw: string Tezos_micheline.Micheline.canonical
          ; kind: type_kind
          ; v: leaf
          ; description: (string * string) option }
      | Pair of {left: t; right: t}
  end

  type t =
    { original: string Tezos_micheline.Micheline.canonical
    ; structure: Structure.t }

  open Structure
  open Metadata_contents.Michelson_blob

  let of_type ?(annotations = []) (Micheline m) =
    let view_annots = annotations in
    let open Tezos_micheline.Micheline in
    (* find the annotation with a given key, if any *)
    let describe annot =
      List.find view_annots ~f:(fun (k, _) ->
          List.mem annot k ~equal:String.equal ) in
    let rec go type_ =
      let raw = strip_locations type_ in
      let create_leaf ?annot kind =
        let description = Option.bind ~f:describe annot in
        Leaf {raw; kind; v= ""; description} in
      match type_ with
      | Prim (_, "nat", [], annot) -> create_leaf Nat ~annot
      | Prim (_, "mutez", [], annot) -> create_leaf Mutez ~annot
      | Prim (_, "bytes", [], annot) -> create_leaf Bytes ~annot
      | Prim (_, "string", [], annot) -> create_leaf String ~annot
      | Prim (_, "address", [], annot) -> create_leaf Address ~annot
      | Prim (_, "bool", [], annot) -> create_leaf Bool ~annot
      | Prim (_, "pair", [l; r], _) -> Pair {left= go l; right= go r}
      | Prim (_, "list", [Prim (_, "nat", [], _)], annot) ->
          create_leaf (List Nat) ~annot
      | Prim
          ( _
          , "map"
          , [Prim (_, "string", [], _); Prim (_, "bytes", [], _)]
          , annot ) ->
          create_leaf (Map (String, Bytes)) ~annot
      | Prim (_, _, _, annot) -> create_leaf Any ~annot
      | _ -> create_leaf Any in
    {original= m; structure= go (root m)}

  let validate_micheline m =
    match parse_micheline ~check_indentation:false ~check_primitives:true m with
    | Ok _ -> true
    | Error _ -> false

  let rec validate_structure = function
    | Leaf {kind= Nat | Mutez; v; _} -> (
      match v with
      | "" -> false
      | s -> ( match Z.of_string s with _ -> true | exception _ -> false ) )
    | Leaf {kind= Bytes; v; _} -> (
      match v with
      | "" -> false
      | s -> (
        match String.chop_prefix (String.strip s) ~prefix:"0x" with
        | None -> false
        | Some s -> (
          match Hex.to_string (`Hex s) with _ -> true | exception _ -> false ) )
      )
    | Leaf lf -> validate_micheline lf.v
    | Pair {left; right} -> validate_structure left && validate_structure right

  let is_valid pt = validate_structure pt.structure

  let bytes_guesses input =
    try
      let raw, default_value =
        match input with
        | `Zero_x bytes ->
            let hex = String.chop_prefix_exn bytes ~prefix:"0x" in
            (Hex.to_string (`Hex hex), `Just_hex hex)
        | `Raw_string s ->
            let (`Hex hex) = Hex.of_string s in
            (s, `Just_hex hex) in
      let json () = `Json (Ezjsonm.value_from_string raw) in
      let utf8 () =
        let maxperline =
          let nl = Uchar.of_char '\n' in
          let folder (count, max_per_line) _ = function
            | `Uchar n when Uchar.equal n nl -> (0, max count max_per_line)
            | `Uchar _ -> (count + 1, max_per_line)
            | `Malformed _ -> Fmt.failwith "nop" in
          let c, m = Uutf.String.fold_utf_8 folder (0, 0) raw in
          max c m in
        let lines = match raw with "" -> [] | _ -> String.split ~on:'\n' raw in
        `Valid_utf_8 (maxperline, lines) in
      let bool () = `Bool (Bool.of_string raw) in
      let number () = `Number (Float.of_string raw) in
      let one_line_not_weird () =
        String.for_all raw ~f:(function '\n' | '\t' -> false | _ -> true) in
      let any_prefix l =
        List.exists l ~f:(fun prefix -> String.is_prefix raw ~prefix) in
      let web_uri () =
        if any_prefix ["https://"; "http://"; "ftp://"] && one_line_not_weird ()
        then `Web_uri raw
        else failwith "not web uri :)" in
      let tzip16_uri () =
        if
          any_prefix ["tezos-storage://"; "ipfs://"; "sha256://"]
          && one_line_not_weird ()
        then `Tzip16_uri raw
        else failwith "not tzip16 uri :)" in
      match
        List.find_map [bool; number; web_uri; tzip16_uri; json; utf8]
          ~f:(fun f -> try Some (f ()) with _ -> None)
      with
      | Some s -> s
      | None -> default_value
    with _ -> `Dont_know

  let micheline_string_bytes_map_exn node =
    let open Tezos_micheline.Micheline in
    let nope = Decorate_error.raise in
    match node with
    | Seq (l, map) -> (
      match map with
      | [] -> []
      | Prim (_, "Elt", [String (_, s); Bytes (_, b)], _) :: more ->
          List.fold more
            ~init:[(s, Bytes.to_string b)]
            ~f:
              (fun prev -> function
                | Prim (_, "Elt", [String (_, s); Bytes (_, b)], _) ->
                    (s, Bytes.to_string b) :: prev
                | other ->
                    nope
                      Message.(
                        text "Michelson-map element has wrong structure:"
                        %% inline_code (micheline_node_to_string other)) )
      | other ->
          nope
            Message.(
              text "Metadata result has wrong structure:"
              %% inline_code (micheline_node_to_string (Seq (l, other)))) )
    | other ->
        nope
          Message.(
            text "Expecting Michelson-map but got"
            %% inline_code (micheline_node_to_string other))
end
