open! Import
open Tezos_micheline

let micheline_of_ezjsonm json =
  let enc =
    Micheline.canonical_encoding ~variant:"custom" Data_encoding.string in
  let mich = Data_encoding.Json.destruct enc json in
  Micheline.root mich

let micheline_of_json s =
  dbgf "micheline_of_json : %d bytes" (String.length s) ;
  let json =
    match Ezjsonm.value_from_string s with
    | `O (("code", code) :: _) -> code
    | other -> other in
  dbgf "micheline_of_json: done parsing" ;
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

    type leaf = string Reactive.var

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
    let describe annot =
      List.find view_annots ~f:(fun (k, _) ->
          List.mem annot k ~equal:String.equal ) in
    let rec go tp =
      let raw = strip_locations tp in
      let leaf ?annot kind =
        let description = Option.bind ~f:describe annot in
        Leaf {raw; kind; v= Reactive.var ""; description} in
      match tp with
      | Prim (_, "nat", [], annot) -> leaf Nat ~annot
      | Prim (_, "mutez", [], annot) -> leaf Mutez ~annot
      | Prim (_, "bytes", [], annot) -> leaf Bytes ~annot
      | Prim (_, "string", [], annot) -> leaf String ~annot
      | Prim (_, "address", [], annot) -> leaf Address ~annot
      | Prim (_, "bool", [], annot) -> leaf Bool ~annot
      | Prim (_, "pair", [l; r], _) -> Pair {left= go l; right= go r}
      | Prim (_, "list", [Prim (_, "nat", [], _)], annot) ->
          leaf (List Nat) ~annot
      | Prim
          ( _
          , "map"
          , [Prim (_, "string", [], _); Prim (_, "bytes", [], _)]
          , annot ) ->
          leaf (Map (String, Bytes)) ~annot
      | Prim (_, _, _, annot) -> leaf Any ~annot
      | _ -> leaf Any in
    {original= m; structure= go (root m)}

  let rec fill_structure_with_value mf node =
    let open Tezos_micheline.Micheline in
    let mich_node = micheline_node_to_string in
    match (mf, node) with
    | Leaf leaf, nn -> Reactive.set leaf.v (mich_node nn)
    | Pair {left; right}, Prim (_, "Pair", [l; r], _) ->
        fill_structure_with_value left l ;
        fill_structure_with_value right r
    | Pair _, other ->
        Decorate_error.(
          raise
            Message.(
              t "Type mismatch" %% inline_code (mich_node other) %% t "is not a pair."))

  let fill_with_value mf node = fill_structure_with_value mf.structure node

  let peek m =
    let rec pk = function
      | Leaf l -> Reactive.peek l.v
      | Pair {left; right} -> Fmt.str "(Pair %s %s)" (pk left) (pk right) in
    pk m.structure

  let validate_micheline m =
    match parse_micheline ~check_indentation:false ~check_primitives:true m with
    | Ok _ -> true
    | Error _ -> false

  let rec validate_structure = function
    | Leaf {kind= Nat | Mutez; v; _} ->
        Reactive.(
          get v
          |> map ~f:(function
               | "" -> false
               | s -> (
                 match Z.of_string s with _ -> true | exception _ -> false ) ))
    | Leaf {kind= Bytes; v; _} ->
        Reactive.(
          get v
          |> map ~f:(function
               | "" -> false
               | s -> (
                 match String.chop_prefix (String.strip s) ~prefix:"0x" with
                 | None -> false
                 | Some s -> (
                   match Hex.to_string (`Hex s) with
                   | _ -> true
                   | exception _ -> false ) ) ))
    | Leaf lf -> Reactive.(get lf.v |> map ~f:validate_micheline)
    | Pair {left; right} ->
        Reactive.(
          map2 ~f:( && ) (validate_structure left) (validate_structure right))

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
                        t "Michelson-map element has wrong structure:"
                        %% inline_code (micheline_node_to_string other)) )
      | other ->
          nope
            Message.(
              t "Metadata result has wrong structure:"
              %% inline_code (micheline_node_to_string (Seq (l, other)))) )
    | other ->
        nope
          Message.(
            t "Expecting Michelson-map but got"
            %% inline_code (micheline_node_to_string other)) 

end