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
open Import

module Uri : sig
  module Fetcher : sig
    type gateway = {main: string; alternate: string}
    type t = {gateway: gateway}

    val create : unit -> t
  end

  val fetch :
       ?limit_bytes:int
    -> ?prefix:string
    -> < fetcher: Fetcher.t
       ; nodes: Query_nodes.Node_list.t
       ; system: System.t
       ; formatter: Caml.Format.formatter
       ; .. >
    -> Metadata_uri.t
    -> current_contract:string option
    -> string Lwt.t

  val validate :
       string
    -> (Metadata_uri.t, Tezos_error_monad.TzCore.error list) result
       * ([> `Address | `Network] * string * string) list

  val needs_context_address : Metadata_uri.t -> bool
end

module Content : sig
  module Tzip_021 : sig
    type uri_format =
      { uri: string option
      ; mime_type: string option
      ; other: (string * Ezjsonm.value) list }

    type t =
      { description: string option
      ; creators: string list option
      ; tags: string list option
      ; transferable: bool option
      ; boolean_amount: bool option
      ; prefers_symbol: bool option
      ; thumbnail: string option
      ; display: string option
      ; artifact: string option
      ; formats: uri_format list option
      ; warnings: Message.t list }
  end

  val token_metadata_value :
       < nodes: Query_nodes.Node_list.t
       ; system: System.t
       ; formatter: Caml.Format.formatter
       ; .. >
    -> address:string
    -> log:(string -> unit)
    -> Z.t Lwt.t
  (** Return metadata about a token FIXME this docstring doesn't make sense *)

  val of_json :
       string
    -> ( [`Fixed_legacy of string * string] list * Metadata_contents.t
       , Tezos_error_monad.Error_monad.tztrace )
       result
end

module Token : sig
  type warning =
    [ `Fetching_uri of string * exn
    | `Parsing_uri of
      string
      * Tezos_error_monad.Error_monad.error
        Tezos_error_monad.Error_monad.TzTrace.trace
    | `Getting_metadata_field of Message.t ]

  type t =
    { address: string
    ; id: int
    ; network: Network.t option
    ; symbol: string option
    ; name: string option
    ; decimals: string option
    ; total_supply: Z.t option
    ; tzip21: Content.Tzip_021.t
    ; main_multimedia: (string, exn) Result.t Option.t
    ; metadata: Metadata_contents.t
    ; special_knowledge: [`Hic_et_nunc of int] list
    ; warnings: (string * warning) list }

  val token_fetch :
       < fetcher: Uri.Fetcher.t
       ; nodes: Query_nodes.Node_list.t
       ; system: System.t
       ; formatter: Caml.Format.formatter
       ; .. >
    -> address:string
    -> id:int
    -> log:(Message.t -> unit)
    -> t Lwt.t
  (** Fetch metadata about a specific token *)
end
