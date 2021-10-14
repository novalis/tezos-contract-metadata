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

module Node_status : sig
  type t = Uninitialized | Non_responsive of exn | Ready of string
end

module Rpc_cache : sig
  module Hashtbl = Caml.Hashtbl

  type t = (string, float * string) Hashtbl.t
end

module Node : sig
  type t =
    { name: string
    ; prefix: string
    ; status: (float * Node_status.t) Reactive.var
    ; rpc_cache: Rpc_cache.t
    ; network: Network.t
    ; info_url: string option }

  module Contract : sig
    type t =
      { storage_node: (int, string) Tezos_micheline.Micheline.node
      ; type_node: (int, string) Tezos_micheline.Micheline.node
      ; metadata_big_map: Z.t }

    val make :
         storage_node:(int, string) Tezos_micheline.Micheline.node
      -> type_node:(int, string) Tezos_micheline.Micheline.node
      -> metadata_big_map:Z.t
      -> t
  end

  val micheline_value_of_big_map_at_nat :
       'a
    -> t
    -> big_map_id:Z.t
    -> key:int
    -> log:(string -> unit)
    -> (int, string) Tezos_micheline.Micheline.node Lwt.t

  val metadata_big_map :
       < system: System.t ; .. >
    -> t
    -> address:string
    -> log:(string -> unit)
    -> Contract.t Lwt.t
end

module Node_list : sig
  type t = (string, Node.t * bool) List.Assoc.t
end

type t =
  { nodes: Node_list.t Reactive.var
  ; wake_up_call: unit Lwt_condition.t
  ; loop_started: bool Reactive.var
  ; loop_interval: float Reactive.var
  ; loop_status: [`Not_started | `In_progress | `Sleeping] Reactive.var }

val metadata_value :
     < nodes: t ; system: System.t ; .. >
  -> address:string
  -> key:string
  -> log:(string -> unit)
  -> string Lwt.t

val find_node_with_contract :
  < nodes: t ; system: System.t ; .. > -> string -> Node.t Lwt.t

val call_off_chain_view :
     < nodes: t ; system: System.t ; .. >
  -> log:(string -> unit)
  -> address:string
  -> view:Metadata_contents.View.Implementation.Michelson_storage.t
  -> parameter:('a, string) Tezos_micheline.Micheline.node
  -> ( (int, string) Tezos_micheline.Micheline.node
       * (int, string) Tezos_micheline.Micheline.node
     , 'b )
     result
     Lwt.t
