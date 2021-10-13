open Import

module Uri :sig
  module Fetcher :sig
    type gateway = {main: string; alternate: string}
    type t = {current_contract: string option Reactive.var; gateway: gateway}
  end
end


module Content :sig
    module Tzip_021 :sig
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
end

module Token :sig
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

val token_fetch : < fetcher : Uri.Fetcher.t; nodes : Query_nodes.t; system : System.t; .. >  -> address:string -> id:int -> log:(Message.t -> unit) -> t Lwt.t
(** Fetch metadata about a specific token *)
end