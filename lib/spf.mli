[@@@warning "-30"]

module Sigs = Sigs
open Sigs

type ctx

val empty : ctx

val with_sender :
  [ `HELO of [ `raw ] Domain_name.t | `MAILFROM of Colombe.Path.t ] ->
  ctx ->
  ctx

val with_ip : Ipaddr.t -> ctx -> ctx

module Macro : sig
  type macro =
    [ `Literal of string
    | `Macro of char * (int option * bool) * string
    | `Macro_encoded_space
    | `Macro_space
    | `Macro_percent ]

  type t = macro list * string option

  val expand_string :
    ctx -> string -> ([ `raw ] Domain_name.t, [> `Msg of string ]) result
end

module Term : sig
  type t =
    [ `Directive of
      char option
      * [ `A of Macro.t option * (int option * int option)
        | `All
        | `Exists of Macro.t
        | `Include of Macro.t
        | `Mx of Macro.t option * (int option * int option)
        | `Ptr of Macro.t option
        | `V4 of Ipaddr.V4.Prefix.t
        | `V6 of Ipaddr.V6.Prefix.t ]
    | `Explanation of Macro.t
    | `Redirect of Macro.t
    | `Unknown of string * Macro.macro list ]
    list

  val parse_record : string -> (t, [> `Msg of string ]) result
end

type record

type mechanism

type res =
  [ `None
  | `Neutral
  | `Pass of mechanism
  | `Fail
  | `Softfail
  | `Temperror
  | `Permerror ]

val pp : record Fmt.t

val pp_res : res Fmt.t

val record :
  ctx:ctx ->
  't state ->
  'dns ->
  (module DNS with type t = 'dns and type backend = 't) ->
  (([ res | `Record of record ], [> `Msg of string ]) result, 't) io

val check :
  ctx:ctx ->
  't state ->
  'dns ->
  (module DNS with type t = 'dns and type backend = 't) ->
  [ res | `Record of record ] ->
  (res, 't) io

val to_field :
  ctx:ctx -> ?receiver:Emile.domain -> res -> Mrmime.Field_name.t * Unstrctrd.t

type newline = LF | CRLF

type extracted = { sender : Emile.mailbox option; received_spf : spf list }

and spf = {
  result :
    [ `None | `Neutral | `Pass | `Fail | `Softfail | `Temperror | `Permerror ];
  receiver : Emile.domain option;
  sender : Emile.mailbox option;
  ip : Ipaddr.t option;
  ctx : ctx;
}

val extract_received_spf :
  ?newline:newline ->
  'flow ->
  't state ->
  (module Sigs.FLOW with type flow = 'flow and type backend = 't) ->
  ((extracted, [> `Msg of string ]) result, 't) io
