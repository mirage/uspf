(** {1 Sender Policy Framework.}

    SPF is a framework to check the identity of the email's sender. When an
    email passes through an SMTP server, some informations are available such as
    the source of the email, the IP address (because the sender must initiate a
    TCP/IP connexion).

    From these informations and via SPF (and DNS records), we are able to
    {i authorize} the given email or not. Indeed, the email submission process
    requires an identity with the SMTP [MAILFROM] command. At this stage, we are
    able to check if the domain name given by [MAILFROM] and the current IP
    address of the sender match!

    The domain-name used by [MAILFROM] should have some DNS records which
    describe which IP address is allowed to send an email via the [MAILFROM]'s
    identity. SPF will check that and it will try to find a {i match}. In any
    results - if SPF fails or not - the SMTP server will put the result of such
    check into the given email.

    Finally, it permits to check, at any step of the submission, the identity of
    the sender. However, it does not ensure a high level of securities when SPF
    should be use with DKIM/DMARC to ensure some others aspects such as the
    integrity of the given email.

    {2 How to use SPF.}

    SPF requires some {i meta} informations such as the [MAILFROM] identity and
    the IP address of the sender. The user can create a {!ctx} and fill it with
    these information:

    {[
      let ctx =
        Spf.empty |> Spf.with_sender (`MAILFROM path) |> Spf.with_ip ipaddr
    ]}

    From this [ctx], then the user is able to {i check} the identity of the
    sender via a DNS implementation. The user must get the SPF DNS record,
    analyze it and use it then with the [ctx]:

    {[
      let res =
        Spf.get ctx sched dns (module DNS)
        >>= Spf.check ctx sched dns (module DNS)
    ]}

    From the result, the user is able to generate an {i header field}. It
    optional to give your identity (your domain) to be exhaustive about {i meta}
    information on the field value:

    {[ let field_name, value = Spf.to_field ~ctx ?receiver res ]}

    The value is well-formed for the incoming email. You just need to prepend
    the field before the email.

    {2 Reproductibility.}

    The API provides a possibility to extract SPF results from an incoming email
    and regenerate the [ctx] from them. By this way, locally, you can reproduce
    the process above. By this way, you are able to reproduce the written result
    and check if it still is valid.

    Indeed, due to the DNS record requirement to check the identity of the
    sender, it possible that {i meta} informations from the given email are
    obsoletes (for any reasons). *)

module Sigs = Sigs
open Sigs

type ctx

val empty : ctx

val with_sender :
  [ `HELO of [ `raw ] Domain_name.t | `MAILFROM of Colombe.Path.t ] ->
  ctx ->
  ctx

val with_ip : Ipaddr.t -> ctx -> ctx

val domain : ctx -> [ `raw ] Domain_name.t option

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

val a : ?cidr_v4:int -> ?cidr_v6:int -> [ `raw ] Domain_name.t -> mechanism

val all : mechanism

val exists : [ `raw ] Domain_name.t -> mechanism

val inc : [ `raw ] Domain_name.t -> mechanism

val mx : ?cidr_v4:int -> ?cidr_v6:int -> [ `raw ] Domain_name.t -> mechanism

val v4 : Ipaddr.V4.Prefix.t -> mechanism

val v6 : Ipaddr.V6.Prefix.t -> mechanism

type modifier

type quantifier = Pass | Fail | Softfail | Neutral

val pass : mechanism -> quantifier * mechanism

val fail : mechanism -> quantifier * mechanism

val softfail : mechanism -> quantifier * mechanism

val neutral : mechanism -> quantifier * mechanism

val record : (quantifier * mechanism) list -> modifier list -> record

val record_to_string : record -> string

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

val get :
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

type newline = LF | CRLF

val to_field :
  ctx:ctx -> ?receiver:Emile.domain -> res -> Mrmime.Field_name.t * Unstrctrd.t

type extracted = spf list

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

(** / *)

val field_received_spf : Mrmime.Field_name.t

val parse_received_spf_field_value :
  Unstrctrd.t -> (spf, [> `Msg of string ]) result
