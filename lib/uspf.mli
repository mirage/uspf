(** {1 (Un)Sender Policy Framework.}

    uSPF is a framework to check the identity of the email's sender. When an
    email passes through an SMTP server, some informations are available such as
    the source of the email, the IP address (because the sender must initiate a
    TCP/IP connexion).

    From these informations and via uSPF (and DNS records), we are able to
    {i authorize} the given email or not. Indeed, the email submission process
    requires an identity with the SMTP [MAILFROM] command. At this stage, we are
    able to check if the domain name given by [MAILFROM] and the current IP
    address of the sender match!

    The domain-name used by [MAILFROM] should have some DNS records which
    describe which IP address is allowed to send an email via the [MAILFROM]'s
    identity. uSPF will check that and it will try to find a {i match}. In any
    results - if uSPF fails or not - the SMTP server will put the result of such
    check into the given email.

    Finally, it permits to check, at any step of the submission, the identity of
    the sender. However, it does not ensure a high level of securities when uSPF
    should be use with DKIM/DMARC to ensure some others aspects such as the
    integrity of the given email.

    {2 How to use uSPF.}

    uSPF requires some {i meta} informations such as the [MAILFROM] identity and
    the IP address of the sender. The user can create a {!type:ctx} and fill it
    with these information:

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
    obsoletes (for any reasons).

    {2 As a server.}

    uSPF allows the end-user to craft its own record and publish it then into
    its primary/secondary DNS server. Multiple values exists such as:

    - {!val:a}
    - {!val:mx}
    - {!val:all}
    - {!val:pass}
    - or {!val:fail}

    They permits to describe {i via} OCaml the SPF record. It can be serialized
    to a simple [string] then and the user can save it into its own
    primary/secondary DNS server. *)

module Sigs = Sigs
open Sigs

type ctx
(** The type for contexts. It's a {i heterogeneous map} of values to help uSPF
    to validate the sender. It requires the [MAILFROM] parameter given by the
    SMTP protocol (which can be filled {i via} {!val:with_sender}) and/or the IP
    address of the sender (which can be filled {i via} {!val:with_ip}). *)

val empty : ctx
(** [empty] is an empty context. *)

val with_sender :
  [ `HELO of [ `raw ] Domain_name.t | `MAILFROM of Colombe.Path.t ] ->
  ctx ->
  ctx
(** [with_sender v ctx] adds into the given [ctx] the sender of the incoming
    email (its simple domain name or the complete email address). *)

val with_ip : Ipaddr.t -> ctx -> ctx
(** [with_ip v ctx] adds into the given [ctx] the IP address of the sender. *)

val domain : ctx -> [ `raw ] Domain_name.t option
(** [domain ctx] returns the domain-name of the sender if it exists. *)

module Macro : sig
  type macro =
    [ `Literal of string
    | `Macro of char * (int option * bool) * string
    | `Macro_encoded_space
    | `Macro_space
    | `Macro_percent ]

  type t = macro list * string option

  val to_string : t -> string

  val pp : t Fmt.t

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

  val to_string : t -> string

  val pp : t Fmt.t

  val equal : t -> t -> bool

  val parse_record : string -> (t, [> `Msg of string ]) result
end

type record
(** The type of SPF records. *)

type mechanism
(** The type of mechanisms.

    A mechanism permits to design and identify a set of IP addresses as being
    permitted or not permitted to use the {!val:domain} for sending mail. *)

val a : ?cidr_v4:int -> ?cidr_v6:int -> [ `raw ] Domain_name.t -> mechanism
(** This mechanism matches if the sender's IP address is one of the
    domain-name's IP addresses. For clarity, this means the [a] mechanism also
    matches [AAAA] records.

    An address lookup is done on the domain-name using the type of lookup (A or
    AAAA) appropriate for the connection type. The IP is compared to the
    returned address(es). If any address matches, the mechanism matches.

    A {i Classless Inter-Domain Routing} can be applied to returned address(es)
    (IPv4 or IPv6) to compare with the sender's IP address. For instance,
    [a=10.0.0.42/32] matches only [10.0.0.42] as the sender's IP address but
    [a=10.0.0.42/24] matches any [10.0.0.*] addresses. By default,
    [cidr_v4 = 32] and [cidr_v6 = 128]. *)

val all : mechanism
(** The [all] mechanism is a test that always matches. It is used as the
    rightmost mechanism (the last mechanism) in a record to provide an explicit
    default. For example [v=spf1 a mx -all].

    Mechanisms after [all] will never be tested. *)

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

val record_of_string : ctx:ctx -> string -> (record, [> `Msg of string ]) result

val record_equal : record -> record -> bool

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

val pp_spf : spf Fmt.t

val extract_received_spf :
  ?newline:newline ->
  'flow ->
  't state ->
  (module Sigs.FLOW with type flow = 'flow and type backend = 't) ->
  ((extracted, [> `Msg of string ]) result, 't) io

(** / *)

val select_spf1 : string list -> (string, [> `None ]) result

val field_received_spf : Mrmime.Field_name.t

val parse_received_spf_field_value :
  Unstrctrd.t -> (spf, [> `Msg of string ]) result
