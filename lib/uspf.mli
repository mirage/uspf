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
        Uspf.empty |> Uspf.with_sender (`MAILFROM path) |> Uspf.with_ip ipaddr
    ]}

    From this [ctx], then the user is able to {i check} the identity of the
    sender via a DNS implementation. The user must get the SPF DNS record,
    analyze it and use it then with the [ctx]:

    {[
      let res =
        Uspf.get ctx sched dns (module DNS)
        >>= Uspf.check ctx sched dns (module DNS)
    ]}

    From the result, the user is able to generate an {i header field}. It
    optional to give your identity (your domain) to be exhaustive about {i meta}
    information on the field value:

    {[
      let field_name, value = Uspf.to_field ~ctx ?receiver res
    ]}

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

type ctx
(** The type for contexts. It's a {i heterogeneous map} of values to help uSPF
    to validate the sender. It requires the [MAILFROM] parameter given by the
    SMTP protocol (which can be filled {i via} {!val:with_sender}) and/or the IP
    address of the sender (which can be filled {i via} {!val:with_ip}). *)

val empty : ctx
(** [empty] is an empty context. *)

val with_sender :
     [ `HELO of [ `raw ] Domain_name.t | `MAILFROM of Colombe.Path.t ]
  -> ctx
  -> ctx
(** [with_sender v ctx] adds into the given [ctx] the sender of the incoming
    email (its simple domain name or the complete email address). *)

val with_ip : Ipaddr.t -> ctx -> ctx
(** [with_ip v ctx] adds into the given [ctx] the IP address of the sender. *)

val domain : ctx -> [ `raw ] Domain_name.t option
(** [domain ctx] returns the domain-name of the sender if it exists. *)

val origin : ctx -> [ `HELO | `MAILFROM ] option

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
(** This mechanism is used to construct an arbitrary domain name that is used
    for a DNS A record query. It allows for complicated schemes involving
    arbitrary parts on the mail envelope to determine what is permitted. *)

val inc : [ `raw ] Domain_name.t -> mechanism
(** The [include] mechanism triggers a {i recursive} evaluation of {!val:check}:

    + The macro is expanded according to the given {!val:ctx}
    + We re-execute {!val:check} with the produced domain-name (IP and sender
      arguments remain the same)
    + The recursive evaluation returns {i match}, {i not-match} or an error.
    + If it returns {i match}, then the appropriate result for the [include]
      mechanism is used (see {!type:qualifier})
    + It it returns {i not-match} or an error, the {!val:check} process tests
      the next mechanism.

    {b Note}: for instance, if the domain-name has [-all], [include] does not
    strictly terminates the processus. It fails and let {!val:check} to process
    the next mechanism. *)

val mx : ?cidr_v4:int -> ?cidr_v6:int -> [ `raw ] Domain_name.t -> mechanism
(** This mechanims matches if the sender's IP is one of the MX hosts for a
    domain-name. A domain-name should have a MX record which is an IP address.
    If this IP address is the same as the given IP address into the given
    {!type:ctx}, we consider that the sender {i matches}.

    {b Note}: if the domain-name has no MX record, {!val:check} does not apply
    the implicit MX rules by querying for an A or AAAA record for the same name.
*)

val v4 : Ipaddr.V4.Prefix.t -> mechanism
(** This mechanism test whether the given IP from the given {!type:ctx} is
    contained within a given IPv4 network. *)

val v6 : Ipaddr.V6.Prefix.t -> mechanism
(** This mechanism test whether the given IP from the given {!type:ctx} is
    contained within a given IPv: network. *)

type modifier
(** The type of modifiers.

    They are not available because they mostly provide additional information
    which are not needed for {!val:check}. By this way, it's not needed to let
    the user to define some when they are not effective on the user's sender
    policy. *)

type qualifier =
  | Pass
  | Fail
  | Softfail
  | Neutral
      (** The type of qualifiers.

          A qualifier specifies what the mechanism returns when it matches or
          not:

          - [+] returns [pass] if the mechanism matches
          - [-] returns [fail] if the mechanism matches
          - [~] returns [softfail] if the mechanism matches
          - [?] returns [neutral] if the mechanism matches *)

val pass : mechanism -> qualifier * mechanism
(** [pass m] specifies the {!type:qualifier} of the given mechanism [m]. If the
    mechanism {i matches} from the given {!type:ctx}, {!val:check} returns
    [`Pass]. Otherwise, {!val:check} tries the next mechanism. *)

val fail : mechanism -> qualifier * mechanism
(** [fail m] specifies the {!type:qualifier} of the given mechanism [m]. If the
    mechanism {i matches} from the given {!type:ctx}, {!val:check} tries the
    next mechanism (as it considers the current one as a failure). If the
    mechanism is the last one, {!val:check} returns [`Fail] so. *)

val softfail : mechanism -> qualifier * mechanism
(** [softfail m] specifies the {!type:qualifier} of the given mechanism [m]. If
    the mechanism {i matches} from the given {!type:ctx}, {!val:check} tries the
    next mechanism (as it considers the current one as a {i soft} failure). If
    the mechanism is the last one, {!val:check} returns [`Softfail] so. *)

val neutral : mechanism -> qualifier * mechanism
(** [neutral m] specifies the {!type:qualifier} of the given mechanism [m].
    Regardless the result of the mechanism (if it matches or not), {!val:check}
    tries the next mechanism. If the mechanism is the last one, {!val:check}
    returns [`Neutral] so. *)

module Record : sig
  type t

  val v : (qualifier * mechanism) list -> modifier list -> t
  (** [record ms []] returns a record which can be serialized into the zone file
      of a specific domain-name as the sender policy. *)

  val to_string : t -> string
  (** [record_to_string v] returns the serialized version of the record to be
      able to save it into the zone file of a domain-name as the sender policy.
  *)

  val of_string : ctx:ctx -> string -> (t, [> `Msg of string ]) result
  (** [record_of_string ~ctx str] tries to parse {b and} expand macro of the
      given string which should come from the TXT record of a domain-name as the
      sender policy of this domain-name. *)

  val pp : t Fmt.t
  val equal : t -> t -> bool
end

module Result : sig
  type t =
    [ `None
    | `Neutral
    | `Pass of mechanism
    | `Fail
    | `Softfail
    | `Temperror
    | `Permerror ]

  val pp : Format.formatter -> t -> unit
end

type error =
  [ `Msg of string
  | `No_data of [ `raw ] Domain_name.t * Dns.Soa.t
  | `No_domain of [ `raw ] Domain_name.t * Dns.Soa.t ]

type 'a response = ('a, error) result
type 'a record = 'a Dns.Rr_map.key

type 'a choose = {
    none: (unit -> 'a t) option
  ; neutral: (unit -> 'a t) option
  ; pass: (mechanism -> 'a t) option
  ; fail: (unit -> 'a t) option
  ; softfail: (unit -> 'a t) option
  ; temperror: (unit -> 'a t) option
  ; permerror: (unit -> 'a t) option
  ; fn: unit -> 'a t
}

and 'a t =
  | Return : 'a -> 'a t
  | Request : _ Domain_name.t * 'a record * ('a response -> 'b t) -> 'b t
  | Tries : (unit -> unit t) list -> unit t
  | Map : 'a t * ('a -> 'b) -> 'b t
  | Choose_on : 'a choose -> 'a t

exception Result of Result.t

val terminate : Result.t -> 'a
val get_and_check : ctx -> unit t

val to_field :
     ctx:ctx
  -> ?receiver:Emile.domain
  -> Result.t
  -> Mrmime.Field_name.t * Unstrctrd.t
(** [to_field ~ctx ?received v] serializes as an email field the result of the
    sender policy check according to the given [ctx]. The user is able to
    prepend then its email with this field. *)

module Extract : sig
  type result =
    [ `None | `Neutral | `Pass | `Fail | `Softfail | `Temperror | `Permerror ]

  type field = {
      result: result
    ; receiver: Emile.domain option
    ; sender: Emile.mailbox option
    ; ip: Ipaddr.t option
    ; ctx: ctx
  }

  type extract

  type decode =
    [ `Await of extract | `Fields of field list | `Malformed of string ]

  val pp : field Fmt.t
  val extractor : unit -> extract
  val extract : extract -> decode
  val src : extract -> string -> int -> int -> extract
  val of_string : string -> (field, [> `Msg of string ]) Stdlib.result
  val of_unstrctrd : Unstrctrd.t -> (field, [> `Msg of string ]) Stdlib.result
end

module Encoder : sig
  open Prettym

  val result : Prettym.ppf -> Result.t -> Prettym.ppf
  val comment : ctx:ctx -> ?receiver:Emile.domain -> ppf -> Result.t -> ppf
  val field : ctx:ctx -> ?receiver:Emile.domain -> ppf -> Result.t -> ppf
end

val field_received_spf : Mrmime.Field_name.t
