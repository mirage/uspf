# µSPF, a SPF verifier/signer in OCaml

`uspf` is a simple library to check or _sign_ an incoming emails from -data.
See [RFC 7208][rfc7208].

## Usage

### How to install it?

You must have an OPAM environment. Then, `uspf` can be installed with:

```sh
$ opam install uspf
```

### How to use it?

`uspf` is a little tool which can be used to verify DNS records from some
informations (like where comes from the incoming email - especially, what is
the IP address of the sender).

From these informations and the DNS record, we are able to check if the IP
address is allowed to send an email under a certain domain-name.

By this way, `uspf` requires:
- few informations (the IP address of the sender, its domain-name, etc.)
- a DNS stack to get records

```ocaml
(* Informations required by uSPF *)
let ctx =
  Uspf.empty
  |> Uspf.with_sender (`MAILFROM address)
  |> Uspf.with_ip ipaddr

(* DNS record *)
let record = Uspf.get ctx sched dns (module DNS)

(* Verification *)
let result = Uspf.check ctx sched dns (module DNS)
```

From the result, the user is able to emit a new [RFC822][rfc822] field _via_
[mrmime][mrmime]:

```ocaml
let field_name, value = Uspf.to_field ~ctx ?receiver result
let () =
  Format.printf "%s: %s\n%!"
    (Mrmime.Field_name.to_string field_name)
    (Unstrctrd.to_utf_8_string value)
```

`uspf` has received funding from the Next Generation Internet Initiative
(NGI) within the framework of the DAPSI Project.

[mrmime]: https://github.com/mirage/mrmime
[rfc822]: https://www.rfc-editor.org/info/rfc822
[rfc7208]: https://www.rfc-editor.org/info/rfc7208
