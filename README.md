# (unsecure) Sender Policy Framework - uSPF

`uspf` is a simple tool and a library to _verify_ and _stamp_ an email from DNS
authorities. When you receive an email, it comes from a specific SMTP server
which has:
- a public IP address
- a domain-name

Sender Policy Framework is a mechanism to identify the sender and ensure that
it's the true sender (instead of an attacker) according to its domain-name and
its IP address.

Indeed, an email sended by foo@bar.org should come from bar.org. When, you
receive an email, a TCP/IP connection is established between your reception
server and the sender. By this fact, we can know the public IP address of
the sender.

Then, we know that the sender is under the bar.org DNS authority. SPF will
aggregate these informations and will ask _via_ DNS which IP should be
trusted if someone from bar.org send you an email.

For instance, these information can be know with:
```sh
$ dig +short TXT gmail.com
"v=spf1 redirect=_spf.google.com"
```

SPF will finally check that expected information such as the sender's IP
correspond to what the DNS authority says. If it true, we can ensure that
the incoming email really come from the right server - so it's probably not
a suspicious email. Otherwise, it's probably a spam or an altered email from
an attacker.

### How to use uSPF?

Currently, `uspf` is just a library. If you want a native tool to check your
email, you should take a look on [`blaze`][blaze] which implements `blaze.spf`:
```sh
$ export MSG=$(blaze.mdir new -D ~/maildir/ | tail -n1)
$ blaze.mdir get -D ~/maildir/ --new $MSG > new.eml
$ blaze.spf analyze new.eml
romain@blaze.org from 192.168.0.0: pass (expected: pass)
```

### Usage

Currently. `uspf` is used by `blaze` as a simple tool to let the user to
execute this mechanism into its emails. But the real usage of the Sender
Policy Framework is into the **reception** server - the SMTP server on `*:25`.

So, `uspf` is used into the [`ptt`][ptt] project which wants to implement an
SMTP server as a [MirageOS][mirage] unikernel.

### Support

`uspf` has received funding from the Next Generation Internet Initiative (NGI)
within the framework of the DAPSI Project.

[blaze]: https://github.com/dinosaure/blaze
[mirage]: https://mirage.io
[ptt]: https://github.com/dinosaure/ptt
