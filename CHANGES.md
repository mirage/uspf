### 0.2.0 (2025-11-27) Paris - France

- Add `Uspf.get` and `Uspf_lwt.get` to check if the SPF record exists
  (@dinosaure, #32, #33)

### 0.1.0 (2025-04-28) Paris - France

- Remove Higher Kinded Polymorphism (#26, #27, @dinosaure)
- Be able to merge multiple SPF contexts (#30, @dinosaure)
- Lint the library and delete useless code (#29, @dinosaure)
- Be able to emit a `Received-SPF` without IP address

### 0.0.4 (2024-10-25) Paris - France

- Add a `mirage` derivation of `uspf` (#23, @dinosaure)
- Handle `redirect` modifier (#24, @dinosaure)

### 0.0.3 (2023-03-09) Paris - France

- Add constraint on `fmt` (@kit-ty-kate, #16)
- Be neutral when the DNS record does not give to us anything (@dinosaure, #17)
- Fix `pp_mechanism` for `a` and `mx` if no domain-spec are present (no `:`)
  (@hannesm, @dinosaure, #18)
- Update `ocamlformat` (@dinosaure, #19, #20)
- Use the last version of `mirage-crypto-rng` (@dinosaure, #20)
- Lint dependencies and remove the `astring` dependency (@dinosaure, #20)
- Update the documentation (@dinosaure, #20)
- Split the libraries according to schedulers (@dinosaure, #21)

### 0.0.2 (2021-12-17) Paris - France

- Delete `rresult` dependency (@dinosaure, #15)

### 0.0.1 (2021-12-08) Paris - France

- First release of `uspf`
