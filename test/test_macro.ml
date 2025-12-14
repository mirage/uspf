open Rresult

let msg = Alcotest.testable Rresult.R.pp_msg ( = )
let ( % ) f g x = f (g x)

let reporter ppf =
  let report src level ~over k msgf =
    let k _ = over () ; k () in
    let with_metadata header _tags k ppf fmt =
      Format.kfprintf k ppf
        ("%a[%a]: " ^^ fmt ^^ "\n%!")
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src) in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt in
  { Logs.report }

let () = Logs.set_reporter (reporter Fmt.stdout)
let () = Logs.set_level ~all:true (Some Logs.Debug)

let test01 =
  Alcotest.test_case "rfc7208" `Quick @@ fun () ->
  let ctx =
    Uspf.empty
    |> Uspf.with_sender (`HELO (Domain_name.of_string_exn "mx.example.org"))
    |> Uspf.with_sender
         (`MAILFROM
            (Colombe.Path.of_string_exn "<strong-bad@email.example.com>")) in
  Alcotest.(check (result string msg))
    "01"
    (Uspf.Macro.expand_string ctx "%{s}" >>| Domain_name.to_string)
    (Ok "strong-bad@email.example.com") ;
  Alcotest.(check (result string msg))
    "02"
    (Uspf.Macro.expand_string ctx "%{o}" >>| Domain_name.to_string)
    (Ok "email.example.com") ;
  Alcotest.(check (result string msg))
    "03"
    (Uspf.Macro.expand_string ctx "%{d}" >>| Domain_name.to_string)
    (Ok "email.example.com") ;
  Alcotest.(check (result string msg))
    "04"
    (Uspf.Macro.expand_string ctx "%{d4}" >>| Domain_name.to_string)
    (Ok "email.example.com") ;
  Alcotest.(check (result string msg))
    "05"
    (Uspf.Macro.expand_string ctx "%{d3}" >>| Domain_name.to_string)
    (Ok "email.example.com") ;
  Alcotest.(check (result string msg))
    "06"
    (Uspf.Macro.expand_string ctx "%{d2}" >>| Domain_name.to_string)
    (Ok "example.com") ;
  Alcotest.(check (result string msg))
    "07"
    (Uspf.Macro.expand_string ctx "%{d1}" >>| Domain_name.to_string)
    (Ok "com") ;
  Alcotest.(check (result string msg))
    "08"
    (Uspf.Macro.expand_string ctx "%{dr}" >>| Domain_name.to_string)
    (Ok "com.example.email") ;
  Alcotest.(check (result string msg))
    "09"
    (Uspf.Macro.expand_string ctx "%{d2r}" >>| Domain_name.to_string)
    (Ok "example.email") ;
  Alcotest.(check (result string msg))
    "10"
    (Uspf.Macro.expand_string ctx "%{l}" >>| Domain_name.to_string)
    (Ok "strong-bad") ;
  Alcotest.(check (result string msg))
    "11"
    (Uspf.Macro.expand_string ctx "%{l-}" >>| Domain_name.to_string)
    (Ok "strong.bad") ;
  Alcotest.(check (result string msg))
    "12"
    (Uspf.Macro.expand_string ctx "%{lr}" >>| Domain_name.to_string)
    (Ok "strong-bad") ;
  Alcotest.(check (result string msg))
    "13"
    (Uspf.Macro.expand_string ctx "%{lr-}" >>| Domain_name.to_string)
    (Ok "bad.strong") ;
  Alcotest.(check (result string msg))
    "14"
    (Uspf.Macro.expand_string ctx "%{l1r-}" >>| Domain_name.to_string)
    (Ok "strong")

let test02 =
  Alcotest.test_case "rfc7208" `Quick @@ fun () ->
  let ctx =
    Uspf.empty
    |> Uspf.with_ip (Ipaddr.of_string_exn "192.0.2.3")
    |> Uspf.with_sender (`HELO (Domain_name.of_string_exn "mx.example.org"))
    |> Uspf.with_sender
         (`MAILFROM
            (Colombe.Path.of_string_exn "<strong-bad@email.example.com>")) in
  Alcotest.(check (result string msg))
    "01"
    (Uspf.Macro.expand_string ctx "%{ir}.%{v}._spf.%{d2}"
    >>| Domain_name.to_string)
    (Ok "3.2.0.192.in-addr._spf.example.com") ;
  Alcotest.(check (result string msg))
    "02"
    (Uspf.Macro.expand_string ctx "%{lr-}.lp._spf.%{d2}"
    >>| Domain_name.to_string)
    (Ok "bad.strong.lp._spf.example.com") ;
  Alcotest.(check (result string msg))
    "03"
    (Uspf.Macro.expand_string ctx "%{ir}.%{v}.%{l1r-}.lp._spf.%{d2}"
    >>| Domain_name.to_string)
    (Ok "3.2.0.192.in-addr.strong.lp._spf.example.com") ;
  Alcotest.(check (result string msg))
    "04"
    (Uspf.Macro.expand_string ctx "%{d2}.trusted-domains.example.net"
    >>| Domain_name.to_string)
    (Ok "example.com.trusted-domains.example.net") ;
  let ctx = Uspf.with_ip (Ipaddr.of_string_exn "2001:db8::cb01") ctx in
  Alcotest.(check (result string msg))
    "05"
    (Uspf.Macro.expand_string ctx "%{ir}.%{v}._spf.%{d2}"
    >>| Domain_name.to_string)
    (Ok
       "1.0.b.c.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.8.b.d.0.1.0.0.2.ip6._spf.example.com")

let test03 =
  Alcotest.test_case "record" `Quick @@ fun () ->
  let record =
    "v=spf1 ip4:184.104.202.128/27 ip4:184.104.202.96/27 ip4:216.218.159.0/27 \
     ip4:216.218.240.64/26 ip4:64.71.168.192/26 ip4:65.19.128.64/26 \
     ip4:66.220.12.128/27 ip4:72.52.80.0/26 ip4:64.62.250.96/27 \
     ip6:2001:470:1:235::/64 ip6:2001:470:1:258::/64 ip6:2001:470:1:3a8::/64 \
     ip6:2001:470:1:59e::/64 ip6:2001:470:1:669::/64 ip6:2001:470:1:791::/64 \
     ip6:2001:470:1:9a5::/64 ip6:2001:470:1:9f1::/64 \
     ip6:2602:fd3f:0000:ff06::/64 include:mailgun.org mx ptr ~all" in
  match Uspf.Term.parse_record record with
  | Ok _ -> ()
  | Error (`Msg err) -> Alcotest.failf "%s." err

let test04 =
  Alcotest.test_case "record" `Quick @@ fun () ->
  let ipv4_0 =
    Uspf.(pass @@ v4 (Ipaddr.V4.Prefix.of_string_exn "192.168.0.0/24")) in
  let ipv4_1 =
    Uspf.(pass @@ v4 (Ipaddr.V4.Prefix.of_string_exn "10.0.0.0/24")) in
  let reject = Uspf.(fail all) in
  let record = Uspf.Record.v [ ipv4_0; ipv4_1; reject ] [] in
  let str = Uspf.Record.to_string record in
  Alcotest.(check string) "record" str "ip4:192.168.0.0/24 ip4:10.0.0.0/24 -all"

type getrrecord = {
    fn: 'a 'r. 'a Domain_name.t -> 'r Dns.Rr_map.key -> 'r Uspf.response
}

let eval : type a. getrrecord:getrrecord -> a Uspf.t -> Uspf.Result.t option =
 fun ~getrrecord t ->
  let rec go : type a. a Uspf.t -> a = function
    | Request (domain_name, record, fn) ->
        Logs.debug (fun m -> m "DNS request on %a" Domain_name.pp domain_name) ;
        let resp = getrrecord.fn domain_name record in
        go (fn resp)
    | Return v -> v
    | Tries fns -> List.iter (fun fn -> go (fn ())) fns
    | Map (x, fn) -> fn (go x)
    | Choose_on c ->
    try go (c.fn ())
    with Uspf.Result result ->
      let none _ = Uspf.terminate result in
      let some = Fun.id in
      let fn =
        match result with
        | `None -> Option.fold ~none ~some c.none
        | `Neutral -> Option.fold ~none ~some c.neutral
        | `Fail -> Option.fold ~none ~some c.fail
        | `Softfail -> Option.fold ~none ~some c.softfail
        | `Temperror -> Option.fold ~none ~some c.temperror
        | `Permerror -> Option.fold ~none ~some c.permerror
        | `Pass m -> begin
            fun () -> match c.pass with Some pass -> pass m | None -> none ()
          end in
      go (fn ()) in
  match go t with exception Uspf.Result result -> Some result | _ -> None

let test05 =
  Alcotest.test_case "mx optional domain name" `Quick @@ fun () ->
  let getrrecord : type a r.
      a Domain_name.t -> r Dns.Rr_map.key -> r Uspf.response =
   fun domain_name record ->
    let _192_168_1_1 = Ipaddr.V4.(Set.singleton (of_string_exn "192.168.1.1")) in
    let fc00__1 = Ipaddr.V6.(Set.singleton (of_string_exn "fc00::1")) in
    let mxs =
      Dns.Rr_map.Mx_set.singleton
        {
          Dns.Mx.preference= 10
        ; mail_exchange= Domain_name.(host_exn (of_string_exn "mail.bar.com"))
        } in
    match (record, Domain_name.to_string domain_name) with
    | Dns.Rr_map.Txt, "bar.com" ->
        Ok (0l, Dns.Rr_map.Txt_set.singleton "v=spf1 mx a:foo.com -all")
    | Dns.Rr_map.A, "mail.bar.com" -> Ok (0l, _192_168_1_1)
    | Dns.Rr_map.Aaaa, "mail.bar.com" -> Ok (0l, fc00__1)
    | Dns.Rr_map.Mx, "bar.com" -> Ok (0l, mxs)
    | _ ->
        R.error_msgf "Error on %a:%a." Dns.Rr_map.ppk (Dns.Rr_map.K record)
          Domain_name.pp domain_name in
  let getrrecord = { fn= getrrecord } in

  (* ipv4 *)
  let ctx =
    Uspf.empty
    |> Uspf.with_sender (`HELO (Domain_name.of_string_exn "bar.com"))
    |> Uspf.with_sender (`MAILFROM (Colombe.Path.of_string_exn "<x@bar.com>"))
    |> Uspf.with_ip (Ipaddr.of_string_exn "192.168.1.1") in
  let result = eval ~getrrecord (Uspf.get_and_check ctx) in
  (match result with
  | Some (`Pass _) -> Alcotest.(check pass) "spf" () ()
  | Some result -> Alcotest.failf "Invalid SPF result: %a" Uspf.Result.pp result
  | None -> Alcotest.failf "Impossible to compute a result") ;

  (* ipv6 *)
  let ctx =
    Uspf.empty
    |> Uspf.with_sender (`HELO (Domain_name.of_string_exn "bar.com"))
    |> Uspf.with_sender (`MAILFROM (Colombe.Path.of_string_exn "<x@bar.com>"))
    |> Uspf.with_ip (Ipaddr.of_string_exn "fc00::1") in
  let result = eval ~getrrecord (Uspf.get_and_check ctx) in
  match result with
  | Some (`Pass _) -> Alcotest.(check pass) "spf" () ()
  | Some result -> Alcotest.failf "Invalid SPF result: %a" Uspf.Result.pp result
  | None -> Alcotest.failf "Impossible to compute a result"

let test06 =
  Alcotest.test_case "a" `Quick @@ fun () ->
  let getrrecord : type a r.
      a Domain_name.t -> r Dns.Rr_map.key -> r Uspf.response =
   fun domain_name record ->
    let _192_168_1_1 = Ipaddr.V4.(Set.singleton (of_string_exn "192.168.1.1")) in
    let fc00__1 = Ipaddr.V6.(Set.singleton (of_string_exn "fc00::1")) in
    match (record, Domain_name.to_string domain_name) with
    | Dns.Rr_map.Txt, "bar.com" ->
        Ok (0l, Dns.Rr_map.Txt_set.singleton "v=spf1 a:bar.com -all")
    | Dns.Rr_map.A, "bar.com" -> Ok (0l, _192_168_1_1)
    | Dns.Rr_map.Aaaa, "bar.com" -> Ok (0l, fc00__1)
    | _ ->
        R.error_msgf "Error on %a:%a." Dns.Rr_map.ppk (Dns.Rr_map.K record)
          Domain_name.pp domain_name in
  let getrrecord = { fn= getrrecord } in

  (* ipv4 *)
  let ctx =
    Uspf.empty
    |> Uspf.with_sender (`HELO (Domain_name.of_string_exn "bar.com"))
    |> Uspf.with_sender (`MAILFROM (Colombe.Path.of_string_exn "<x@bar.com>"))
    |> Uspf.with_ip (Ipaddr.of_string_exn "192.168.1.1") in
  let result = eval ~getrrecord (Uspf.get_and_check ctx) in
  (match result with
  | Some (`Pass _) -> Alcotest.(check pass) "spf" () ()
  | Some result -> Alcotest.failf "Invalid SPF result: %a" Uspf.Result.pp result
  | None -> Alcotest.failf "Impossible to compute a result") ;

  (* ipv6 *)
  let ctx =
    Uspf.empty
    |> Uspf.with_sender (`HELO (Domain_name.of_string_exn "bar.com"))
    |> Uspf.with_sender (`MAILFROM (Colombe.Path.of_string_exn "<x@bar.com>"))
    |> Uspf.with_ip (Ipaddr.of_string_exn "fc00::1") in
  let result = eval ~getrrecord (Uspf.get_and_check ctx) in
  match result with
  | Some (`Pass _) -> Alcotest.(check pass) "spf" () ()
  | Some result -> Alcotest.failf "Invalid SPF result: %a" Uspf.Result.pp result
  | None -> Alcotest.failf "Impossible to compute a result"

let () =
  Alcotest.run "decoding"
    [
      ("macro", [ test01; test02 ]); ("record", [ test03; test04 ])
    ; ("spf", [ test05; test06 ])
    ]
