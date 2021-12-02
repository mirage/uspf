let github_com = Domain_name.of_string_exn "github.com"

let janestreet_com = Domain_name.of_string_exn "janestreet.com"

let noreply_github_com = Colombe.Path.of_string_exn "<noreply@github.com>"

let tbraibant = Colombe.Path.of_string_exn "<tbraibant@janestreet.com>"

let receiver =
  let vs = Domain_name.(to_strings (of_string_exn (Unix.gethostname ()))) in
  `Domain vs

let ctx0 =
  Uspf.empty
  |> Uspf.with_sender (`HELO github_com)
  |> Uspf.with_sender (`MAILFROM noreply_github_com)
  |> Uspf.with_ip (Ipaddr.of_string_exn "192.30.252.192")

let ctx1 =
  Uspf.empty
  |> Uspf.with_sender (`HELO janestreet_com)
  |> Uspf.with_sender (`MAILFROM tbraibant)
  |> Uspf.with_ip (Ipaddr.of_string_exn "38.105.200.233")

let reporter ppf =
  let report src level ~over k msgf =
    let k _ =
      over () ;
      k () in
    let with_metadata header _tags k ppf fmt =
      Format.kfprintf k ppf
        ("%a[%a]: " ^^ fmt ^^ "\n%!")
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src) in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt in
  { Logs.report }

let sigpipe = 13

let () = Mirage_crypto_rng_unix.initialize ()

let () = Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ~utf_8:true ()

let () = Logs.set_reporter (reporter Fmt.stdout)

let () = Logs.set_level ~all:true (Some Logs.Debug)

let () =
  match Uspf_unix.check ~timeout:5_000_000_000L ctx0 with
  | Ok res ->
      let field_name, v = Uspf.to_field ~ctx:ctx0 ~receiver res in
      Fmt.pr "%a: %s\n%!" Mrmime.Field_name.pp field_name
        (Unstrctrd.to_utf_8_string v)
  | Error (`Msg err) -> Fmt.epr "[ERR]: %s.\n%!" err

let () =
  match Uspf_unix.check ~timeout:5_000_000_000L ctx1 with
  | Ok res ->
      let field_name, v = Uspf.to_field ~ctx:ctx1 ~receiver res in
      Fmt.pr "%a: %s\n%!" Mrmime.Field_name.pp field_name
        (Unstrctrd.to_utf_8_string v)
  | Error (`Msg err) -> Fmt.epr "[ERR]: %s.\n%!" err

let pp_result ppf = function
  | `None -> Fmt.string ppf "none"
  | `Neutral -> Fmt.string ppf "neutral"
  | `Pass -> Fmt.string ppf "pass"
  | `Fail -> Fmt.string ppf "fail"
  | `Softfail -> Fmt.string ppf "softfail"
  | `Temperror -> Fmt.string ppf "temperror"
  | `Permerror -> Fmt.string ppf "permerror"

let () =
  match Uspf_unix.extract_received_spf stdin with
  | Ok received_spf ->
      let check { Uspf.result; receiver; sender; ip; ctx } =
        Fmt.pr "Expected result: %a.\n%!" pp_result result ;
        Fmt.pr "Receiver: %a.\n%!" Fmt.(Dump.option Emile.pp_domain) receiver ;
        Fmt.pr "Sender: %a.\n%!" Fmt.(Dump.option Emile.pp_mailbox) sender ;
        Fmt.pr "IP: %a.\n%!" Fmt.(Dump.option Ipaddr.pp) ip ;
        match Uspf_unix.check ~timeout:5_000_000_000L ctx with
        | Ok res ->
            let field_name, v = Uspf.to_field ~ctx ?receiver res in
            Fmt.pr "%a: %s\n%!" Mrmime.Field_name.pp field_name
              (Unstrctrd.to_utf_8_string v)
        | Error (`Msg err) -> Fmt.epr "[ERR]: %s.\n%!" err in
      List.iter check received_spf
  | Error (`Msg err) -> Fmt.epr "[ERR]: %s.\n%!" err
