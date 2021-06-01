let github_com = Domain_name.(host_exn (of_string_exn "github.com"))

let noreply_github_com = Colombe.Path.of_string_exn "<noreply@github.com>"

let ctx =
  Spf.empty
  |> Spf.with_sender (`HELO github_com)
  |> Spf.with_sender (`MAILFROM noreply_github_com)
  |> Spf.with_ip (Ipaddr.of_string_exn "192.30.252.192")

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
  match Spf_unix.check ~timeout:5_000_000_000L ctx with
  | Ok res -> Fmt.pr "%a\n%!" Spf.pp_res res
  | Error (`Msg err) -> Fmt.epr "[ERR]: %s.\n%!" err
