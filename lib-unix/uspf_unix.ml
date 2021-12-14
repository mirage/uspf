module Unix_scheduler = Uspf.Sigs.Make (struct
  type 'a t = 'a
end)

let state =
  let open Uspf.Sigs in
  let open Unix_scheduler in
  { return = (fun x -> inj x); bind = (fun x f -> f (prj x)) }

module DNS = struct
  type t = Dns_client_unix.t
  and backend = Unix_scheduler.t

  and error =
    [ `Msg of string
    | `No_data of [ `raw ] Domain_name.t * Dns.Soa.t
    | `No_domain of [ `raw ] Domain_name.t * Dns.Soa.t ]

  let getrrecord dns response domain_name =
    Unix_scheduler.inj
    @@ Dns_client_unix.get_resource_record dns response domain_name
end

module Flow = struct
  type flow = in_channel
  and backend = Unix_scheduler.t

  let input ic tmp off len = Unix_scheduler.inj @@ input ic tmp off len
end

let ( >>| ) x f = Result.map f x

let check ?nameservers ~timeout ctx =
  let dns = Dns_client_unix.create ?nameservers ~timeout () in
  Uspf.get ~ctx state dns (module DNS) |> Unix_scheduler.prj >>| fun record ->
  Uspf.check ~ctx state dns (module DNS) record |> Unix_scheduler.prj

let extract_received_spf ?newline ic =
  Uspf.extract_received_spf ?newline ic state (module Flow)
  |> Unix_scheduler.prj
