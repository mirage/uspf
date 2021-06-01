open Rresult

module Unix_scheduler = Spf.Sigs.Make (struct
  type 'a t = 'a
end)

let state =
  let open Spf.Sigs in
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

let check ?nameserver ~timeout ctx =
  let dns = Dns_client_unix.create ?nameserver ~timeout () in
  Spf.record ~ctx state dns (module DNS) |> Unix_scheduler.prj >>| fun record ->
  Spf.check ~ctx state dns (module DNS) record |> Unix_scheduler.prj
