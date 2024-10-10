module Lwt_scheduler = Uspf.Sigs.Make (struct
  type 'a t = 'a Lwt.t
end)

let state =
  let open Uspf.Sigs in
  let open Lwt_scheduler in
  let open Lwt.Infix in
  {
    bind = (fun x f -> inj (prj x >>= fun x -> prj (f x)));
    return = (fun x -> inj (Lwt.return x));
  }

module Make (Dns_client : Dns_client_mirage.S) = struct
  module Dns = struct
    type t = Dns_client.t
    type backend = Lwt_scheduler.t

    type error =
      [ `Msg of string
      | `No_data of [ `raw ] Domain_name.t * Dns.Soa.t
      | `No_domain of [ `raw ] Domain_name.t * Dns.Soa.t ]

    let getrrecord dns key domain_name =
      Dns_client.get_resource_record dns key domain_name |> Lwt_scheduler.inj
  end

  let get ~ctx dns = Uspf.get ~ctx state dns (module Dns) |> Lwt_scheduler.prj

  let check ~ctx dns record =
    Uspf.check ~ctx state dns (module Dns) record |> Lwt_scheduler.prj
end
