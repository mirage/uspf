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

  let getaddrinfo dns response domain_name =
    let vs =
      match response with
      | `TXT ->
          Dns_client_unix.getaddrinfo dns Dns.Rr_map.Txt domain_name
          >>= fun (_, txts) -> Ok (Dns.Rr_map.Txt_set.elements txts)
      | `A ->
          Dns_client_unix.getaddrinfo dns Dns.Rr_map.A domain_name
          >>= fun (_, vs) ->
          Ok (List.map Ipaddr.V4.to_string (Dns.Rr_map.Ipv4_set.elements vs))
      | `AAAA ->
          Dns_client_unix.getaddrinfo dns Dns.Rr_map.Aaaa domain_name
          >>= fun (_, vs) ->
          Ok (List.map Ipaddr.V6.to_string (Dns.Rr_map.Ipv6_set.elements vs))
      | `MX ->
          Dns_client_unix.getaddrinfo dns Dns.Rr_map.Mx domain_name
          >>= fun (_, vs) ->
          Ok
            (List.map
               (fun { Dns.Mx.mail_exchange; _ } ->
                 Domain_name.to_string mail_exchange)
               (Dns.Rr_map.Mx_set.elements vs)) in
    Unix_scheduler.inj vs
end

let get_records :
    Spf.ctx ->
    ([ `None | `Permerror | `Record of Spf.record ], [> `Msg of string ]) result
    =
 fun ctx ->
  let dns = Dns_client_unix.create ~timeout:5_000_000L () in
  Spf.get_records ctx state dns (module DNS) |> Unix_scheduler.prj
