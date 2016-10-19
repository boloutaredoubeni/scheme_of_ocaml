open Core.Std

module Key = struct
  module T = struct
    type t = Lisp.t [@@deriving sexp]
    let compare = compare
    let hash = Hashtbl.hash
  end

  include T
  include Hashable.Make (T)
end

type env_t = (Key.t, Key.t) Hashtbl.t

let the_env: env_t = Key.Table.create ()

let add ~key ~data =
  match Hashtbl.add the_env ~key:key ~data:data with
  | `Duplicate -> failwith "Value is immutable for now"
  | `Ok -> ()

let lookup key = Hashtbl.find the_env key
