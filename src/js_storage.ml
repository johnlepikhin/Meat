
open Js
open Unsafe
open Js_primitives

let storage =
	get window "localStorage"

module M = struct
	let set (name : string) (v : string) : unit =
		let name = inject (string name) in
		let v = inject (string v) in
		meth_call storage "setItem" [| name; v |]

	let get (name : string) : string option =
		let name = inject (string name) in
		let v = meth_call storage "getItem" [| name |] in
		match Opt.to_option v with
			| None -> None
			| Some s -> Some (to_string s)

	let remove (name : string) : unit =
		let name = inject (string name) in
		meth_call storage "removeItem" [| name |]
end

module type NAMESPACE = sig
	val prefix : string
end

module MlExpirable = functor(NS : NAMESPACE) -> struct
	let create_name n = NS.prefix ^ "." ^ n

	type 'a t = {
		expire_time : int64;
		v : 'a;
	}

	let set ~timeout ~name v =
		let v = {
			expire_time = Int64.of_float (Js_date.now () +. timeout);
			v = v;
		} in
		let name = create_name name in
		let v = Bit6.a_to_bit6 v in
		M.set name v

	let get name =
		let name = create_name name in
		let v = M.get name in
		match v with
			| None -> None
			| Some v ->
				let v = Bit6.bit6_to_a v in
				match v with
					| None -> None
					| Some v ->
						let now = Int64.of_float (Js_date.now ()) in
						if v.expire_time > now then
							Some v.v
						else
							None

	let remove name =
		let name = create_name name in
		M.remove name
end

module Common = MlExpirable(struct let prefix = "common" end)
