
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

module type T = sig
	type t

	val name : string
end

module MlExpirable (T : T) = struct
	type t = {
		expire_time : int64;
		v : T.t;
	}

	let set ~timeout (v : T.t) =
		let v = {
			expire_time = Int64.of_float (Js_date.now () +. timeout);
			v = v;
		} in
		let v = Bit6.a_to_bit6 v in
		M.set T.name v

	let get () =
		let v = M.get T.name in
		match v with
			| None -> Lwt.return None
			| Some v ->
				try_lwt
					lwt v = Bit6.bit6_to_a v in
					let now = Int64.of_float (Js_date.now ()) in
					if v.expire_time > now then
						let v : T.t = v.v in
						Lwt.return (Some v)
					else
						Lwt.return None
				with
					| _ -> Lwt.return None

	let remove () = M.remove T.name
end
