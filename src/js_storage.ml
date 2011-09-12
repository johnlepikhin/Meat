
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
		Opt.to_option v

	let remove (name : string) : unit =
		let name = inject (string name) in
		meth_call storage "removeItem" [| name |]
end
