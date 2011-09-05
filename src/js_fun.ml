
open Js_primitives

let var =
	let undef = Js.string "undefined" in
	fun name ->
		try
			let v = Js.Unsafe.variable name in
			if Js.typeof v = undef then
				None
			else
				Some v
		with
			| _ -> None

let string name =
	match var name with
		| None -> None
		| Some s -> Some (Js.to_string s)

module type NewFUN = sig
	type returns

	type real_returns

	type input

	val name: string

	val make_args: input -> Js.Unsafe.any array

	val make_ret: real_returns -> returns

	val f: input -> returns
end

let fatal () = fatal "Не найдена одна очень важная функция"

module NewFUN = functor(F : NewFUN) -> struct
	let _ =
		Js.Unsafe.set window F.name (Js.wrap_callback F.f)

	let call (args : F.input) : F.returns =
		let args = F.make_args args in
		let f = var F.name in
		match f with
			| None -> fatal ()
			| Some f ->
				let r = Js.Unsafe.fun_call f args in
				F.make_ret r
end

module type ExFUN = sig
	type returns

	type real_returns

	type input

	val name: string

	val make_args: input -> Js.Unsafe.any array

	val make_ret: real_returns -> returns
end

module ExFUN = functor(F : ExFUN) -> struct
	let call (args : F.input) : F.returns =
		let args = F.make_args args in
		let f = var F.name in
		match f with
			| None -> fatal ()
			| Some f ->
				let r = Js.Unsafe.fun_call f args in
				F.make_ret r
end
