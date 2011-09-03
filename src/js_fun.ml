
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

module type FUN = sig
	type returns

	type input

	val name: string

	val make_args: input -> Js.Unsafe.any array

	val f: input -> returns
end

module F = functor(F : FUN) -> struct
	let _ =
		Js.Unsafe.set window F.name (Js.wrap_callback F.f)

	let call (args : F.input) : F.returns =
		let args = F.make_args args in
		let f = var F.name in
		match f with
			| None -> fatal "Не найдена одна очень важная функция"
			| Some f -> Js.Unsafe.fun_call f args
end


