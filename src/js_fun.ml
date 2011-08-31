
open Js_common

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
		let f = Js.Unsafe.variable F.name in
		Js.Unsafe.fun_call f args
end
