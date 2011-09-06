open Js_primitives

open Js_var

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
		let f = var_opt F.name in
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
		let f = var_opt F.name in
		match f with
			| None -> fatal ()
			| Some f ->
				let r = Js.Unsafe.fun_call f args in
				F.make_ret r
end

module type NewACTION = sig
	type input

	val name: string

	val make_args: input -> Js.Unsafe.any array

	val f: input -> unit Lwt.t
end

module NewACTION = functor(F : NewACTION) -> struct
	let _ =
		Js.Unsafe.set window F.name (Js.wrap_callback F.f)

	let call (args : F.input) : unit Lwt.t =
		let args = F.make_args args in
		let f = var_opt F.name in
		match f with
			| None -> fatal ()
			| Some f -> Js.Unsafe.fun_call f args
end


