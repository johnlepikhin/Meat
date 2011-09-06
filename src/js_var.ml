open Js_primitives

let var_opt =
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

let var name =
	match var_opt name with
		| None -> fatal ("Не найдено одно очень важное значение: " ^ name)
		| Some var -> Lwt.return var

let set_string name v =
	Js.Unsafe.set window name (Js.string v)

let string_opt name =
	match var_opt name with
		| None -> None
		| Some s -> Some (Js.to_string s)

let string name =
	lwt s = var name in
	Lwt.return (Js.to_string s)

module type GLOBAL_ML_VAR = sig
	type t

	val name: string
end

module GlobalMlVar = functor(N : GLOBAL_ML_VAR) -> struct
	let get () =
		lwt v = string N.name in
		let r : N.t option = API.of_string v in
		match r with
			| None -> fatal ("Не найдена одна очень важная переменная: " ^ N.name)
			| Some v -> Lwt.return v

	let set (v : N.t) =
		let s = API.to_string v in
		set_string N.name s
end
