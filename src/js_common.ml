
open Js
open Js_primitives

module D = Dom
module DH = Dom_html

module type ELEMENT = sig
	val id: string
end

module Coerce = struct
	exception CantCoerce

	let coerce el f =
		let d = f el in
		match Js.Opt.to_option d with
			| None -> Lwt.fail CantCoerce
			| Some d -> Lwt.return d
end

module EID = struct
	type 'a t = {
		id : string;
		mutable el : 'a option;
	}

	exception NotFound of string

	let get name coerce =
		let e = doc##getElementById (string name) in
		match Js.Opt.to_option e with
			| None -> Lwt.fail (NotFound name)
			| Some e -> Coerce.coerce e coerce

	let init id coerce =
		let v = {
			id = id;
			el = None;
		} in
		fun () -> match v.el with
			| None ->
				lwt nv = get v.id coerce in
				v.el <- Some nv;
				Lwt.return nv
			| Some e -> Lwt.return e

	let string_of_field f =
		lwt f = f () in
		let r = to_string (f##value) in
		Lwt.return r

	let div id = init id Dom_html.CoerceTo.div
end

let body = EID.init Common.body_id Dom_html.CoerceTo.body

let wrap_error f arg =
	try_lwt
		f arg
	with
		| Exit -> Lwt.fail Exit
		| e ->
			let error = match e with
				| EID.NotFound s -> "Не найден элемент с ID='" ^ s ^ "'"
				| Coerce.CantCoerce -> "Не получилось преобразовать тип"
				| Js_primitives.Request s -> "Ошибка общения с сервером: " ^ s
				| Js_primitives.Invalid_response -> "Неожиданный ответ сервера"
				| e -> Printexc.to_string e
			in
			fatal error

let lwt_nofail f arg =
	try_lwt
		f ()
	with
		| _ -> Lwt.return ()

let eignore f arg =
	ignore (wrap_error f arg)

let handler f r = Dom_html.handler (fun v -> eignore f v; r)

let removeChilds p =
	let rec loop () =
		let c = p##firstChild in
		match Js.Opt.to_option c with
			| None -> ()
			| Some c -> D.removeChild p c; loop ()
	in
	loop ()

let getParent el =
	let p = el##parentNode in
	match Js.Opt.to_option p with
		| None -> Lwt.fail Not_found
		| Some p -> Lwt.return p

let replaceElement el newel =
	lwt p = getParent el in
	Dom.replaceChild p newel el;
	Lwt.return ()

let removeElement el =
	let p = el##parentNode in
	match Js.Opt.to_option p with
		| None -> ()
		| Some p -> D.removeChild p el

let appendText n t = D.appendChild n (DH.document##createTextNode (string t))

let asDiv el = Coerce.coerce el Dom_html.CoerceTo.div

let asElement el = Coerce.coerce el Dom_html.CoerceTo.element

let have_parent el =
	let p = el##parentNode in
	match Js.Opt.to_option p with
		| None -> false
		| Some _ -> true
