
open Js
module D = Dom
module DH = Dom_html

exception Request of string

let doc = Dom_html.document
let window = Dom_html.window
let alert e = window##alert (string e)

let errmsg e = "Прозошло что-то непредвиденное. Попробуйте загрузить страницу заново. Если ошибка повторится — дайте знать администрации сайта. Подробности об ошибке:\n\n" ^ e

let fatal e =
	alert (errmsg e);
	raise Exit

let protocol_error e = Lwt.fail (Request e)

let (is_ssl, url) =
	match Url.Current.get () with
		| None -> fatal "Не удалось получить текущий URL"
		| Some Url.Http url -> false, url
		| Some Url.Https url -> true, url
		| Some Url.File _ -> fatal "Некорректный текущий URL"

module URL = struct
	let args_rex = Regexp.regexp "&"
	let arg_rex = Regexp.regexp "\\??([^=]+)=(.*)"
	let plus_rex = Regexp.regexp "\\+"

	let replace_plus s = Regexp.global_replace plus_rex s " "

	let args =
		let l = Regexp.split args_rex (to_string (DH.window##location##search)) in
		let rec loop = function
			| hd :: tl ->
			begin
				match Regexp.string_match arg_rex hd 0 with
					| None -> loop tl
					| Some r ->
					begin
						let n = Regexp.matched_group r 1 in
						let v = Regexp.matched_group r 2 in
						match n, v with
							| Some n, Some v -> ((replace_plus (Url.urldecode n)), (replace_plus (Url.urldecode v))) :: loop tl
							| _ -> loop tl
					end
			end
			| [] -> []
		in
		loop l

	let make path get_args =
		let spath = "/" ^ (String.concat "/" path) in
		let url = Url.Current.get () in
		match url with
			| None -> protocol_error "не смогли преобразовать URL"
			| Some Url.Http url -> Lwt.return (Url.Http { url with Url.hu_path_string = spath; Url.hu_path = path; Url.hu_arguments = get_args })
			| Some Url.Https url -> Lwt.return (Url.Https { url with Url.hu_path_string = spath; Url.hu_path = path; Url.hu_arguments = get_args })
			| Some Url.File url -> protocol_error "не смогли преобразовать URL этого типа"
end

let request ~get_args ?post_args func =
	lwt url = URL.make func get_args in
	lwt r = match post_args with
		| None -> XmlHttpRequest.perform url
		| Some post_args -> XmlHttpRequest.perform ~post_args url
	in
	lwt r =
		if (r.XmlHttpRequest.code = 200) then
			try
				let r = Microml.of_string r.XmlHttpRequest.content in
				Lwt.return r
			with
				| e -> protocol_error (Printexc.to_string e)
		else
			protocol_error ("HTTP response code = " ^ (string_of_int r.XmlHttpRequest.code) ^ "\n\n" ^ r.XmlHttpRequest.content)
	in
	Lwt.return r

module type ELEMENT = sig
	val id: string
end


module EID = struct
	type 'a t = {
		id : string;
		mutable el : 'a option;
	}

	exception NotFound of string
	exception CantCoerce of string

	let init id coerce =
		let v = {
			id = id;
			el = None;
		} in
		fun () ->
			match v.el with
				| None ->
					let e = doc##getElementById (string v.id) in
					begin
						match Js.Opt.to_option e with
							| None -> raise (NotFound v.id)
							| Some e ->
								begin
									let e = coerce e in
									match Js.Opt.to_option e with
										| None -> Lwt.fail (CantCoerce id)
										| Some e ->
											v.el <- Some e;
											Lwt.return e
								end
					end
				| Some e ->
					Lwt.return e
end

let body = EID.init Common.body_id Dom_html.CoerceTo.body

let wrap_error f arg =
	try_lwt
		f arg
	with
		| e ->
		begin
			let error = match e with
				| EID.NotFound s -> "Не найден элемент с ID='" ^ s ^ "'"
				| EID.CantCoerce s -> "Не получилось преобразовать тип для элемента с ID='" ^ s ^ "'"
				| e -> Printexc.to_string e
			in
			fatal error
		end

let handler f r = Dom_html.handler (fun v -> ignore (wrap_error f v); r)

let removeChilds p =
	let rec loop () =
		let c = p##firstChild in
		match Js.Opt.to_option c with
			| None -> ()
			| Some c -> D.removeChild p c; loop ()
	in
	loop ()

let removeElement el =
	let p = el##parentNode in
	match Js.Opt.to_option p with
		| None -> ()
		| Some p -> D.removeChild p el

let appendText n t = D.appendChild n (DH.document##createTextNode (string t))
