open Js
open Js_primitives

module DH = Dom_html
module M = Microml

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
			| None -> fatal "не смогли преобразовать URL"
			| Some Url.Http url -> Lwt.return (Url.Http { url with Url.hu_path_string = spath; Url.hu_path = path; Url.hu_arguments = get_args })
			| Some Url.Https url -> Lwt.return (Url.Https { url with Url.hu_path_string = spath; Url.hu_path = path; Url.hu_arguments = get_args })
			| Some Url.File url -> fatal "не смогли преобразовать URL этого типа"
end

let protocol_error e = Lwt.fail (Request e)
	
let request ~get_args ~post_args func =
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

let request_a ~get_args ~post_args func =
	lwt r = request ~get_args ~post_args func in
	match r with
		| M.String s -> Lwt.fail (API s)
		| M.List v -> Lwt.return v

let request_1 ~get_args ?post_args func =
	lwt r = request_a ~get_args ~post_args func in
	match r with
		| [M.String v] -> Lwt.return v
		| _ -> Lwt.fail Invalid_response

let request_list ~get_args ?post_args func =
	lwt r = request_a ~get_args ~post_args func in
	let rec loop = function
		| [] -> Lwt.return []
		| M.String v :: tl ->
			lwt tl = loop tl in
			Lwt.return (v :: tl)
		| _ -> Lwt.fail Invalid_response
	in
	loop r

