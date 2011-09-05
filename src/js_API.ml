open Js
open Js_primitives

module DH = Dom_html

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

let request ~args func =
	lwt url = URL.make func [] in
	lwt r = XmlHttpRequest.perform ~post_args:args url in
	if (r.XmlHttpRequest.code = 200) then
	begin
		let r = API.of_string r.XmlHttpRequest.content in
		match r with
			| None ->
				protocol_error "Получены некорректные данные от сервера"
			| Some (API.Data v) ->
				Lwt.return v
			| Some (API.Error s) ->
				protocol_error s
	end
	else
		protocol_error ("HTTP response code = " ^ (string_of_int r.XmlHttpRequest.code) ^ "\n\n" ^ r.XmlHttpRequest.content)
