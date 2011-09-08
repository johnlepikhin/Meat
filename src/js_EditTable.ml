
module type INFO = sig
	val element_tr_prefix: string

	val delete_div_prefix: string

	val get_list: unit -> int list Lwt.t

	val set_list: int list -> unit

	val can_delete: int -> bool Lwt.t

	val try_delete: int -> bool Lwt.t

	val css_element: string

	val css_element_hover: string
end

module EditTable = functor(C : Js_controllers.TYPE) -> functor(Info : INFO) -> struct

	let el prefix id coerce =
		let id = prefix ^ (string_of_int id) in
		Js_common.EID.get id coerce

	let element id = el Info.element_tr_prefix id Dom_html.CoerceTo.tr

	let element_delete id = el Info.delete_div_prefix id Dom_html.CoerceTo.div

	let if_can_edit f =
		lwt can_edit = C.can_edit () in
		if can_edit then
			f ()
		else
			Lwt.return ()

	let onmouseover id _ = if_can_edit (fun () ->
		lwt el = element id in
		el##className <- Js.string Info.css_element_hover;
		lwt del = element_delete id in
		lwt cd = Info.can_delete id in
		del##style##display <- Js.string (if cd then "block" else "none");
		Lwt.return ()
	)

	let onmouseout id _ = if_can_edit (fun () ->
		lwt el = element id in
		el##className <- Js.string Info.css_element;
		lwt del = element_delete id in
		del##style##display <- Js.string "none";
		Lwt.return ()
	)

	let remove id =
		lwt lst = Info.get_list () in
		let lst = List.filter ((<>) id) lst in
		Info.set_list lst;
		Lwt.return ()

	let on_delete_click id _ =
		lwt r = Info.try_delete id in
		if r then
		begin
			lwt _ = remove id in
			lwt el = element id in
			Js_common.removeElement el;
			Lwt.return ()
		end
		else
			Lwt.return ()

	let init_id id =
		lwt el = element id in
		el##onmouseover <- Js_common.handler (onmouseover id) Js._true;
		el##onmouseout <- Js_common.handler (onmouseout id) Js._true;
		lwt del = element_delete id in
		del##onclick <- Js_common.handler (on_delete_click id) Js._true;
		Lwt.return ()

	let refresh_element id =
(*
		lwt cd = Info.can_delete id in
		lwt ce = Info.can_edit id in
		lwt del = element_delete id in
		if ce && cd then
			del##style##display <- Js.string "block"
		else
			del##style##display <- Js.string "none";
*)
		Lwt.return ()

	let refresh_all () =
		let f () = 
			lwt lst = Info.get_list () in
			List.iter (Js_common.eignore refresh_element) lst;
			Lwt.return ()
		in
		Js_common.eignore f ()

	let on_login userinfo =
		refresh_all ()
	
	let on_logout () =
		refresh_all ()

	let lwt_init () =
		Js_login.add_on_login on_login;
		Js_login.add_on_logout on_logout;
		lwt lst = Info.get_list () in
		List.iter (Js_common.eignore init_id) lst;
		Lwt.return ()

	let init () = Js_common.eignore lwt_init ()
end
