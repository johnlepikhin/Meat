open Js_common
open Js

module KeyEvent = struct
   type key = int list
	type f = Dom_html.keyboardEvent Js.t -> unit Lwt.t
	type v = f * (bool Js.t)

   let enter = 13
   let shift = 16
   let ctrl = 17
   let alt = 18
   let escape = 27
   let space = 32
   let left = 37
   let right = 39
   let up = 38
   let down = 40
   let home = 36
   let _end = 35

   let pressed = ref []

   let storage = ref []

   let actions : (key, v) Hashtbl.t ref = ref (Hashtbl.create 101)

   let get_pressed () =
      List.map (fun (k, _) -> k) !pressed

   let reset_pressed () =
      pressed := []

   let is_pressed k =
      try
         ignore (List.find (fun (code, _) -> code = k) !pressed);
         true
      with
         | _ -> false

   let akey = List.sort compare

   let process_event e =
      try
         let k = akey (get_pressed ()) in
         let (f, ret) = Hashtbl.find !actions k in
         ignore (f e);
			ret
      with
         | _ -> Js._true

   let init () =
		lwt body = body () in
      window##onblur <- Dom_html.handler (fun _ -> reset_pressed (); Js._true);
      body##onkeydown <- Dom_html.handler (fun e ->
         let code = e##keyCode in
         let id = Int64.to_string (Random.int64 Int64.max_int) in
         pressed := (code, id) :: !pressed;
         let rec repeat _ =
            try
               ignore (List.find (fun (k, i) -> i = id && k = code) !pressed);
               ignore (process_event e);
               lwt _ = Lwt_js.sleep 0.1 in
               repeat ()
            with
               | _ -> Lwt.return ()
         in
         if code <> shift && code <> alt && code <> ctrl then
            ignore (
               lwt _ = Lwt_js.sleep 0.2 in
               repeat ()
            )
         else
            ();
         process_event e;
      );
      body##onkeyup <- Dom_html.handler (fun e ->
         let code = e##keyCode in
         pressed := List.filter (fun (k, _) -> k <> code) !pressed;
         process_event e);
		Lwt.return ()

   let push keys f ret =
      let k = akey keys in
      Hashtbl.add !actions k (f, ret)

   let pop keys =
      let k = akey keys in
      Hashtbl.remove !actions k

   let backup () =
      storage := !actions :: !storage;
      actions := Hashtbl.create 101

   let restore () =
      match !storage with
         | [] -> actions := Hashtbl.create 101;
         | hd :: tl ->
            actions := hd;
            storage := tl
end

module Hint = struct
	let show msg =
		alert msg;
		Lwt.return ()
end
