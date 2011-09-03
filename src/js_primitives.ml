
open Js

exception Request of string
exception API of string
exception Invalid_response

let doc = Dom_html.document
let window = Dom_html.window
let alert e = window##alert (string e)

let errmsg e = "Прозошло что-то непредвиденное. Попробуйте загрузить страницу заново. Если ошибка повторится — дайте знать администрации сайта. Подробности об ошибке:\n\n" ^ e

let fatal e =
	alert (errmsg e);
	raise Exit
