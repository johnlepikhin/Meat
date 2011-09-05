
open Js

module F = struct
	let name = "SHA1"

	type input = string

	type real_returns = js_string t

	type returns = string

	let make_args msg = [| Unsafe.inject (string msg) |]

	let make_ret r = to_string r
end

include Js_fun.ExFUN(F)
