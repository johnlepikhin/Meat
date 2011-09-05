
module F = struct
	let hash = Js_SHA1.call
end

include Password.F(F)
