
module F = struct

	let hash s =
		let s = Cryptokit.hash_string (Cryptokit.Hash.sha1 ()) s in
		Cryptokit.transform_string (Cryptokit.Hexa.encode ()) s
end

include Password.F(F)
