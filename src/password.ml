
module type CRYPT = sig
	val hash: string -> string
end

module F = functor(C: CRYPT) -> struct
	let hash = C.hash

	let encrypt_seed_hash ~seed s =
		let s = s ^ seed in
		hash s

	let encrypt_plain ~seed s =
		let h = hash s in
		encrypt_seed_hash ~seed h
end
