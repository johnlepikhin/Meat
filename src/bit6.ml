
let chars = "QWERTYUIOP{}ASDFGHJKL:ZXCVBNM@#$qwertyuiop[]asdfghjkl*zxcvbnm%^-"

let echar = '.'

let encode s =
	let l = String.length s in
	let r = Buffer.create (l*2) in
	let rec drop buf = function
		| 4 -> 0
		| n -> Buffer.add_char r (chars.[buf land 0b111111]); drop (buf lsr 6) (n+1)
	in
	let rec loop buf pos =
		let bpos = pos mod 3 in
		if pos < l then
		begin
			let buf = buf + ((Char.code s.[pos]) lsl (bpos * 8)) in
			if bpos = 2 then
				loop (drop buf 0) (pos+1)
			else
				loop buf (pos+1)
		end
		else
			if bpos > 0 then
			begin
				ignore (drop buf (3-bpos));
				Buffer.add_string r (String.make (3-bpos) echar)
			end
			else ()
	in
	loop 0 0;
	Buffer.contents r

let decode s =
	let l = String.length s in
	let r = Buffer.create l in
	let rec drop buf = function
		| 3 -> 0
		| n ->
			let b = (buf lsr (n*8)) land 0b11111111 in
			Buffer.add_char r (Char.chr b);
			drop buf (n+1)
	in
	let rec loop buf pos =
		if pos = l then
			()
		else
			let c = s.[pos] in
			if c = echar then
				ignore (drop buf 0)
			else
			begin
				let bpos = pos mod 4 in
				let buf = buf lor ((String.index chars c) lsl (bpos*6)) in
				if bpos = 3 then
					loop (drop buf 0) (pos+1)
				else
					loop buf (pos+1)
			end
	in
	loop 0 0;
	Buffer.contents r

let a_to_bit6 s =
	let v = Marshal.to_string s [] in
	encode v

let bit6_to_a s =
	let v = decode s in
	try
		let v = Marshal.from_string v 0 in
		Some v
	with
		| _ -> None
