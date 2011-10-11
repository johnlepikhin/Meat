
{
	open Fr_grammar
	open Lexing
}

let lident = ['a'-'z']+

rule recipe = parse
	| "recipe" { Recipe }
	| "component" { Component }
	| "step" { Step }
	| '"' {
		let buf = Buffer.create 16380 in
		quoted buf true lexbuf;
		Quoted (Buffer.contents buf)
	}
	| "<<" {
		let buf = Buffer.create 16380 in
		text buf true lexbuf;
		Text (Buffer.contents buf)
	}
	| '=' { Eq }
	| '[' { QLeftBrace }
	| ']' { QRightBrace }
	| '{' { QLeftFigure }
	| '}' { QRightFigure }
	| lident as l { Lident l }
	| '\n'
	| '\r'
	| '\t'
	| ' ' { recipe lexbuf }
	| eof { EOF }

and text buf is_begin = parse
	| ">>" { () }
	| '\r' { text buf is_begin lexbuf }
	| ('\n' | '\t' | ' ') as c {
		if not is_begin then
			Buffer.add_char buf c
		else ();
		text buf is_begin lexbuf
	}
	| _ as c {
		Buffer.add_char buf c;
		text buf false lexbuf
	}

and quoted buf is_begin = parse
	| '"' { () }
	| '\r' { quoted buf is_begin lexbuf }
	| ('\n' | '\t' | ' ') as c {
		if not is_begin then
			Buffer.add_char buf c
		else ();
		quoted buf is_begin lexbuf
	}
	| _ as c {
		Buffer.add_char buf c;
		quoted buf false lexbuf
	}
