open Printf
open Ocamlbuild_plugin

let ocsigen = "ocsigen.opt"
let ocsigen_modules_path = "ocsigen/modules"
let ocsigen_js_path = "ocsigen/js"
let ocsigen_css_path = "ocsigen/css"

let targets = [
	("eliom_no_meat.cmxs", Some ocsigen_modules_path);
	("ocsipersist.cmxs", Some ocsigen_modules_path);
	("js_search.js", Some ocsigen_js_path);
	("sha.js", Some ocsigen_js_path);
	("css_main.css", Some ocsigen_css_path);
]

let pp_opts = [ "-DEBUG" ]

(* +-----------------------------------------------------------------+
   | Ocamlfind                                                       |
   +-----------------------------------------------------------------+ *)

(* these functions are not really officially exported *)
let run_and_read = Ocamlbuild_pack.My_unix.run_and_read
let blank_sep_strings = Ocamlbuild_pack.Lexers.blank_sep_strings

let split s ch =
	let x = ref [] in
	let rec go s =
	let pos = String.index s ch in
		x := (String.before s pos)::!x;
		go (String.after s (pos + 1))
	in
	try
		go s
	with Not_found -> !x

let split_nl s = split s '\n'

	let before_space s =
	try
		String.before s (String.index s ' ')
	with Not_found -> s

(* this lists all supported packages *)
let installed_packages () = List.map before_space (split_nl & run_and_read "ocamlfind list")

(* List of syntaxes: *)
let syntaxes = [ "camlp4o"; "camlp4r"; "camlp5o"; "camlp5r" ]

let flag_all_except_link tag f =
	flag ["ocaml"; "compile"; tag] f;
	flag ["ocaml"; "ocamldep"; tag] f;
	flag ["ocaml"; "doc"; tag] f

let flag_all tag f =
	flag_all_except_link tag f;
	flag ["ocaml"; "link"; tag] f

let _ =
	let ocamlfind x = S[A"ocamlfind"; A x] in
	Options.ocamlc   := ocamlfind "ocamlc";
	Options.ocamlopt := ocamlfind "ocamlopt";
	Options.ocamldep := ocamlfind "ocamldep";
	Options.ocamldoc := ocamlfind "ocamldoc"

let empty_action = fun _ _ -> Cmd (S[])

let rule_install fname path =
	rule (fname ^ ".install") ~prod:(fname ^ ".install") ~deps:[fname]
		(fun _ _ -> Cmd (S[A"cp"; A fname; A("../../stuff/" ^ path ^ "/")]))

let rule_cmxs () =
	rule "build shared module" ~prod:"%.cmxs" ~dep:"%.cmxa"
		(fun env build ->
			let tags = Tags.union
				(tags_of_pathname (env "%.cmxs"))
				(tags_of_pathname (env "%.cmxa"))
					++ "ocaml" ++ "link" ++ "module"
			in
			Cmd(S[!Options.ocamlopt; A"-shared"; A"-linkall";
			T tags; A"-o"; P(env "%.cmxs"); P(env "%.cmxa")]))

let _ =
	dispatch begin function
		| Before_rules ->
			rule_cmxs ();
		| After_rules ->
			flag ["ocaml"; "link"; "program"] & A"-linkpkg";
(*			flag ["ocaml"; "compile"] (S[A "-thread"]); *)
			flag ["ocaml"; "pkg_threads"; "compile"] (S[A "-thread"]);
			flag ["ocaml"; "pkg_threads"; "link"] (S[A "-thread"]);
			flag ["ocaml"; "pkg_threads"; "infer_interface"] (S[A "-thread"]);

			List.iter
				(fun package -> flag_all ("pkg_" ^ package) (S[A"-package"; A package]))
				(installed_packages ());

			List.iter
				(fun syntax -> flag_all_except_link ("syntax_" ^ syntax) (S[A"-syntax"; A syntax]))
				syntaxes;

         List.iter
            (fun opt -> flag_all_except_link ("ppopt" ^ opt) (S[A"-ppopt"; A opt]))
            pp_opts;

			rule "clear_shared" ~prod:"clear_shared" ~deps:[]
				(fun _ _ -> Cmd (S[A"sh"; A"-c"; A"rm *.cmxs >/dev/null 2>&1; true"]));

			rule "clear_js" ~prod:"%.js.clear" ~deps:[] (fun env _build ->
				let js = env "%.js" in
				Cmd (S[A"sh"; A"-c"; A("rm \"" ^ js ^ "\" >/dev/null 2>&1; true")])
			);

			let install_deps = ref [] in
			List.iter (fun (fname, inst_dir) -> match inst_dir with
					| None -> ()
					| Some inst_dir ->
						rule_install fname inst_dir;
						install_deps := (fname ^ ".install") :: !install_deps;
			) targets;

			rule "Compile JS" ~prod:"%.js" ~deps:["%.js.clear"; "%.byte"] (fun env _build ->
				let byte = env "%.byte" in
				Cmd (S[A"js_of_ocaml"; P(byte)])
			);

			rule "Compile CSS" ~prod:"%.css" ~deps:["%_print.native"] (fun env _build ->
				let exec = env "%_print.native" in
				let out = env "%.css" in
				Cmd (S[A"sh"; A"-c"; A("./" ^ exec ^ ">" ^ out)])
			);

			rule "install" ~prod:"install" ~deps:("clear_shared" :: !install_deps) empty_action;

			rule "start" ~prod:"start" ~deps:[]
				(fun _ _ -> Cmd (S[A"sh"; A"-c"; A("cd ../../stuff/; " ^ ocsigen ^ " -c ocsigen/ocsigen.conf -d")]));

			rule "stop" ~prod:"stop" ~deps:[]
				(fun _ _ -> Cmd (S[A"sh"; A"-c"; A("killall " ^ ocsigen ^ " >/dev/null 2>&1; true")]));

			rule "restart" ~prod:"restart" ~deps:["stop"; "start"] empty_action;

			rule "Install & restart" ~prod:"IR" ~deps:["install"; "restart"] empty_action;

			rule "Dump DB structure" ~prod:"dump"
				(fun _ _ -> Cmd (S[A"sh"; A"-c"; A("pg_dump -s -Fc >../db_struct")]));

			rule "Restore DB structure" ~prod:"restore"
				(fun _ _ -> Cmd (S[A"sh"; A"-c"; A("pg_restore -s -c --no-acl -1 -U \"$PGUSER\" -d \"$PGDATABASE\" ../db_struct")]));
		| _ -> ()
	end
