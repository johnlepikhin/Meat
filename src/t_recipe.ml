

module StepText = struct
	type t = string
end

module Step = struct
	type t = {
		text : StepText.t;
	}

	let default text = {
		text = text;
	}
end

module Recipe = struct
	type t = {
		name : string;
		steps : Step.t list;
		(* TODO add ingridients *)
	}
end

module Component = struct
	type t = {
		name : string;
	}
end

type t =
	| Recipe of Recipe.t
	| Component of Component.t
