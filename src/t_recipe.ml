

module StepText = struct
	type t = string
end

module Step = struct
	type t = {
		text : StepText.t;
	}

	let make text = {
		text = text;
	}
end

module Ingridient = struct
	type t = {
		name : string;
	}

	let make name = {
		name = name;
	}
end

module Recipe = struct
	type t = {
		name : string;
		steps : Step.t list;
		ingridients : Ingridient.t list;
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
