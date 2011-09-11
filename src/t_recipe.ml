

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

type t = {
	title : string;
	steps : Step.t list;
}
