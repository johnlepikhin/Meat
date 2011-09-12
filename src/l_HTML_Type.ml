type _attribute =
	| Id of string
	| Class of string

type elname =
	| EDiv
	| EImg of (string * string)
	| ETextArea of (int * int * string)

