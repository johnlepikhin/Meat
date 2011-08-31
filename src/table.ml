
open Db

module Property = struct
	type row = {
		name : string;
	}
	type key = string
	type db_row = key * string

	let of_db (key, name) : key * row =
		let row = {
			name = name;
		} in
		key, row

	let select key =
		PGSQL(h) "select name, name from property where name=$key"

	let all () =
		PGSQL(h) "select name, name from property"

	let insert t =
		let name = t.name in
		PGSQL(h) "insert into property (name) values ($name)"

	let update key t =
		let name = t.name in
		PGSQL(h) "update property set name=$name where name=$key"

	let delete key =
		PGSQL(h) "delete from property where name=$key"
end
