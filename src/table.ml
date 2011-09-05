
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
		Db.use (fun db -> PGSQL(db) "select name, name from property where name=$key")

	let all () =
		Db.use (fun db -> PGSQL(db) "select name, name from property")

	let insert t =
		let name = t.name in
		Db.use (fun db -> PGSQL(db) "insert into property (name) values ($name)")

	let update key t =
		let name = t.name in
		Db.use (fun db -> PGSQL(db) "update property set name=$name where name=$key")

	let delete key =
		Db.use (fun db -> PGSQL(db) "delete from property where name=$key")
end
