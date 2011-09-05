
let seed : string Eliom_sessions.volatile_table = Eliom_sessions.create_volatile_table ()

let user : string Eliom_sessions.persistent_table = Eliom_sessions.create_persistent_table "user"
