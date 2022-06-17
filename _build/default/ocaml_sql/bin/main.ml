open Core
open Ocaml_sql.Sql

let () = List.iter ~f:(Fn.compose print_endline query_to_string)
  Sample_queries.[
    select_from_table_one
    ; select_from_table_one_join_table_two
    ; select_from_table_one_join_subquery ]