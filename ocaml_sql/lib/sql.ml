open Core

type query =
  | Select of field list * source
and field =
  | Column of string
  | Subquery_field of query * string
  | Star
and source =
  | Table of string
  | Subquery_source of query * string
  | Join of join
and join =
  | Natural of source * source
  | Inner of source * source * field * field
  | Outer of source * source * field * field
  | Left of source * source * field * field
  | Right of source * source * field * field

let rec query_to_string = function
  | Select (fields, source) ->
    let fields = List.map fields ~f:field_to_string |> String.concat in
    "SELECT " ^ fields ^ " FROM " ^ (source_to_string source)
and field_to_string = function
  | Column column_name -> column_name
  | Subquery_field (query, name) ->
    let query = query_to_string query in
    "(" ^ query ^ ") AS " ^ name
  | Star -> "*"
and source_to_string = function
  | Table table_name -> table_name
  | Subquery_source (query, name) ->
    let query = query_to_string query in
    "(" ^ query ^ ") AS " ^ name
  | Join join -> join_to_string join
and join_to_string join = match join with
  | Natural (source_left, source_right) ->
    let source_left = source_to_string source_left in
    let source_right = source_to_string source_right in
    source_left ^ " NATURAL JOIN " ^ source_right
  | Inner (source_left, source_right, field_left, field_right)
  | Outer (source_left, source_right, field_left, field_right)
  | Left (source_left, source_right, field_left, field_right)
  | Right (source_left, source_right, field_left, field_right) ->
    let source_left = source_to_string source_left in
    let source_right = source_to_string source_right in
    let field_left = field_to_string field_left in
    let field_right = field_to_string field_right in
    let join_string = match join with
      | Inner _ -> " INNER JOIN "
      | Outer _ -> " OUTER JOIN "
      | Left _ -> " LEFT JOIN "
      | Right _ -> " RIGHT JOIN "
      | _ -> assert false
    in
    String.concat [ source_left
      ; join_string
      ; source_right
      ; " ON ("
      ; field_left
      ; " = "
      ; field_right
      ; ")"]

module Sample_queries = struct
  let select_from_table_one = Select ([Star], Table "table1")

  let select_from_table_one_join_table_two =
    Select (
      [Column "table_id"],
      Join (
        Inner (
          (Table "table_1"),
          (Table "table_2"),
          (Column "table_1_id"),
          (Column "table_2_id")
        )
      )
    )

  let select_from_table_one_join_subquery =
    Select (
      [Column "table_id"],
      Join (
        Inner (
          (Table "table_1"),
          Subquery_source (
            Select ([Column "ticket_id"], (
              Table "table_2"
            )), "table_2_id"
          ),
          (Column "table_1_id"),
          (Column "table_2_id")
        )
      )
    )
end