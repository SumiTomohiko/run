
type table = (int * int * int) array
type t = {
  path: string;
  name: string;
  lineno_table: table;
  exception_table: table;
  ops: int Op.kind array }

let rec find tbl target index not_found =
  if (Array.length tbl) = index then
    not_found
  else
    let front, rear, datum = Array.get tbl index in
    if (front <= target) && (target < rear) then
      datum
    else
      find tbl target (index + 1) not_found

let ops_of_code { ops } = ops
let path_of_code { path } = path
let name_of_code { name } = name
let lineno_of_pc { lineno_table } pc = find lineno_table pc 0 (-1)
let dest_of_exception { exception_table } pc = find exception_table pc 0 (-1)
let make path name lineno_table exception_table ops = {
  path=path;
  name=name;
  lineno_table=lineno_table;
  exception_table=exception_table;
  ops=ops }

let printf = Printf.printf

let rec print_exception_table tbl index =
  if index = (Array.length tbl) then
    ()
  else
    let front, rear, dest = (Array.get tbl index) in
    printf "  from=%d, end=%d, dest=%d\n" front rear dest;
    print_exception_table tbl (index + 1)

let rec print_ops ops index =
  if index = (Array.length ops) then
    ()
  else begin
    printf "  %d: %s\n" index (Op.name_of_op (Array.get ops index));
    print_ops ops (index + 1)
  end

let dump { path; name; exception_table; ops } =
  printf "path: %s\n" path;
  printf "name: %s\n" name;
  printf "exception table:\n";
  print_exception_table exception_table 0;
  printf "operations:\n";
  print_ops ops 0

(**
 * vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2
 *)
