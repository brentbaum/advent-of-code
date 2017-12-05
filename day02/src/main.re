let parse_matrix = (str) =>
  str |> String.split("\n") |> List.map(String.split("\t")) |> List.map(List.map(int_of_string));

let list_find =
  fun
  | ([], f) => invalid_arg("empty list")
  | ([x, ...xs], f) => List.fold_left(f, x, xs);

let list_max = (list) => list_find((list, max));

let list_min = (list) => list_find((list, min));

let find_range_pairs = (list) => (Lib.list_min(list), Lib.list_max(list));

let divisor = (a, b) => b mod a == 0 && a != b;

let find_divisor_pairs = (list) => {
  let upper = List.find((a) => List.exists(divisor(a), list), list);
  (upper, List.find(divisor(upper), list))
};

let matrix_pair_sum = (pair_fn, str) =>
  str
  |> parse_matrix
  |> List.map(pair_fn)
  |> List.map(((a, b)) => b - a)
  |> List.fold_left((a, b) => a + b, 0);

Js.log(matrix_pair_sum(find_range_pairs, Input.input));

Js.log(matrix_pair_sum(find_divisor_pairs, Input.input));