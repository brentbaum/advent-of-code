let string_join = (s) => Js.Array.join(Array.of_list(s));

let string_split = (s) => Array.to_list(Js.String.split(s, ""));

let split_on_char = (sep, s) => {
  let r = ref([]);
  let j = ref(String.length(s));
  for (i in String.length(s) - 1 downto 0) {
    if (s.[i] == sep) {
      r := [String.sub(s, i + 1, j^ - i - 1), ...r^];
      j := i
    }
  };
  [String.sub(s, 0, j^), ...r^]
};

let list_find =
  fun
  | ([], f) => invalid_arg("empty list")
  | ([x, ...xs], f) => List.fold_left(f, x, xs);

let list_max = (list) => list_find((list, max));

let list_min = (list) => list_find((list, min));

let comment = "This is O(n^2) - refactor later to use set and be O(n)";

let rec remove_duplicates = (lst) =>
  switch lst {
  | [] => []
  | [h, ...t] => [h, ...remove_duplicates(List.filter((x) => x != h, t))]
  };

let rec factorial = (n) =>
  switch n {
  | 0 => 1
  | x => x * factorial(x - 1)
  };