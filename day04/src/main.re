let str = FourIn.str;

let rec remove_dups = (list) =>
  switch list {
  | [] => []
  | [h, ...t] => [h, ...remove_dups(List.filter((x) => x != h, t))]
  };

let word_list = str |> String.split('\n') |> List.map(String.split(' '));

let sort_string = (word) => word |> Js.String.split("") |> Js.Array.sortInPlace |> Array.to_list;

let valid = (list) => list |> remove_dups |> List.length == List.length(list);

let valid_anagrams = (list) => list |> List.map(sort_string) |> valid;

let part_1 = List.filter(valid, word_list);

let part_2 = List.filter(valid_anagrams, word_list);