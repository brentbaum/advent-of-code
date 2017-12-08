module StringMap = Map.Make(String);

let nameWeightRe = [%bs.re "/[^0-9]/"];

let unsafeUnwrap = (option) =>
  switch option {
  | None => raise(Not_found)
  | Some(k) => k
  };

/* Ugly, but it works */
let find_weight = (str) =>
  str
  |> Js.String.split("(")
  |> ((a) => a[1])
  |> Js.String.split(")")
  |> ((a) => a[0])
  |> int_of_string;

let readRow = (row) => {
  let parts = Js.String.split(" -> ", row);
  let parent = Js.String.split(" ", parts[0])[0];
  let weight = find_weight(parts[0]);
  let children = Array.length(parts) > 1 ? Array.to_list(Js.String.split(", ", parts[1])) : [];
  ((parent, weight), children)
};

let rows = Input.input |> Js.String.split("\n") |> Array.to_list |> List.map(readRow);

let nodes = rows |> List.map(((parent, _)) => parent);

let children = rows |> List.map(((_, children)) => children) |> List.flatten;

let top = List.find((((a, _), _)) => ! List.exists((b) => a == b, children), rows);

let find_node = (name) => List.find((((n, _), _)) => name === n, rows);

let rec sum_children = (name) => {
  let ((_, weight), children) = find_node(name);
  switch children {
  | [] => weight
  | list => list |> List.map(sum_children) |> List.fold_left((+), weight)
  }
};

let filter_unique = (fn, list) =>
  List.filter((a) => List.length(List.filter((b) => fn(b, a), list)) == 1, list);

let find_diff = (list) => {
  let pair = List.sort_uniq((a, b) => a == b ? 0 : 1, list);
  List.nth(pair, 0) - List.nth(pair, 1)
};

let rec find_imbalance = (name) => {
  let ((_, _), children) = find_node(name);
  let weights = List.map(sum_children, children);
  let pairs = List.combine(children, weights);
  let uniq = filter_unique(((_, w1), (_, w2)) => w1 == w2, pairs);
  switch uniq {
  | n when List.length(n) === 0 => 0
  | list =>
    let (unique_name, value) = List.nth(uniq, 0);
    /* Recur here, get the imbalance of the children. If it's 0, then return what I was about to return. */
    let imbalance = find_imbalance(unique_name);
    switch imbalance {
    | 0 => find_diff(weights) + value
    | i => i
    }
  }
};

let ((top_name, _), _) = top;

Js.log(find_imbalance(top_name));
/*
 1. Find weights of children
 2. If they're all the same, return 0.
 3. If they're different, find the different one. Get the difference, add it.
 find the unique one.
 2. If they're
 2. Check its children.
 3. If they're all the same, return the difference between the unique one
 */