module StringSet = Set.Make(String);

let rec find_index = (a, ~n=0, x) =>
  if (a[n] == x) {
    n
  } else {
    find_index(a, x, ~n=n + 1)
  };

let select_bank = (list) => Array.fold_left(max, (-1), list) |> find_index(list);

let inc_bank = (position, list) => {
  list[position] = list[position] + 1;
  list
};

let stringify = (list) => String.concat(" ", Array.to_list(Array.map(string_of_int, list)));

let redistribute_bank = (banks) => {
  let maxIndex = select_bank(banks);
  let blockCount = banks[maxIndex];
  let incIndex = (index) => (index + 1) mod Array.length(banks);
  let rec step = (~index=incIndex(maxIndex), ~blocks=blockCount - 1, bankList) => {
    let next_bank_list = inc_bank(index, bankList);
    switch (next_bank_list, blocks) {
    | (list, b) when b == 0 => list
    | _ => step(next_bank_list, ~blocks=blocks - 1, ~index=incIndex(index))
    }
  };
  banks[maxIndex] = 0;
  step(banks)
};

let store_banks = (state_map, banks) => {
  let next_bank_str = stringify(banks);
  let repeat = StringSet.exists((s) => s == next_bank_str, state_map^);
  let next_state_set = StringSet.add(next_bank_str, state_map^);
  state_map := next_state_set;
  repeat
};

let rec solve = (state_map, ~iterations=0, banks) => {
  let redistributed_banks = redistribute_bank(banks);
  let repeat = store_banks(state_map, banks);
  repeat ?
    (iterations + 1, redistributed_banks) :
    solve(state_map, redistributed_banks, ~iterations=iterations + 1)
};

let state_map = ref(StringSet.of_list([]));

let (first, solution) = solve(state_map, Array.of_list(Input.list));

Js.log("Part one: " ++ string_of_int(first));

let state_map = ref(StringSet.of_list([]));

let (second, _) = solve(state_map, solution);

Js.log("Part one: " ++ string_of_int(second));