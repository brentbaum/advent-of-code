/* First approach - without pattern matching */
let instructions = FiveIn.str |> String.split("\n") |> List.map(int_of_string) |> Array.of_list;

let touch = (item) => item >= 3 ? item - 1 : item + 1;

let touch_list = (position, list) => {
  list[position] = touch(list[position]);
  list
};

let rec navigate = (list, index, acc) =>
  switch index {
  | n when n < 0 || n > Array.length(list) - 1 => acc
  | n => navigate(touch_list(n, list), n + list[n], acc + 1)
  };

Js.log(navigate(instructions, 0, 0));