/* First approach - without pattern matching */
let instructions = FiveIn.str |> Lib.split_on_char('\n') |> List.map(int_of_string);

let increment = (item) => item >= 3 ? item - 1 : item + 1;

let increment_index = (position, list) =>
  List.mapi((index, item) => position === index ? increment(item) : item, list);

let rec navigate = (list, index, acc) =>
  if (index < 0 || index > List.length(list) - 1) {
    acc
  } else {
    navigate(increment_index(index, list), index + List.nth(list, index), acc + 1)
  };

Js.log(navigate(instructions, 0, 0));