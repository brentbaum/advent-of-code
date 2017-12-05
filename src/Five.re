/* First approach - without pattern matching */
let instructions = FiveIn.str |> Lib.split_on_char('\n') |> List.map(int_of_string);

let increment = (item) => item >= 3 ? item - 1 : item + 1;

let increment_index = (position, list) =>
  List.mapi((index, item) => position === index ? increment(item) : item, list);

let instruction_list = ref(instructions);

let navigate = (list) => {
  let rec step = (index, acc) =>
    if (index < 0 || index > List.length(list^) - 1) {
      acc
    } else {
      list := increment_index(index, list^);
      step(index + List.nth(list^, index), acc + 1)
    };
  step(0, 0)
};

Js.log(navigate(instruction_list));