open Types;

let getOperation = (opStr) =>
  switch opStr {
  | "inc" => (+)
  | "dec" => (-)
  | _ => raise(Failure("Failed to parse operator"))
  };

let getComparator = (compStr) =>
  switch compStr {
  | ">=" => (>=)
  | ">" => (>)
  | "==" => (==)
  | "!=" => (!=)
  | "<" => (<)
  | "<=" => (<=)
  | _ => raise(Failure("Failed to parse comparator"))
  };

let parseInstruction = (instructionStr) : instruction => {
  let [instruction, condition] = instructionStr |> Js.String.split(" if ") |> Array.to_list;
  let [register, operationStr, amountStr] = instruction |> Js.String.split(" ") |> Array.to_list;
  let [comp_register, comparatorStr, thresholdStr] =
    condition |> Js.String.split(" ") |> Array.to_list;
  {
    register,
    operation: getOperation(operationStr),
    amount: int_of_string(amountStr),
    comparator: getComparator(comparatorStr),
    comp_register,
    threshold: int_of_string(thresholdStr)
  }
};

let parseInstructionList = (str) =>
  str |> Js.String.split("\n") |> Array.map(parseInstruction) |> Array.to_list;