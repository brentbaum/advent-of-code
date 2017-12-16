module StringMap = Map.Make(String);

let clearRegisters = (instructions) =>
  instructions
  |> List.map((a: Types.instruction) => a.register)
  |> List.fold_left((map, elm) => StringMap.add(elm, 0, map), StringMap.empty);

let maxRegisterValue = ref((-10000000));

let evaluateInstruction = (registers, instruction: Types.instruction) => {
  /* we don't guard with mem here because it _should_ be available */
  /* Better name for target? What are parts of conditional? */
  let targetValue = StringMap.find(instruction.comp_register, registers);
  let registerValue = StringMap.find(instruction.register, registers);
  let satisfiesConditional = instruction.comparator(targetValue, instruction.threshold);
  let nextValue =
    satisfiesConditional ?
      instruction.operation(registerValue, instruction.amount) : registerValue;
  if (nextValue > maxRegisterValue^) {
    maxRegisterValue := nextValue
  };
  nextValue
};

let evaluateInstructionList =
  List.fold_left(
    (map, instruction: Types.instruction) =>
      StringMap.add(instruction.register, evaluateInstruction(map, instruction), map)
  );

let mapMaxValue = (stringMap) =>
  stringMap |> StringMap.bindings |> List.map(((a, b)) => b) |> List.fold_left(max, (-100000));

let solve = (instructionStr) => {
  let instructions = Parse.parseInstructionList(Input.input);
  let registers = clearRegisters(instructions);
  let result = evaluateInstructionList(registers, instructions);
  mapMaxValue(result)
};

Js.log((solve(Input.input), maxRegisterValue^));

let stack = [];