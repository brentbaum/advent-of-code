module StringMap = Map.Make(String);

let state = StringMap.empty;

let safe_parse = (s) =>
  try {
    ignore(int_of_string(s));
    Int64.of_string(s)
  } {
  | _ => Int64.of_int(0)
  };

let getRegisterValue = (state, register) =>
  if (StringMap.exists((key, _) => key == register, state)) {
    StringMap.find(register, state)
  } else {
    safe_parse(register)
  };

let sound = (state, args, position, _) => {
  let next = getRegisterValue(state, args[0]);
  (state, position + 1, next)
};

let op = (fn, state, args, position, lastHeard) => {
  let register = args[0];
  let register2 = args[1];
  let value = fn(getRegisterValue(state, register), getRegisterValue(state, register2));
  let nextState = StringMap.add(register, value, state);
  (nextState, position + 1, lastHeard)
};

let execIfNonZero = (fn, state, args, position, lastHeard) => {
  let register = args[0];
  if (getRegisterValue(state, register) == Int64.of_int(0)) {
    (state, position + 1, lastHeard)
  } else {
    fn(state, args, position, lastHeard)
  }
};

let recover =
  execIfNonZero(
    (state, args, _, lastHeard) => {
      Js.log(getRegisterValue(state, args[0]));
      (state, (-1), lastHeard)
    }
  );

let jump =
  execIfNonZero(
    (state, args, position, lastHeard) => {
      let jumpAmount = Int64.to_int(safe_parse(args[1]));
      (state, position + jumpAmount, lastHeard)
    }
  );

let process = (lastHeard, instruction, position, state) => {
  let [operation, ...arguments] = instruction |> Js.String.split(" ") |> Array.to_list;
  let fn =
    switch operation {
    | "snd" => sound
    | "set" => op((_, b) => b)
    | "add" => op(Int64.add)
    | "mul" => op(Int64.mul)
    | "mod" => op(Int64.rem)
    | "rcv" => recover
    | "jgz" => jump
    | _ => ((_) => raise(Failure("Unknown instruction")))
    };
  fn(state, arguments |> Array.of_list, position, lastHeard)
};

let execute = (str) => {
  let instructionList = Js.String.split("\n", str);
  let position = ref(0);
  let state = ref(StringMap.empty);
  let lastHeard = ref(Int64.of_int(0));
  while (0 <= position^ && position^ < Array.length(instructionList)) {
    let instruction = instructionList[position^];
    let (nextState, nextPosition, heard) = process(lastHeard^, instruction, position^, state^);
    lastHeard := heard;
    Js.log(
      state^
      |> StringMap.bindings
      |> List.map(((k, v)) => k ++ "-" ++ Int64.to_string(v))
      |> Array.of_list
      |> Js.Array.joinWith(",")
    );
    state := nextState;
    position := nextPosition
  };
  lastHeard^
};

Js.log(execute(Input.input));