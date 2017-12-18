type state = {
  a: int64,
  b: int64
};

let spin = (n, array) => {
  let pivot = Array.length(array) - n;
  let tail = Array.sub(array, pivot, n);
  let head = Array.sub(array, 0, pivot);
  Array.append(tail, head)
};

let exchange = ((x, y), array) => {
  let next = Array.sub(array, 0, Array.length(array));
  next[x] = array[y];
  next[y] = array[x];
  next
};

let rec findIndex = (~index=0, name, array) =>
  if (array[index] == name) {
    index
  } else {
    findIndex(name, array, ~index=index + 1)
  };

let partner = ((n1, n2), array) => {
  let i1 = findIndex(n1, array);
  let i2 = findIndex(n2, array);
  exchange((i1, i2), array)
};

let explode = (s) => {
  let rec exp = (i, l) =>
    if (i < 0) {
      l
    } else {
      exp(i - 1, [String.sub(s, i, 1), ...l])
    };
  exp(String.length(s) - 1, [])
};

let initialPositions = Js.String.split("", "abcdefghijklmnop");

let parseInstruction = (state, instruction) => {
  let instructionType = instruction.[0];
  let parameters = String.sub(instruction, 1, String.length(instruction) - 1);
  switch instructionType {
  | 's' => spin(Parse.parseSpin(parameters), state)
  | 'x' => exchange(Parse.parseExchange(parameters), state)
  | 'p' => partner(Parse.parsePartner(parameters), state)
  | _ => raise(Failure("Parse parameter"))
  }
};

let dance = (danceStr, positions) =>
  danceStr |> Js.String.split(",") |> Array.fold_left(parseInstruction, positions);

module StringMap = Map.Make(String);

let rec findCycleIndex = (danceStr, positions, ~index=0, set) => {
  let nextPositions = dance(danceStr, positions);
  let positionStr = Js.Array.joinWith("", nextPositions);
  if (StringMap.exists((s, _) => s == positionStr, set)) {
    index
  } else {
    findCycleIndex(
      danceStr,
      nextPositions,
      StringMap.add(positionStr, index, set),
      ~index=index + 1
    )
  }
};

let solve2 = (danceStr) => {
  let positions = ref(initialPositions);
  let cycleIndex = findCycleIndex(danceStr, initialPositions, StringMap.empty);
  for (_ in 0 to 1000000000 mod cycleIndex - 1) {
    positions := dance(danceStr, positions^)
  };
  positions^ |> Js.Array.joinWith("")
};