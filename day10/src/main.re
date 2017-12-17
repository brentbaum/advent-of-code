let baseString = "ljoxqyyw";

let rec pad_left = (str) =>
  if (String.length(str) == 8) {
    str
  } else {
    pad_left("0" ++ str)
  };

let toBinary = (hexStr) =>
  Scanf.sscanf(hexStr, "%x", (x) => x) |> Js.Int.toStringWithRadix(~radix=2) |> pad_left;

let strToBinary = (str) =>
  str |> Ten.make_dense_hash |> Array.map(toBinary) |> Js.Array.joinWith("");

let makeMatrix = (str) =>
  Array.make(128, 0)
  |> Array.mapi((i, a) => str ++ "-" ++ string_of_int(i))
  |> Array.map(strToBinary)
  /* Split by character. Might not need later. */
  |> Array.map((str) => str |> Js.String.split("") |> Array.map(int_of_string));

let countFilled = (str) => {
  let matrix = makeMatrix(str);
  Array.fold_left((count, row) => count + Array.fold_left((+), 0, row), 0, matrix)
};

Js.log(countFilled("ljoxqyyw"));