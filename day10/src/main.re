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
  |> Array.to_list
  |> List.mapi((i, a) => str ++ "-" ++ string_of_int(i))
  |> List.map(strToBinary)
  /* Split by character. Might not need later. */
  |> List.map((str) => str |> Js.String.split("") |> Array.map(int_of_string) |> Array.to_list);

let countFilled = List.fold_left((count, row) => count + List.fold_left((+), 0, row), 0);

let solve1 = () => baseString |> makeMatrix |> countFilled;

module StringSet = Set.Make(String);

module StringMap = Map.Make(String);

let cellId = ((x, y)) => string_of_int(x * 1000 + y);

let matrixGet = (x, y, matrix) => List.nth(List.nth(matrix, x), y);

let getNeighbors = (matrix, x, y) =>
  [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
  |> List.filter(
       ((x, y)) =>
         x > (-1)
         && x < List.length(matrix)
         && y > (-1)
         && y < List.length(List.nth(matrix, 0))
         && matrixGet(x, y, matrix) == 1
     )
  |> List.map(cellId);

let generateMappings = (matrix) => {
  /* double loop. Unique index. For , matrixeach cell, add neighbor above, below, left, and right. Run through day02. */
  let map = ref(StringMap.empty);
  let ids =
    List.mapi(
      (x, row) =>
        List.mapi(
          (y, _) => {
            let id = cellId((x, y));
            let neighbors = getNeighbors(matrix, x, y);
            map := StringMap.add(id, neighbors, map^);
            matrixGet(x, y, matrix) == 1 ? id : ""
          },
          row
        )
        |> List.filter((i) => i != ""),
      matrix
    )
    |> List.flatten;
  (ids, map^)
};

let solve2 = (str) => {
  let (ids, map) = str |> makeMatrix |> generateMappings;
  Twelve.countGroups(ids, map)
};