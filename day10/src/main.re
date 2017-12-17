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

let countFilled = Array.fold_left((count, row) => count + Array.fold_left((+), 0, row), 0);

let solve1 = () => baseString |> makeMatrix |> countFilled;

module StringSet = Set.Make(String);

module StringMap = Map.Make(String);

let rec findGroup = (~group=StringSet.empty, map, stack) =>
  switch stack {
  | [] => group
  | [element, ...rest] =>
    let list = StringMap.exists((k, _) => k == element, map) ? StringMap.find(element, map) : [];
    if (! StringSet.exists((e) => e == element, group)) {
      findGroup(map, List.append(rest, list), ~group=StringSet.add(element, group))
    } else {
      findGroup(map, rest, ~group)
    }
  };

let rec countGroups = (~count=0, list, map) =>
  switch list {
  | [] => count
  | [head, ...rest] =>
    let group = findGroup(map, [head]);
    let remaining = StringSet.diff(StringSet.of_list(rest), group);
    countGroups(~count=count + 1, StringSet.elements(remaining), map)
  };

let cellId = ((x, y)) => string_of_int(x * 1000 + y);

let getNeighbors = (matrix, x, y) =>
  [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
  |> List.filter(
       ((x, y)) =>
         x > (-1)
         && x < Array.length(matrix)
         && y > (-1)
         && y < Array.length(matrix[0])
         && matrix[x][y] == 1
     )
  |> List.map(cellId);

let generateMappings = (matrix) => {
  /* double loop. Unique index. For each cell, add neighbor above, below, left, and right. Run through day02. */
  let map = ref(StringMap.empty);
  let ids =
    Array.mapi(
      (x, row) =>
        Array.mapi(
          (y, _) => {
            let id = cellId((x, y));
            let neighbors = getNeighbors(matrix, x, y);
            map := StringMap.add(id, neighbors, map^);
            matrix[x][y] == 1 ? id : ""
          },
          row
        )
        |> Array.to_list
        |> List.filter((i) => i != ""),
      matrix
    )
    |> Array.to_list
    |> List.flatten;
  (ids, map^)
};

let solve2 = () => {
  let (ids, map) = baseString |> makeMatrix |> generateMappings;
  countGroups(ids, map)
};

Js.log(solve2());