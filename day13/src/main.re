let str = "0 <-> 2\n1 <-> 1\n2 <-> 0, 3, 4\n3 <-> 2, 4\n4 <-> 2, 3, 6\n5 <-> 6\n6 <-> 4, 5";

module StringSet = Set.Make(String);

module StringMap = Map.Make(String);

let parseMappings = (str) =>
  str
  |> Js.String.split("\n")
  |> Array.to_list
  |> List.map(
       (line) => {
         let [key, values] = Js.String.split(" <-> ", line) |> Array.to_list;
         let parsedValues = values |> Js.String.split(", ") |> Array.to_list;
         (key, parsedValues)
       }
     )
  |> List.fold_left((set, (k, vs)) => StringMap.add(k, vs, set), StringMap.empty);

let rec findGroup = (~group=StringSet.empty, map, stack) =>
  switch stack {
  | [] => group
  | [element, ...rest] =>
    let list = StringMap.find(element, map);
    if (! StringSet.exists((e) => e == element, group)) {
      findGroup(map, List.append(rest, list), ~group=StringSet.add(element, group))
    } else {
      findGroup(map, rest, ~group)
    }
  };

let solve1 = (str) => {
  let map = parseMappings(str);
  let group = findGroup(map, ["0"]);
  List.length(StringSet.elements(group))
};

let rec countGroups = (~count=0, map, list) =>
  switch list {
  | [] => count
  | [head, ...rest] =>
    let group = findGroup(map, [head]);
    let remaining = StringSet.diff(StringSet.of_list(rest), group);
    countGroups(~count=count + 1, map, StringSet.elements(remaining))
  };

let solve2 = (str) => {
  let map = parseMappings(str);
  let keyList = map |> StringMap.bindings |> List.map(((k, v)) => k);
  countGroups(map, keyList)
};

Js.log(solve1(Input.input));

Js.log(solve2(Input.input));