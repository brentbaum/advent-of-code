/* Copied from day 12 */
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