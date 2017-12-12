let str = "ne,ne,ne";

type coord = {
  x: int,
  y: int,
  z: int
};

let move = ({x, y, z}) =>
  fun
  | "nw" => {x: x - 1, y: y + 1, z}
  | "n" => {x, y: y + 1, z: z - 1}
  | "ne" => {x: x + 1, y, z: z - 1}
  | "sw" => {x: x - 1, y, z: z + 1}
  | "s" => {x, y: y - 1, z: z + 1}
  | "se" => {x: x + 1, y: y - 1, z}
  | _ => raise(Failure("Unmatched input"));

let distance = ({x, y, z}) => max(max(abs(x), abs(y)), abs(z));

let rec travel = (~position={x: 0, y: 0, z: 0}, ~furthest=0, steps) => {
  let further = distance(position) > furthest ? distance(position) : furthest;
  switch steps {
  | [] => (distance(position), furthest)
  | [x, ...xs] => travel(~position=move(position, x), ~furthest=further, xs)
  }
};

let steps = Js.String.split(",", Input.input) |> Array.to_list;

Js.log(travel(steps));