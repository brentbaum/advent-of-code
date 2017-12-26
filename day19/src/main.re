let rec findIndex = (~index=0, name, array) =>
  if (array[index] == name) {
    index
  } else {
    findIndex(name, array, ~index=index + 1)
  };

let posOp = (op, (x1, y1), (x2, y2)) => (op(x1, x2), op(y1, y2));

let getInitialPosition = (grid) => (findIndex("|", grid[0]), 0);

let getPosition = (grid, (x, y)) =>
  if (y >= Array.length(grid) - 1 || y < 0) {
    " " /*raise(Failure("X coord out of bounds"))*/
  } else if (x >= Array.length(grid[0]) || x < 0) {
    " " /*raise(Failure("Y coord out of bounds"))*/
  } else {
    grid[y][x]
  };

let resolveJunction = (grid, lastChar, position) =>
  List.find(
    (pos) => getPosition(grid, pos) != " " && getPosition(grid, pos) != lastChar,
    [(0, (-1)), (0, 1), (1, 0), ((-1), 0)] |> List.map(posOp((+), position))
  );

let normalize = (n) => {
  let sign = Js.Math.sign_int(n);
  let a = min(abs(n), 1);
  a * sign
};

let getDirection = (position, lastPosition) => {
  let (d1, d2) = posOp((-), position, lastPosition);
  (normalize(d1), normalize(d2))
};

/* if char at currentPosition + step == currentPosition, recur in current direction.
   If char at currentPosition + step == +, recur in current direction.
    If char at currentPosition + step == opposite(currentPosition), recur in current direction, skip one. */
let followPath = (grid, position, lastPosition) => {
  let direction = getDirection(position, lastPosition);
  let next = posOp((+), position, direction);
  let c = getPosition(grid, position);
  let n = getPosition(grid, next);
  if (c == "|" && n == "-" || c == "-" && n == "|") {
    (2, posOp((+), position, posOp(( * ), direction, (2, 2))))
  } else {
    (1, next)
  }
};

let rec step = (~count=0, grid, position, lastPosition, list) => {
  let currentChar = getPosition(grid, position);
  let lastChar = getPosition(grid, lastPosition);
  let (steps, followed) = followPath(grid, position, lastPosition);
  switch currentChar {
  | " " => (count, list)
  | "+" => step(~count=count + 1, grid, resolveJunction(grid, lastChar, position), position, list)
  | c when c == "|" || c == "-" => step(~count=count + steps, grid, followed, position, list)
  | c => step(~count=count + steps, grid, followed, position, List.append(list, [c]))
  }
};

let splitGrid = (str) => str |> Js.String.split("\n") |> Array.map(Js.String.split(""));

let solve1 = (str) => {
  let grid = splitGrid(str);
  let (initialX, _) = getInitialPosition(grid);
  let (count, occurances) = step(grid, (initialX, 1), (initialX, 0), []);
  Js.log(("Steps", count + 1));
  occurances |> Array.of_list |> Js.Array.joinWith("")
};

let inputStr = Node.Fs.readFileSync("./src/input.txt", `utf8);

Js.log(solve1(inputStr));