type state = {
  a: int64,
  b: int64
};

let start = {a: Int64.of_int(634), b: Int64.of_int(301)};

/* let start = {a: Int64.of_int(65), b: Int64.of_int(8921)};*/
let factors = {a: Int64.of_int(16807), b: Int64.of_int(48271)};

let divisor = Int64.of_int(2147483647);

let generateNext = (state) => {
  let next = {
    a: Int64.rem(Int64.mul(state.a, factors.a), divisor),
    b: Int64.rem(Int64.mul(state.b, factors.b), divisor)
  };
  next
};

let rec pad_left = (str) =>
  if (String.length(str) >= 16) {
    str
  } else {
    pad_left("0" ++ str)
  };

let getBinaryTail = (n) =>
  n
  |> Int64.to_int
  |> Js.Int.toStringWithRadix(~radix=2)
  |> pad_left
  |> ((a) => String.sub(a, String.length(a) - 16, 16));

let match = (pair) => getBinaryTail(pair.a) == getBinaryTail(pair.b);

let pairs = ref(start);

let count = ref(0);

for (x in 0 to 40000000) {
  pairs := generateNext(pairs^);
  if (match(pairs^)) {
    Js.log("match!");
    count := count^ + 1
  }
};

Js.log(count^);