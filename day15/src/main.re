type state = {
  a: int,
  b: int
};

let start = {a: 634, b: 301};

/* let start = {a: Int64.of_int(65), b: Int64.of_int(8921)};*/
let factors = {a: 16807, b: 48271};

let divisor = 2147483647;

let rec generateNext = (n, factor, divisor, conditionalDivisor) => {
  let next =
    Int64.rem(Int64.mul(Int64.of_int(n), Int64.of_int(factor)), Int64.of_int(divisor))
    |> Int64.to_int;
  if (next mod conditionalDivisor == 0) {
    next
  } else {
    generateNext(next, factor, divisor, conditionalDivisor)
  }
};

let generateNextPair = (state) => {
  let next = {
    a: generateNext(state.a, factors.a, divisor, 4),
    b: generateNext(state.b, factors.b, divisor, 8)
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
  |> Js.Int.toStringWithRadix(~radix=2)
  |> pad_left
  |> ((a) => String.sub(a, String.length(a) - 16, 16));

let match = (pair) => getBinaryTail(pair.a) == getBinaryTail(pair.b);

let pairs = ref(start);

let count = ref(0);

for (x in 0 to 5000000) {
  pairs := generateNextPair(pairs^);
  if (match(pairs^)) {
    Js.log("match!");
    count := count^ + 1
  }
};

Js.log(count^);