/* Loosely based off a reddit solution because I wasn't motivated to solve this one */
type state = {
  array: array(int),
  position: int,
  skip_size: int
};

let swap = (array, j, k) => {
  let temp = array[j];
  array[j] = array[k];
  array[k] = temp
};

let reverse_slice = (array, start, len) => {
  let length = Array.length(array);
  for (i in 0 to len / 2 - 1) {
    let j = (start + i) mod length;
    let k = (start + len - 1 - i) mod length;
    swap(array, j, k)
  }
};

let knot_hash = (state, length) => {
  reverse_slice(state.array, state.position, length);
  let array_length = Array.length(state.array);
  let skip = state.skip_size;
  {
    array: state.array,
    position: (state.position + length + skip) mod array_length,
    skip_size: skip + 1
  }
};

let list = [0, 1, 2, 3, 4];

let hash = (state, lengthArray) =>
  Array.fold_left((state, length) => knot_hash(state, length), state, lengthArray);

let init = (length) => Array.mapi((i, a) => i, Array.make(length, 0));

let solve1 = (str) => {
  let lengths = str |> Js.String.split(",") |> Array.map(int_of_string);
  let array = init(256);
  let state = {position: 0, array, skip_size: 0};
  let hashed = hash(state, lengths).array;
  hashed[0] * hashed[1]
};

let explode = (s) => {
  let rec exp = (i, l) =>
    if (i < 0) {
      l
    } else {
      exp(i - 1, [s.[i], ...l])
    };
  exp(String.length(s) - 1, [])
};

let rec make_dense_array = (~dense=Array.make(0, 0), sparse) =>
  if (Array.length(sparse) == 0) {
    dense
  } else {
    let head = Array.sub(sparse, 0, 16);
    let rest = Array.sub(sparse, 16, Array.length(sparse) - 16);
    let item = Array.fold_left((lxor), 0, head);
    make_dense_array(rest, ~dense=Array.append(dense, Array.of_list([item])))
  };

/*
 1. split string by character
 2. convert characters to ints.
 3. append 17, 31, 73, 47, 23.
 4. fold_left hash 64 times. This is the sparse hash.
 5. For each block of 16 numbers, numeric bitwise XOR them together. This is the dense hash
 6. For each of remaining 16 elements, convert to hexidecimal. Join. This is result.
 */
let make_dense_hash = (str) => {
  let codeList = str |> explode |> List.map(int_of_char);
  let array = init(256);
  let lengthArray = List.append(codeList, [17, 31, 73, 47, 23]) |> Array.of_list;
  let state = {position: 0, array, skip_size: 0};
  let runs = Array.to_list(Array.make(64, 0));
  let sparse_hash = List.fold_left((s, _) => hash(s, lengthArray), state, runs).array;
  let dense_hash = make_dense_array(sparse_hash);
  Js.Array.join(Array.map(Printf.sprintf("%X"), dense_hash))
};

Js.log(make_dense_hash(Input.input));