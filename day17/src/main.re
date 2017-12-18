let state = Array.of_list([0, 2, 1]);

let stepSize = 380;

let max = 50000001;

let rec findIndex = (~index=0, name, array) =>
  if (array[index] == name) {
    index
  } else {
    findIndex(name, array, ~index=index + 1)
  };

let rec spin = (~index=3, ~currentPosition=1, array) => {
  let nextPosition = (currentPosition + stepSize + 1) mod Array.length(array);
  let head = Array.sub(array, 0, nextPosition);
  let tail = Array.sub(array, nextPosition, Array.length(array) - nextPosition);
  let nextArray = Array.append(Array.append(head, Array.of_list([index])), tail);
  if (index == max) {
    array[findIndex(0, array) + 1]
  } else {
    spin(~index=index + 1, ~currentPosition=nextPosition, nextArray)
  }
};

Js.log(spin(state));