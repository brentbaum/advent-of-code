let getCount = (str) => {
  let cStr = str;
  let count = ref(0);
  let length = String.length(cStr);
  String.iteri(
    (i, c) => {
      let nextIndex = (i + length / 2) mod length;
      let next = cStr.[nextIndex];
      let matches = next == c;
      let n = int_of_string(Char.escaped(c));
      if (matches) {
        count := count^ + n
      }
    },
    cStr
  );
  count
};

Js.log(getCount(Input.input));