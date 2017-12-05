let split_on_char = (sep, s) => {
  let r = ref([]);
  let j = ref(String.length(s));
  for (i in String.length(s) - 1 downto 0) {
    if (s.[i] == sep) {
      r := [String.sub(s, i + 1, j^ - i - 1), ...r^];
      j := i
    }
  };
  [String.sub(s, 0, j^), ...r^]
};