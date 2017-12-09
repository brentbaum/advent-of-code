let rec step = (string, stack, level, score, garbage) =>
  switch (string, stack) {
  | ([], _) => (score, garbage)
  | ([_, ...restString], ['!', ...restStack]) => step(restString, restStack, level, score, garbage)
  | (['!', ...restString], stack) =>
    step(restString, List.append(['!'], stack), level, score, garbage)
  | (['>', ...restString], ['<', ...restStack]) =>
    step(restString, restStack, level, score, garbage)
  | ([_, ...restString], ['<', ...restStack]) => step(restString, stack, level, score, garbage + 1)
  | (['}', ...restString], ['{', ...restStack]) =>
    step(restString, restStack, level - 1, score + level, garbage)
  | (['<', ...restString], stack) =>
    step(restString, List.append(['<'], stack), level, score, garbage)
  | (['{', ...restString], stack) =>
    step(restString, List.append(['{'], stack), level + 1, score, garbage)
  | ([_, ...restString], stack) => step(restString, stack, level, score, garbage)
  | _ => raise(Failure("Unmatched case"))
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

Js.log(step(explode(Input.input), [], 0, 0, 0));