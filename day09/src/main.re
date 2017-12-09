let rec step = (string, stack, level, score) =>
  switch (string, stack) {
  | ([], _) => score
  | ([_, ...restString], ['!', ...restStack]) => step(restString, restStack, level, score)
  | (['!', ...restString], stack) => step(restString, List.append(['!'], stack), level, score)
  | (['>', ...restString], ['<', ...restStack]) => step(restString, restStack, level, score)
  | ([_, ...restString], ['<', ...restStack]) => step(restString, stack, level, score)
  | (['}', ...restString], ['{', ...restStack]) =>
    step(restString, restStack, level - 1, score + level)
  | (['<', ...restString], stack) => step(restString, List.append(['<'], stack), level, score)
  | (['{', ...restString], stack) => step(restString, List.append(['{'], stack), level + 1, score)
  | ([_, ...restString], stack) => step(restString, stack, level, score)
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

Js.log(step(explode(Input.input), [], 0, 0));