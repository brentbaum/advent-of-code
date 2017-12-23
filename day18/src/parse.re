/* Hello */
let parseSpin = (parameters) => int_of_string(parameters);

let parseExchange = (parameters) => {
  let [s1, s2] = parameters |> Js.String.split("/") |> Array.to_list;
  (int_of_string(s1), int_of_string(s2))
};

let parsePartner = (parameters) => {
  let [s1, s2] = parameters |> Js.String.split("/") |> Array.to_list;
  (s1, s2)
};