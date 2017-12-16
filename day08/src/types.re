type instruction = {
  register: string,
  operation: (int, int) => int,
  amount: int,
  comparator: (int, int) => bool,
  comp_register: string,
  threshold: int
};