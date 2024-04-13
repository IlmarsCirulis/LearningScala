enum ElementaryRowOperation:
  case swap(firstRow: Int, secondRow: Int)
  case addition(row: Int, rowToAdd: Int)
  case multiplication(row: Int, multiplier: Fraction)
  case additionOfMultipliedRow(row: Int, rowToAdd: Int, multiplier: Fraction)