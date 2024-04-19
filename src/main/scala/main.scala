@main def main(): Unit =
  val firstRow = Fraction(-1, 1) :: Fraction(-5, 3) :: Fraction(2, 1) :: Nil
  val secondRow = Fraction(7, 2) :: Fraction(-1, 1) :: Fraction(-1, 2) :: Nil
  val thirdRow = Fraction(-1, 2) :: Fraction(-9, 10) :: Fraction(217, 205) :: Nil
  val matrix = Matrix(firstRow :: secondRow :: thirdRow :: Nil)
  println(matrix)
  matrix.gaussianEliminationBothParts()
  println(matrix)