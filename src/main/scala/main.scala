@main def main(): Unit =
  println(Fraction(-4, 2) <= Fraction(-2, 1))
  val firstRow = List(Fraction(2, 1), Fraction(-1, 1), Fraction(0, 1))
  val secondRow = List(Fraction(-1, 1), Fraction(2, 1), Fraction(-1, 1))
  val thirdRow = List(Fraction(0, 1), Fraction(-1, 1), Fraction(2, 1))
  val startList = List(firstRow, secondRow, thirdRow)
  val testMatrix = Matrix(startList)
  println(testMatrix)

  val operations = testMatrix.gaussianEliminationBothParts()
  println(testMatrix)
  println(operations)

  val firstRowOfUnitMatrix = List(Fraction(1, 1), Fraction(0, 1), Fraction(0, 1))
  val secondRowOfUnitMatrix = List(Fraction(0, 1), Fraction(1, 1), Fraction(0, 1))
  val thirdRowOfUnitMatrix = List(Fraction(0, 1), Fraction(0, 1), Fraction(1, 1))
  val unitMatrix = Matrix(List(firstRowOfUnitMatrix, secondRowOfUnitMatrix, thirdRowOfUnitMatrix))
  unitMatrix.applyGivenOperations(operations)
  println(unitMatrix)