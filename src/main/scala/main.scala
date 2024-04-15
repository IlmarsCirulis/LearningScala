@main def main(): Unit =
  val firstRow = Fraction(2, 1) :: Fraction(-1, 1) :: Fraction(0, 1) :: Nil
  val secondRow = Fraction(-1, 1) :: Fraction(2, 1) :: Fraction(-1, 1) :: Nil
  val thirdRow = Fraction(0, 1) :: Fraction(-1, 1) :: Fraction(2, 1) :: Nil
  val matrixA = Matrix(firstRow :: secondRow :: thirdRow :: Nil)
  println(matrixA)

  val operations = matrixA.gaussianEliminationBothParts()
  println(matrixA)
  println(operations)

  val matrixB = Matrix(3)
  matrixB.applyGivenOperations(operations)
  println(matrixB)
  
  println(Matrix(firstRow :: secondRow :: thirdRow :: Nil).multiply(matrixB))