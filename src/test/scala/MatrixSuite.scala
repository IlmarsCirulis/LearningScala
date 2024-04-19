import org.scalatest.funsuite.AnyFunSuite

class MatrixSuite extends AnyFunSuite {

  /* Testing main constructor for Matrix */

  test("Check if jagged list in Matrix constructor causes exception") {
    val firstRow = Fraction(1, 2) :: Fraction(3, 4) :: Nil
    val secondRow = Fraction(5, 6) :: Nil
    val exception = intercept[IllegalArgumentException](Matrix(firstRow :: secondRow :: Nil))
    assert(exception.getMessage.equals("requirement failed: Jagged list isn't accepted"))
  }

  test("Check if empty list in Matrix constructor causes exception") {
    val exception = intercept[IllegalArgumentException](Matrix(Nil))
    assert(exception.getMessage.equals("requirement failed: Empty list isn't accepted"))
  }

  test("Check if list of empty lists in Matrix constructor causes exception") {
    val firstRow = Nil
    val secondRow = Nil
    val exception = intercept[IllegalArgumentException](Matrix(firstRow :: secondRow :: Nil))
    assert(exception.getMessage.equals("requirement failed: Rows must be nonempty"))
  }

  test("Test another constructor, that accepts function and dimensions") {
    val firstRow = Fraction(1, 1) :: Fraction(0, 1) :: Fraction(0, 1) :: Nil
    val secondRow = Fraction(1, 1) :: Fraction(1, 1) :: Fraction(0, 1) :: Nil
    val thirdRow = Fraction(1, 1) :: Fraction(1, 1) :: Fraction(1, 1) :: Nil
    val firstMatrix = Matrix(firstRow :: secondRow :: thirdRow :: Nil)
    val fun = (row: Int) => (col: Int) => if (col <= row) Fraction(1, 1) else Fraction(0, 1)
    val secondMatrix = Matrix(fun, 3, 3)
    assert(firstMatrix == secondMatrix)
  }

  test("Test constructor that takes size and makes unit matrix of that size") {
    val firstRow = Fraction(1, 1) :: Fraction(0, 1) :: Fraction(0, 1) :: Nil
    val secondRow = Fraction(0, 1) :: Fraction(1, 1) :: Fraction(0, 1) :: Nil
    val thirdRow = Fraction(0, 1) :: Fraction(0, 1) :: Fraction(1, 1) :: Nil
    val firstMatrix = Matrix(firstRow :: secondRow :: thirdRow :: Nil)
    val secondMatrix = Matrix(3)
    assert(firstMatrix == secondMatrix)
  }

  /* Testing mathod "isSquareMatrix" */

  test("Given square matrix, isSquareMatrix returns true") {
    val firstRow = Fraction(1, 7) :: Fraction(5, 3) :: Fraction(1, 4) :: Nil
    val secondRow = Fraction(1, 2) :: Fraction(3, 2) :: Fraction(1, 3) :: Nil
    val thirdRow = Fraction(3, 1) :: Fraction(4, 1) :: Fraction(3, 10) :: Nil
    val testMatrix = Matrix(firstRow :: secondRow :: thirdRow :: Nil)
    assert(testMatrix.isSquareMatrix)
  }

  test("Given nonsquare matrix, isSquareMatrix returns false") {
    val firstRow = Fraction(1, 7) :: Fraction(5, 3) :: Fraction(1, 4) :: Fraction(5, 2) :: Nil
    val secondRow = Fraction(1, 2) :: Fraction(3, 2) :: Fraction(1, 3) :: Fraction(3, 4) :: Nil
    val thirdRow = Fraction(3, 1) :: Fraction(4, 1) :: Fraction(3, 10) :: Fraction(2, 3) :: Nil
    val testMatrix = Matrix(firstRow :: secondRow :: thirdRow :: Nil)
    assert(!testMatrix.isSquareMatrix)
  }

  /* Testing method "transpose" */

  test("Transposition of square matrix") {
    val firstRow = Fraction(1, 1) :: Fraction(5, 4) :: Fraction(7, 2) :: Nil
    val secondRow = Fraction(5, 4) :: Fraction(9, 5) :: Fraction(1, 5) :: Nil
    val thirdRow = Fraction(1, 2) :: Fraction(9, 8) :: Fraction(3, 1) :: Nil
    val firstMatrix = Matrix(firstRow :: secondRow :: thirdRow :: Nil)
    val firstColumn = Fraction(1, 1) :: Fraction(5, 4) :: Fraction(1, 2) :: Nil
    val secondColumn = Fraction(5, 4) :: Fraction(9, 5) :: Fraction(9, 8) :: Nil
    val thirdColumn = Fraction(7, 2) :: Fraction(1, 5) :: Fraction(3, 1) :: Nil
    val secondMatrix = Matrix(firstColumn :: secondColumn :: thirdColumn :: Nil)
    assert(firstMatrix.transpose == secondMatrix)
  }

  test("Transposition of non-square matrix") {
    val firstRow = Fraction(1, 1) :: Fraction(5, 4) :: Fraction(7, 2) :: Nil
    val firstMatrix = Matrix(firstRow :: Nil)
    val firstColumn = Fraction(1, 1) :: Nil
    val secondColumn = Fraction(5, 4) :: Nil
    val thirdColumn = Fraction(7, 2) :: Nil
    val secondMatrix = Matrix(firstColumn :: secondColumn :: thirdColumn :: Nil)
    assert(firstMatrix.transpose == secondMatrix)
  }

  /* Testing method "opposite" */

  test("Opposite of matrix has opposite elements") {
    val firstRow = Fraction(-6, 7) :: Fraction(-7, 3) :: Nil
    val secondRow = Fraction(1, 1) :: Fraction(-2, 5) :: Nil
    val firstMatrix = Matrix(firstRow :: secondRow :: Nil)
    val firstRowOfOppositeMatrix = Fraction(6, 7) :: Fraction(7, 3) :: Nil
    val secondRowOfOppositeMatrix = Fraction(-1, 1) :: Fraction(2, 5) :: Nil
    val secondMatrix = Matrix(firstRowOfOppositeMatrix :: secondRowOfOppositeMatrix :: Nil)
    assert(firstMatrix.opposite == secondMatrix)
  }

  /* Testing method "add" */

  test("Sum of two 2x2 matrices") {
    val firstMatrix = Matrix((Fraction(1, 1) :: Fraction(-5, 2) :: Nil) :: (Fraction(7, 8) :: Fraction(1, 5) :: Nil) :: Nil)
    val secondMatrix = Matrix((Fraction(5, 7) :: Fraction(1, 2) :: Nil) :: (Fraction(2, 7) :: Fraction(-3, 1) :: Nil) :: Nil)
    val sumMatrix = Matrix((Fraction(12, 7) :: Fraction(-2, 1) :: Nil) :: (Fraction(65, 56) :: Fraction(-14, 5) :: Nil) :: Nil)
    assert (firstMatrix.add(secondMatrix) == sumMatrix)
  }

  test("Matrices with different dimensions can't be summed") {
    val firstMatrix = Matrix((Fraction(1, 4) :: Fraction(1, 8) :: Nil) :: (Fraction(-6, 7) :: Fraction(2, 1) :: Nil) :: Nil)
    val secondMatrix = Matrix((Fraction(3, 8) :: Nil) :: (Fraction(-1, 2) :: Nil) :: Nil)
    val exception = intercept[IllegalArgumentException](firstMatrix.add(secondMatrix))
    assert(exception.getMessage.equals("requirement failed: To add or subtract matrices, they have to have same dimensions"))
  }

  /* Testing method "subtract" */

  test("Difference of two 2x2 matrices") {
    val firstMatrix = Matrix((Fraction(8, 3) :: Fraction(-3, 1) :: Nil) :: (Fraction(4, 3) :: Fraction(-4, 7) :: Nil) :: Nil)
    val secondMatrix = Matrix((Fraction(-1, 1) :: Fraction(9, 10) :: Nil) :: (Fraction(2, 1) :: Fraction(1, 2) :: Nil) :: Nil)
    val differenceMatrix = Matrix((Fraction(11, 3) :: Fraction(-39, 10) :: Nil) :: (Fraction(-2, 3) :: Fraction(-15, 14) :: Nil) :: Nil)
    assert(firstMatrix.subtract(secondMatrix) == differenceMatrix)
  }

  test("Matrices with different dimensions can't be subtracted") {
    val firstMatrix = Matrix((Fraction(-7, 10) :: Fraction(-4, 3) :: Nil) :: Nil)
    val secondMatrix = Matrix((Fraction(-4, 9) :: Nil) :: (Fraction(-2, 1) :: Nil) :: Nil)
    val exception = intercept[IllegalArgumentException](firstMatrix.subtract(secondMatrix))
    assert(exception.getMessage.equals("requirement failed: To add or subtract matrices, they have to have same dimensions"))
  }

  /* Testing method "multiply" */

  test("Can't multiply matrices with no correct dimensions") {
    val firstMatrix = Matrix((Fraction(7, 3) :: Fraction(-8, 3) :: Nil) :: Nil)
    val secondMatrix = Matrix((Fraction(10, 9) :: Fraction(10, 7) :: Fraction(1, 10) :: Nil) :: Nil)
    val exception = intercept[IllegalArgumentException](firstMatrix.multiply(secondMatrix))
    assert(exception.getMessage.equals("requirement failed: To multiply matrices, number of columns in the first matrix must be equal to number of rows in the second matrix"))
  }

  test("Multiply matrices of dimensions 1x2 and 2x3") {
    val firstMatrix = Matrix((Fraction(1, 1) :: Fraction(5, 1) :: Nil) :: Nil)
    val secondMatrix = Matrix((Fraction(-1, 2) :: Fraction(-1, 2) :: Fraction(-1, 1) :: Nil) :: (Fraction(-7, 3) :: Fraction(2, 3) :: Fraction(-10, 9) :: Nil) :: Nil)
    val expectedResult = Matrix((Fraction(-73, 6) :: Fraction(17, 6) :: Fraction(-59, 9) :: Nil) :: Nil)
    assert(firstMatrix.multiply(secondMatrix) == expectedResult)
  }

  test("Multiply matrices of dimensions 1x2 and 2x1") {
    val firstMatrix = Matrix((Fraction(5, 2) :: Fraction(5, 7) :: Nil) :: Nil)
    val secondMatrix = Matrix((Fraction(1, 5) :: Nil) :: (Fraction(-7, 2) :: Nil) :: Nil)
    val expectedResult = Matrix((Fraction(-2, 1) :: Nil) :: Nil)
  }

  /* Gaussian elimination, first part */

  test("Gaussian elimination first part of invertible matrix, checking only resulting matrix") {
    val firstRow = Fraction(4, 5) :: Fraction(-4, 3) :: Fraction(8, 9) :: Nil
    val secondRow = Fraction(1, 5) :: Fraction(-9, 10) :: Fraction(-3, 2) :: Nil
    val thirdRow = Fraction(-3, 1) :: Fraction(-1, 1) :: Fraction(-1, 6) :: Nil
    val matrix = Matrix(firstRow :: secondRow :: thirdRow :: Nil)
    val firstRowOfResult = Fraction(1, 1) :: Fraction(-5, 3) :: Fraction(10, 9) :: Nil
    val secondRowOfResult = Fraction(0, 1) :: Fraction(1, 1) :: Fraction(155, 51) :: Nil
    val thirdRowOfResult = Fraction(0, 1) :: Fraction(0, 1) :: Fraction(1, 1) :: Nil
    val result = Matrix(firstRowOfResult :: secondRowOfResult :: thirdRowOfResult :: Nil)
    matrix.gaussianEliminationPartOne()
    assert(matrix == result)
  }

  test("Gaussian elimination first part of non-invertible matrix, checking only resulting matrix") {
    val firstRow = Fraction(-1, 2) :: Fraction(-8, 7) :: Fraction(-5, 1) :: Nil
    val secondRow = Fraction(1, 1) :: Fraction(-8, 3) :: Fraction(0, 1) :: Nil
    val thirdRow = Fraction(1, 1) :: Fraction(0, 1) :: Fraction(70, 13) :: Nil
    val matrix = Matrix(firstRow :: secondRow :: thirdRow :: Nil)
    val firstRowOfResult = Fraction(1, 1) :: Fraction(16, 7) :: Fraction(10, 1) :: Nil
    val secondRowOfResult = Fraction(0, 1) :: Fraction(1, 1) :: Fraction(105, 52) :: Nil
    val thirdRowOfResult = Fraction(0, 1) :: Fraction(0, 1) :: Fraction(0, 1) :: Nil
    val result = Matrix(firstRowOfResult :: secondRowOfResult :: thirdRowOfResult :: Nil)
    matrix.gaussianEliminationPartOne()
    assert(matrix == result)
  }

  /* Gaussian elimination, both parts */

  test("Gaussian elimination, both parts, on invertible matrix, checking only resulting matrix") {
    val firstRow = Fraction(5, 1) :: Fraction(-1, 2) :: Fraction(7, 3) :: Nil
    val secondRow = Fraction(-10, 1) :: Fraction(-8, 5) :: Fraction(-6, 5) :: Nil
    val thirdRow = Fraction(2, 7) :: Fraction(1, 3) :: Fraction(6, 7) :: Nil
    val matrix = Matrix(firstRow :: secondRow :: thirdRow :: Nil)
    matrix.gaussianEliminationBothParts()
    assert(matrix == Matrix(3))
  }

  test("Gaussian elimination, both parts, on non-invertible matrix, checking only resulting matrix") {
    val firstRow = Fraction(-1, 1) :: Fraction(-5, 3) :: Fraction(2, 1) :: Nil
    val secondRow = Fraction(7, 2) :: Fraction(-1, 1) :: Fraction(-1, 2) :: Nil
    val thirdRow = Fraction(-1, 2) :: Fraction(-9, 10) :: Fraction(218, 205) :: Nil
    val matrix = Matrix(firstRow :: secondRow :: thirdRow :: Nil)
    val firstRowOfResult = Fraction(1, 1) :: Fraction(0, 1) :: Fraction(-17, 41) :: Nil
    val secondRowOfResult = Fraction(0, 1) :: Fraction(1, 1) :: Fraction(-39, 41) :: Nil
    val thirdRowOfResult = Fraction(0, 1) :: Fraction(0, 1) :: Fraction(0, 1) :: Nil
    val result = Matrix(firstRowOfResult :: secondRowOfResult :: thirdRowOfResult :: Nil)
    matrix.gaussianEliminationBothParts()
    assert(matrix == result)
  }

}