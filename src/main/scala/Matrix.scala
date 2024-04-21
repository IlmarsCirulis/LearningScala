import ElementaryRowOperation.*

class Matrix(var content: Seq[Seq[Fraction]]) {
  require(content.nonEmpty, "Empty list isn't accepted")
  require(content.forall(row => row.length == content.head.length), "Jagged list isn't accepted")
  require(content.head.nonEmpty, "Rows must be nonempty")
  val (numberOfRows, numberOfColumns) = (content.length, content.head.length)

  def this(fun: Int => Int => Fraction, numberOfRows: Int, numberOfColumns: Int) =
    this(Seq.range(0, numberOfRows).map(row => Seq.range(0, numberOfColumns).map(col => fun(row)(col))))

  def this(size: Int) = this(row => col => if (row == col) Fraction(1, 1) else Fraction(0, 1), size, size)

  def isSquareMatrix: Boolean = numberOfRows == numberOfColumns

  def transpose: Matrix = Matrix(row => col => content(col)(row), numberOfColumns, numberOfRows)

  def opposite: Matrix = {
    Matrix(content.map(row => row.map(x => x.opposite)))
  }

  private def sameDimensions(other: Matrix): Boolean = numberOfRows == other.numberOfRows && numberOfColumns == other.numberOfColumns

  def add(other: Matrix): Matrix = {
    require(sameDimensions(other), "To add or subtract matrices, they have to have same dimensions")
    Matrix(row => col => content(row)(col).add(other.content(row)(col)), numberOfRows, numberOfColumns)
  }

  def subtract(other: Matrix): Matrix = this.add(other.opposite)

  private def getColumn(column: Int): Seq[Fraction] = content.map(row => row(column))

  private def scalarProduct(first: Seq[Fraction], second: Seq[Fraction]): Fraction =
    first.lazyZip(second).map(_.multiply(_)).reduce(_.add(_))

  def multiply(other: Matrix): Matrix = {
    require(numberOfColumns == other.numberOfRows,
      "To multiply matrices, number of columns in the first matrix must be equal to number of rows in the second matrix")
    Matrix(row => col => scalarProduct(content(row), other.getColumn(col)), numberOfRows, other.numberOfColumns)
  }

  private def swapRows(firstRow: Int, secondRow: Int): Unit = {
    val t = content(firstRow)
    content = content.updated(firstRow, content(secondRow))
    content = content.updated(secondRow, t)
  }

  private def addRowToAnotherRow(row: Int, rowToAdd: Int): Unit = {
    val rowSum = content(row).lazyZip(content(rowToAdd)).map(_.add(_))
    content = content.updated(row, rowSum)
  }

  private def multiplyRow(row: Int, multiplier: Fraction): Unit = {
    val multipliedRow = content(row).map(_.multiply(multiplier))
    content = content.updated(row, multipliedRow)
  }

  private def addMultipliedRowToAnotherRow(row: Int, rowToAdd: Int, multiplier: Fraction): Unit = {
    val multipliedRow = content(rowToAdd).map(_.multiply(multiplier))
    val rowSum = content(row).lazyZip(multipliedRow).map(_.add(_))
    content = content.updated(row, rowSum)
  }

  def gaussianEliminationPartOne(): Seq[ElementaryRowOperation] = {
    var usedOperations = Seq[ElementaryRowOperation]()
    var (currentRow, currentColumn) = (0: Int, 0: Int)
    while (currentRow < numberOfRows && currentColumn < numberOfColumns) {
      val nonzero = content.indexWhere(r => r(currentColumn) != Fraction(0, 1), currentRow)
      if (nonzero != -1) {
        if (nonzero != currentRow) {
          swapRows(nonzero, currentRow)
          usedOperations = usedOperations.appended(ElementaryRowOperation.swap(nonzero, currentRow))
        }
        val multiplier1 = content(currentRow)(currentColumn).reciprocal
        multiplyRow(currentRow, multiplier1)
        usedOperations = usedOperations.appended(ElementaryRowOperation.multiplication(currentRow, multiplier1))
        for (row <- currentRow + 1 until numberOfRows) {
          if (content(row)(currentColumn) != Fraction(0, 1)) {
            val multiplier2 = content(row)(currentColumn).opposite
            addMultipliedRowToAnotherRow(row, currentRow, multiplier2)
            usedOperations = usedOperations.appended(ElementaryRowOperation.additionOfMultipliedRow(row, currentRow, multiplier2))
          }
        }
        currentRow += 1
      }
      currentColumn += 1
    }
    usedOperations
  }

  /* TODO: check this method more carefully */
  def gaussianEliminationPartTwo(): Seq[ElementaryRowOperation] = {
    var usedOperations = Seq[ElementaryRowOperation]()
    var (currentRow, currentColumn) = (numberOfRows - 1, numberOfColumns - 1)
    while (currentRow >= 0 && currentColumn >= 0) {
      val nonzero = content(currentRow).indexWhere(x => x != Fraction(0, 1))
      if (nonzero != -1) {
        currentColumn = nonzero
        for (row <- 0 until currentRow) {
          if (content(row)(currentColumn) != Fraction(0, 1)) {
            val multiplier = content(row)(currentColumn).opposite
            addMultipliedRowToAnotherRow(row, currentRow, multiplier)
            usedOperations = usedOperations.appended(ElementaryRowOperation.additionOfMultipliedRow(row, currentRow, multiplier))
          }
        }
      }
      currentRow -= 1
    }
    usedOperations
  }

  def gaussianEliminationBothParts(): Seq[ElementaryRowOperation] =
    gaussianEliminationPartOne() ++ gaussianEliminationPartTwo()

  def applyGivenOperations(operations: Seq[ElementaryRowOperation]): Unit = {
    for (op <- operations) {
      op match {
        case ElementaryRowOperation.swap(firstRow, secondRow) => swapRows(firstRow, secondRow)
        case ElementaryRowOperation.multiplication(row, multiplier) => multiplyRow(row, multiplier)
        case ElementaryRowOperation.addition(row, rowToAdd) => addRowToAnotherRow(row, rowToAdd)
        case ElementaryRowOperation.additionOfMultipliedRow(row, rowToAdd, multiplier) => addMultipliedRowToAnotherRow(row, rowToAdd, multiplier)
      }
    }
  }

  def calculateDeterminant(): Fraction = {
    require(isSquareMatrix, "Must be square matrix to calculate determinant")
    val operations = gaussianEliminationPartOne()
    val diagonal = List.range(0, numberOfRows).map(i => content(i)(i))
    if (diagonal.forall(x => x == Fraction(1))) {
      val fun: ElementaryRowOperation => Fraction = {
        case ElementaryRowOperation.multiplication(_, multiplier) => multiplier
        case _ => Fraction(1)
      }
      operations.map(fun).reduce(_.multiply(_)).reciprocal
    } else {
      Fraction(0)
    }
  }

  override def equals(other: Any): Boolean = other match
    case that: Matrix =>
      numberOfRows == that.numberOfRows &&
        numberOfColumns == that.numberOfColumns &&
        content == that.content
    case _ => false

  override def hashCode(): Int =
    val state = Seq(content)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)

  override def toString: String = content.toString

}

