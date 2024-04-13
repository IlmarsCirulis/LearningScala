class Matrix(var content: Seq[Seq[Fraction]]) {
  require(content.forall(row => row.length == content.head.length), "Jagged array isn't accepted")
  val (numberOfRows, numberOfColumns) = (content.length, content.head.length)
  
  def isSquareMatrix: Boolean = numberOfRows == numberOfColumns
  
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

  def gaussianEliminationPartTwo(): Seq[ElementaryRowOperation] = {
    var usedOperations = Seq[ElementaryRowOperation]()
    var (currentRow, currentColumn) = (numberOfRows - 1, numberOfColumns - 1)
    while (currentRow >= 0 && currentColumn >= 0) {
      if (content(currentRow)(currentColumn) != Fraction(0, 1)) {
        for (row <- 0 until currentRow) {
          if (content(row)(currentColumn) != Fraction(0, 1)) {
            val multiplier = content(row)(currentColumn).opposite
            addMultipliedRowToAnotherRow(row, currentRow, multiplier)
            usedOperations = usedOperations.appended(ElementaryRowOperation.additionOfMultipliedRow(row, currentRow, multiplier))
          }
        }
        currentRow -= 1
      }
      currentColumn -= 1
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

  override def toString: String = content.toString
}
