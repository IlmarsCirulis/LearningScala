import org.scalatest.funsuite.AnyFunSuite

class FractionSuite extends AnyFunSuite {

  /* Testing constructor of Fraction */

  test("Check if Fraction(2, 0) throws exception") {
    val exception = intercept[IllegalArgumentException](Fraction(2, 0))
    assert(exception.getMessage.equals("requirement failed: Denominator must be different from 0"))
  }

  test("Check if Fraction(2, 1) is constructed correctly") {
    val fraction = Fraction(2, 1)
    assert(fraction.numerator == 2 && fraction.denominator == 1)
  }

  test("Check if Fraction(-2, 1) is constructed correctly") {
    val fraction = Fraction(-2, 1)
    assert(fraction.numerator == -2 && fraction.denominator == 1)
  }

  test("Check if Fraction(2, -1) is constructed correctly") {
    val fraction = Fraction(2, -1)
    assert(fraction.numerator == -2 && fraction.denominator == 1)
  }

  test("Check if Fraction(-2, -1) is constructed correctly") {
    val fraction = Fraction(-2, -1)
    assert(fraction.numerator == 2 && fraction.denominator == 1)
  }

  test("Fraction(4, 2) should be reduced to 2/1") {
    val fraction = Fraction(4, 2)
    assert(fraction.numerator == 2 && fraction.denominator == 1)
  }

  test("Fraction(12, 48) should be reduced to 1/4") {
    val fraction = Fraction(12, 48)
    assert(fraction.numerator == 1 && fraction.denominator == 4)
  }

  test("Fraction made of two large BigInt numbers, if their gcd = 1, isn't reduced") {
    val numerator = BigInt("6433475313700850949489303197863861127817114")
    val denominator = BigInt("170581960623309904414284800471343406106496673")
    val fraction = Fraction(numerator, denominator)
    assert(fraction.numerator.equals(numerator) && fraction.denominator.equals(denominator))
  }

  test("Fraction made of two large BigInt numbers, if their gcd <> 1, is reduced correctly") {
    val gcd = BigInt("89303197863861127817114")
    val numerator = BigInt("64334753137008509494")
    val denominator = BigInt("170581960623309904414281")
    // gcd(numerator, denominator) = 1
    val fraction = Fraction(numerator * gcd, denominator * gcd)
    assert(fraction.numerator.equals(numerator) && fraction.denominator.equals(denominator))
  }

  /* Testing equality or == method */

  test("Fraction(3, 4) == Fraction(3, 4) is true") {
    assert(Fraction(3, 4) == Fraction(3, 4))
  }

  test("Fraction(3, 4) == Fraction(3, 5) is false") {
    assert(Fraction(3, 4) != Fraction(3, 5))
  }

  test("Fraction(1, 2) == Fraction(-1, -2) is true") {
    assert(Fraction(1, 2) == Fraction(-1, -2))
  }

  test("Fraction(1, 2) == Fraction(-1, 2) is false") {
    assert(Fraction(1, 2) != Fraction(-1, 2))
  }

  test("Fraction(1, 2) == null is false") {
    assert(Fraction(1, 2) != null)
  }

  test("null == Fraction(1, 2) is false") {
    assert(null != Fraction(1, 2))
  }

  test("(null: Fraction) == (null: Fraction) is true") {
    assert((null: Fraction) == (null: Fraction))
  }

  /* Testing method "signum" */

  test("Fraction(0, 4).signum must be 0") {
    assert(Fraction(0, 4).signum == 0)
  }

  test("Fraction(-2, 4).signum must be -1") {
    assert(Fraction(-2, 4).signum == -1)
  }

  test("Fraction(2, 4).signum must be 1") {
    assert(Fraction(2, 4).signum == 1)
  }

  /* Testing method "opposite" */

  test("Opposite of zero is zero") {
    assert(Fraction(0, 1).opposite == Fraction(0, 1))
  }

  test("Opposite of 1/2 is -1/2") {
    assert(Fraction(1, 2).opposite == Fraction(-1, 2))
  }

  test("Opposite of -1/2 is 1/2") {
    assert(Fraction(-1, 2).opposite == Fraction(1, 2))
  }

  /* Testing method "abs" */

  test("Absolute value of zero is zero") {
    assert(Fraction(0, 1).abs == Fraction(0, 1))
  }

  test("Absolute value of 1/2 is 1/2") {
    assert(Fraction(1, 2).abs == Fraction(1, 2))
  }

  test("Absolute value of -1/2 is 1/2") {
    assert(Fraction(-1, 2).abs == Fraction(1, 2))
  }

  /* Testing method "reciprocal" */

  test("Reciprocal of 0/1 causes IllegalArgumentException") {
    val exception = intercept[IllegalArgumentException](Fraction(0, 1).reciprocal)
    assert(exception.getMessage.equals("requirement failed: Denominator must be different from 0"))
  }

  test("Reciprocal of 1/2 is 2/1") {
    assert(Fraction(1, 2).reciprocal == Fraction(2, 1))
  }

  test("Reciprocal of -1/2 is -2/1") {
    assert(Fraction(-1, 2).reciprocal == Fraction(-2, 1))
  }

  /* Testing method "add" */

  test("Sum of two zeroes is zero") {
    assert(Fraction(0, 1).add(Fraction(0, 1)) == Fraction(0, 1))
  }

  test("Sum of zero and a fraction is this fraction") {
    assert(Fraction(0, 1).add(Fraction(2, 3)) == Fraction(2, 3))
  }

  test("Sum of a fraction and zero is this fraction") {
    assert(Fraction(2, 3).add(Fraction(0, 1)) == Fraction(2, 3))
  }

  test("Sum of fractions with equal denominators (no reducing)") {
    assert(Fraction(2, 3).add(Fraction(2, 3)) == Fraction(4, 3))
  }

  test("Sum of fractions with equal denominators (with reducing)") {
    assert(Fraction(1, 12).add(Fraction(5, 12)) == Fraction(1, 2))
  }

  test("Sum of fractions with different denominators") {
    assert(Fraction(1, 12).add(Fraction(1, 2)) == Fraction(7, 12))
  }

  test("Sum of different sign fractions") {
    assert(Fraction(-1, 12).add(Fraction(1, 2)) == Fraction(5, 12))
  }

  test("Sum of different sign fractions N2") {
    assert(Fraction(4, 17).add(Fraction(-2, 3)) == Fraction(-22, 51))
  }

  test("Sum of two fractions made from big numbers") {
    val fraction1 = Fraction(BigInt("107224588561680849158155"), BigInt("532977310187969519"))
    val fraction2 = Fraction(BigInt("170581960623309904414284"), BigInt("800471343406106496673"))
    val result = Fraction(
      BigInt("85921126766675311526236312590915817298527711"),
      BigInt("426633063491137091563629149472398910287")
    )
    assert(fraction1.add(fraction2) == result)
  }

  /* Testing method "subtract" */

  test("Difference of two zeroes is zero") {
    assert(Fraction(0, 1).subtract(Fraction(0, 1)) == Fraction(0, 1))
  }

  test("Difference of zero and a fraction is opposite of this fraction") {
    assert(Fraction(0, 1).subtract(Fraction(2, 3)) == Fraction(-2, 3))
  }

  test("Difference of fraction and zero is this fraction") {
    assert(Fraction(2, 3).subtract(Fraction(0, 1)) == Fraction(2, 3))
  }

  test("Difference of fractions with equal denominators (no reducing)") {
    assert(Fraction(5, 7).subtract(Fraction(3, 7)) == Fraction(2, 7))
  }

  test("Difference of fractions with equal denominators (with reducing)") {
    assert(Fraction(1, 12).subtract(Fraction(5, 12)) == Fraction(-1, 3))
  }

  test("Difference of fractions with different denominators") {
    assert(Fraction(1, 12).subtract(Fraction(1, 2)) == Fraction(-5, 12))
  }

  test("Difference of different sign fractions") {
    assert(Fraction(1, 12).subtract(Fraction(-1, 2)) == Fraction(7, 12))
  }

  test("Difference of different sign fractions N2") {
    assert(Fraction(4, 17).subtract(Fraction(-2, 3)) == Fraction(46, 51))
  }

  test("Difference of two fractions made from big numbers") {
    val fraction1 = Fraction(BigInt("107224588561680849158155"), BigInt("532977310187969519"))
    val fraction2 = Fraction(BigInt("170581960623309904414284"), BigInt("800471343406106496673"))
    val result = Fraction(
      BigInt("85739294137596107835757267161282359418108919"),
      BigInt("426633063491137091563629149472398910287")
    )
    assert(fraction1.subtract(fraction2) == result)
  }

  /* Testing method "multiply" */

  test("0 multiplied by 0 is zero") {
    assert(Fraction(0, 1).multiply(Fraction(0, 1)) == Fraction(0, 1))
  }

  test("0 multiplied by nonzero fraction") {
    assert(Fraction(0, 1).multiply(Fraction(7, 12)) == Fraction(0, 1))
  }

  test("Nonzero fraction multiplied by zero") {
    assert(Fraction(-3, 7).multiply(Fraction(0, 1)) == Fraction(0, 1))
  }

  test("Product of two simple nonzero fractions, both positive") {
    assert(Fraction(3, 1).multiply(Fraction(1, 6)) == Fraction(1, 2))
  }

  test("Product of two simple nonzero fractions, first positive and second negative") {
    assert(Fraction(1, 2).multiply(Fraction(-4, 7)) == Fraction(-2, 7))
  }

  test("Product of two simple nonzero fractions, first negative and second positive") {
    assert(Fraction(-3, 16).multiply(Fraction(4, 3)) == Fraction(-1, 4))
  }

  test("Product of two simple nonzero fractions, both negative") {
    assert(Fraction(-4, 7).multiply(Fraction(-14, 3)) == Fraction(8, 3))
  }

  test("Product of two fractions, made of big numbers") {
    val fraction1 = Fraction(BigInt("107224588561680849158155"), BigInt("532977310187969519"))
    val fraction2 = Fraction(BigInt("170581960623309904414284"), BigInt("800471343406106496673"))
    val result = Fraction(
      BigInt("18290580543879248191104649498895462779757086020"),
      BigInt("426633063491137091563629149472398910287")
    )
    assert(fraction1.multiply(fraction2) == result)
  }

  /* testing method "divide" */

  test("0 divided by 0 throws exception") {
    val exception = intercept[ArithmeticException](Fraction(0, 1).divide(Fraction(0, 1)))
    assert(exception.getMessage.equals("The value to divide by must not be zero"))
  }

  test("Nonzero fraction divided by zero throws exception") {
    val exception = intercept[ArithmeticException](Fraction(3, 5).divide(Fraction(0, 1)))
    assert(exception.getMessage.equals("The value to divide by must not be zero"))
  }

  test("0 divided by nonzero fraction") {
    assert(Fraction(0, 1).divide(Fraction(7, 12)) == Fraction(0, 1))
  }

  test("Division of two simple nonzero fractions, both positive") {
    assert(Fraction(3, 1).divide(Fraction(1, 6)) == Fraction(18, 1))
  }

  test("Division of two simple nonzero fractions, first positive and second negative") {
    assert(Fraction(1, 2).divide(Fraction(-4, 7)) == Fraction(-7, 8))
  }

  test("Division of two simple nonzero fractions, first negative and second positive") {
    assert(Fraction(-3, 16).divide(Fraction(4, 3)) == Fraction(-9, 64))
  }

  test("Division of two simple nonzero fractions, both negative") {
    assert(Fraction(-4, 7).divide(Fraction(-14, 3)) == Fraction(6, 49))
  }

  test("Division of two fractions, made of big numbers") {
    val fraction1 = Fraction(BigInt("107224588561680849158155"), BigInt("532977310187969519"))
    val fraction2 = Fraction(BigInt("170581960623309904414284"), BigInt("800471343406106496673"))
    val result = Fraction(
      BigInt("85830210452135709680996789876099088358318315"),
      BigInt("90916314539601845239522714816728940209396")
    )
    assert(fraction1.divide(fraction2) == result)
  }

  /* Testing method "compare" */

  test("Few comparisons of positive fractions") {
    assert(Fraction(1, 2).compare(Fraction(1, 3)) == 1)
    assert(Fraction(1, 4).compare(Fraction(1, 4)) == 0)
    assert(Fraction(1, 6).compare(Fraction(1, 5)) == -1)
  }

  test("Few comparisons of negative fractions") {
    assert(Fraction(-1, 2).compare(Fraction(-1, 3)) == -1)
    assert(Fraction(-1, 4).compare(Fraction(-1, 4)) == 0)
    assert(Fraction(-1, 6).compare(Fraction(-1, 5)) == 1)
  }

  test("Few comparisons of different signum fractions") {
    assert(Fraction(-1, 2).compare(Fraction(1, 3)) == -1)
    assert(Fraction(1, 4).compare(Fraction(-1, 4)) == 1)
    assert(Fraction(0, 1).compare(Fraction(1, 7)) == -1)
    assert(Fraction(-2, 3).compare(Fraction(0, 1)) == -1)
    assert(Fraction(2, 3).compare(Fraction(0, 1)) == 1)
    assert(Fraction(0, 1).compare(Fraction(-1, 7)) == 1)
  }

  test("Comparison of two fractions, made from big numbers") {
    val fraction1 = Fraction(BigInt("107224588561680849158155"), BigInt("532977310187969519"))
    val fraction2 = Fraction(BigInt("170581960623309904414284"), BigInt("800471343406106496673"))
    assert(fraction1.compare(fraction2) == 1)
  }

  /* Testing method "toString" */

  test("Different integers to string") {
    assert(Fraction(0, 1).toString.equals("0"))
    assert(Fraction(10, 1).toString.equals("10"))
    assert(Fraction(-13, 1).toString.equals("-13"))
    assert(Fraction(-1000, 125).toString.equals("-8"))
    assert(Fraction(BigInt("1234567890123456789"), 1).toString.equals("1234567890123456789"))
  }

  test("Different noninteger fractions to string") {
    assert(Fraction(1, 2).toString.equals("1 / 2"))
    assert(Fraction(-3, 7).toString.equals("-3 / 7"))
    assert(Fraction(3, 12).toString.equals("1 / 4"))
    assert(Fraction(-5, 15).toString.equals("-1 / 3"))
    assert(Fraction(BigInt("1234567890123456789"), BigInt("9876543210987654321")).toString.equals("13717421 / 109739369"))
  }

}