/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/* Modified into Scala source by Ilmars Cirulis */

import scala.math.BigInt

class Fraction(val num: BigInt, den: BigInt = BigInt(1)) extends Ordered[Fraction] {
  require(!den.equals(BigInt(0)), "Denominator must be different from 0")

  val (numerator: BigInt, denominator: BigInt) = {
    val gcd: BigInt = num.gcd(den)
    if (BigInt(1) < gcd) {
      (num / gcd * den.signum, den / gcd * den.signum)
    } else {
      (num * den.signum, den * den.signum)
    }
  }

  def signum: Int = numerator.signum * denominator.signum

  def opposite: Fraction = Fraction(-numerator, denominator)

  def abs: Fraction = if (signum >= 0) this else opposite

  def reciprocal: Fraction = new Fraction(denominator, numerator)

  private def isZero = numerator.signum == 0

  def add(value: Fraction): Fraction =
    if (value.isZero)
      this
    else if (this.isZero)
      value
    else {
      val (num: BigInt, den: BigInt) = if (denominator.equals(value.denominator))
        (numerator + value.numerator, denominator)
      else
        (numerator * value.denominator + value.numerator * denominator, denominator * value.denominator)
      if (num.signum == 0)
        Fraction(BigInt(0), BigInt(1))
      else
        Fraction(num, den)
    }

  def subtract(value: Fraction): Fraction = this.add(value.opposite)

  def multiply(value: Fraction): Fraction =
    if (value.signum == 0 || isZero)
      Fraction(BigInt(0), BigInt(1))
    else
      Fraction(value.numerator * numerator, value.denominator * denominator)

  def divide(value: Fraction): Fraction =
    if (value.isZero)
      throw new ArithmeticException("The value to divide by must not be zero")
    else if (isZero)
      Fraction(BigInt(0), BigInt(1))
    else
      Fraction(numerator * value.denominator, denominator * value.numerator)


  override def compare(other: Fraction): Int = {
    val leftSignum = signum
    val rightSignum = other.signum
    if (leftSignum != rightSignum)
      if (leftSignum > rightSignum)
        1
      else
        -1
    else if (leftSignum == 0)
      0
    else
      (numerator * other.denominator).compare(denominator * other.numerator)
  }

  override def equals(other: Any): Boolean = other match
    case that: Fraction =>
      if (signum == that.signum)
        numerator.abs.equals(that.numerator.abs) && denominator.equals(that.denominator)
      else
        false
    case _ => false

  override def hashCode: Int = {
    val numSignum = numerator.signum
    val denSignum = denominator.signum
    (31 * (31 + numerator.hashCode * numSignum) + denominator.hashCode * denSignum) * numSignum * denSignum
  }

  override def toString: String =
    if (isZero)
      "0"
    else if (BigInt(1).equals(denominator))
      numerator.toString()
    else
      s"$numerator / $denominator"

}
