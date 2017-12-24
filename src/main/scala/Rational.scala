

class Rational (n: Int, d: Int) {
  require(d != 0)
  def this (n: Int) = this (n, 1) // costruttore ausiliare; se denominatore non è specificato allore è 1

//  private val g = gcd(n.abs, d.abs) TODO: gcd function fails. disabling till it's fixed

  val numeratore: Int = n // / g
  val denominatore: Int = d // / g

  private def gcd (a: Int, b: Int): Int = {
    if (b == 0) a else gcd(b, a & b)
  }

  override def toString: String = numeratore + " / " + denominatore

  def + (that: Rational): Rational = {
    new Rational(numeratore * that.denominatore + that.numeratore * denominatore, denominatore * that.denominatore)
  }

  def + (i: Int): Rational = {
    new Rational(numeratore + i * denominatore, denominatore)
  }

  def - (that: Rational) : Rational = {
    new Rational(numeratore * that.denominatore - that.numeratore * denominatore, denominatore * that.denominatore)
  }

  def - (i: Int) : Rational = {
    new Rational(numeratore - i * denominatore, denominatore)
  }

  def * (that: Rational) : Rational = {
    new Rational(numeratore * that.numeratore, denominatore * that.denominatore)
  }

  def * (i: Int) : Rational = {
    new Rational(numeratore * i, denominatore)
  }

  def / (that: Rational) : Rational = {
    new Rational(numeratore * that.denominatore, denominatore * that.numeratore)
  }

  def / (i: Int) : Rational = {
    new Rational(numeratore, denominatore * i)
  }
}

object Rational {
  def main(args: Array[String]):Unit = {
    println("Inserire nominatore 1")
    val nom1: Int = io.StdIn.readInt()
    println("Inserire denominatore 1")
    val den1: Int = io.StdIn.readInt()

    println("Inserire nominatore 2")
    val nom2: Int = io.StdIn.readInt()
    println("Inserire denominatore 2")
    val den2: Int = io.StdIn.readInt()

    println(s"Frazione 1: $nom1 | $den1 --- Frazione 2: $nom2 | $den2")

    Calcolatore(new Rational(nom1, den1), new Rational(nom2, den2))
  }

  def Calcolatore(r1: Rational, r2: Rational): Unit = {
    println(r1 + r2)
  }
}
