class Rational (n: Int, d: Int) {
  require(d != 0)
  def this (n: Int) = this (n, 1) // costruttore ausiliare; se denominatore non è specificato allore è 1

  private val g = gcd(n.abs, d.abs)

  val numeratore: Int = n / g
  val denominatore: Int = d / g

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
