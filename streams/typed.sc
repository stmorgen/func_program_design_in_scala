trait Gaz {

}

trait Liquid {

}

trait Solid {

}

trait Physics {
  implicit def air: Gaz
  implicit def condense(implicit gaz: Gaz): Liquid
  implicit def freeze(implicit liquid: Liquid): Solid

  def test() = {
    println("hi")
    println(summon[Solid])
    println(implicitly[Solid](freeze(condense(air))))
  }
}

object Mein extends Physics {

  override def air: Gaz = null

  override def condense(using gaz: Gaz): Liquid = null

  override implicit def freeze(implicit liquid: Liquid): Solid = null
}

Mein.test()

val x = 1 + 3


import scala.language.implicitConversions

case class Rational(numerator: Int, denominator: Int)

object Rational {
  implicit def fromInt(n: Int): Rational = Rational(n, 1)
}

val r: Rational = 42
println(r)

implicit class HasIsEven(n: Int) {
  def isEven: Boolean = n % 2 == 0
}

42.isEven
new HasIsEven(42).isEven