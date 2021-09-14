package calculator

object Polynomial extends PolynomialInterface:
  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] =
    Signal {

      val ax = a()
      val bx = b()
      val cx = c()
      bx * bx - 4 * ax * cx
    }

  def computeSolutions(a: Signal[Double], b: Signal[Double], c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] =
    Signal {

      val ax = a()
      val bx = b()
      val cx = c()
      val dx = delta()

      if (dx < 0) Set()
      else {

        val first = (-1.0 * bx + Math.sqrt(dx)) / (2 * ax)
        val second = (-1.0 * bx - Math.sqrt(dx)) / (2 * ax)
        Set() + first + second
      }
    }
