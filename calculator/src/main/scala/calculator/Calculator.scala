package calculator

enum Expr:
  case Literal(v: Double)
  case Ref(name: String)
  case Plus(a: Expr, b: Expr)
  case Minus(a: Expr, b: Expr)
  case Times(a: Expr, b: Expr)
  case Divide(a: Expr, b: Expr)

object Calculator extends CalculatorInterface:
 import Expr.*

  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] =
    namedExpressions.map {
      e => (e._1, Signal {
        val ex = e._2()
        eval(ex, namedExpressions.removed(e._1))
      })
    }


  def eval(expr: Expr, references: Map[String, Signal[Expr]])(using Signal.Caller): Double = {

    expr match {
      case Literal(v: Double) => v
      case Ref(name: String) => {
        val e = getReferenceExpr(name, references)
        eval(e, references.removed(name))
      }
      case Plus(a: Expr, b: Expr) => {
        val ax = eval(a, references)
        val bx = eval(b, references)
        ax + bx
      }
      case Minus(a: Expr, b: Expr) => {
        val ax = eval(a, references)
        val bx = eval(b, references)
        ax - bx
      }
      case Times(a: Expr, b: Expr) => {
        val ax = eval(a, references)
        val bx = eval(b, references)
        ax * bx
      }
      case Divide(a: Expr, b: Expr) => {
        val ax = eval(a, references)
        val bx = eval(b, references)
        ax / bx
      }
    }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]])(using Signal.Caller): Expr =
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
