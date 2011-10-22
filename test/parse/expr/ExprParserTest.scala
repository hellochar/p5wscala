package parse.expr

import org.scalatest.FunSuite
import collection.mutable.Stack

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 10/21/11
 * Time: 6:29 PM
 */

class ExprParserTest extends FunSuite {
  implicit def numeric2const[A:Numeric](a:A) = Const(implicitly[Numeric[A]].toFloat(a))
  implicit def d2f(d:Double) = d.toFloat

  def testOnce(s:String, should:Expr, expr:ExprParser = new ExprParser()) {
    val parse = expr parse s
    assert { parse.isRight }
    assert { parse.right.get == should }
  }

  test("addition and subtraction") {
    testOnce("3+7+8-2-1+6-4",
      Sub(Add(Sub(Sub(Add(Add(Const(3.0), Const(7.0)), Const(8.0)), Const(2.0)), Const(1.0)), Const(6.0)), Const(4.0)))
  }

  test("Multiplication and division") {
    testOnce("3*7/4*6",
      Mul(Div(Mul(3, 7), 4), 6))
  }

  test("Exponentiation") {
    testOnce("2^4^6^8",
      Pow(Pow(Pow(2, 4), 6), 8))
  }

  test("Symbol resolution") {
    val expr = new ExprParser(collection.mutable.Map("x" -> 3, "y" -> 5, "z" -> 10))
    import expr._
    testOnce("x+y*z",
      Add(Variable("x"), Mul(Variable("y"), Variable("z"))),
      expr)
    testOnce("PI*x",
      Mul(expr.consts("PI"), Variable("x")),
      expr)
  }
}