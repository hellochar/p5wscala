package parse.expr

import processing.core.PApplet

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 10/9/11
 * Time: 3:01 PM
 */
trait Expr { def eval:Float; }
case class Const(eval:Float) extends Expr

abstract class Unary(e:Expr) extends Expr {
  def func:Float => Float
  def eval = func(e.eval)
}

case class Neg(e:Expr) extends Unary(e) { override def func = -_; }

abstract class Binary(left:Expr, right:Expr) extends Expr { //hmmm
  def func:(Float, Float) => Float
  def eval = func(left.eval, right.eval)
}

case class Add(left:Expr, right:Expr) extends Binary(left, right) { override def func = _ + _; }
case class Sub(left:Expr, right:Expr) extends Binary(left, right) { override def func = _ - _; }
case class Mul(left:Expr, right:Expr) extends Binary(left, right) { override def func = _ * _; }
case class Div(left:Expr, right:Expr) extends Binary(left, right) { override def func = _ / _; }
case class Pow(left:Expr, right:Expr) extends Binary(left, right) { override def func = PApplet.pow _; }
//todo: get rid of PApplet reference