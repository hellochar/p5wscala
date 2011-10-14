package parse.expr

import processing.core.PApplet

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 10/9/11
 * Time: 3:01 PM
 */
trait Expr { def eval:Float; }
case class Const(eval:Float) extends Expr { override def toString = eval.toString }

abstract class Unary(e:Expr) extends Expr {
  def func:Float => Float
  def eval = func(e.eval)
}

case class Neg(e:Expr) extends Unary(e) { override def func = -_; override def toString = "-("+e+")" }

abstract class Binary(left:Expr, right:Expr) extends Expr { //hmmm
  def func:(Float, Float) => Float
  def eval = func(left.eval, right.eval)

  def char:Char
  override def toString = "("+left+char+right+")"
}

case class Add(left:Expr, right:Expr) extends Binary(left, right) { override def func = _ + _; override def char = '+' }
case class Sub(left:Expr, right:Expr) extends Binary(left, right) { override def func = _ - _; override def char = '-' }
case class Mul(left:Expr, right:Expr) extends Binary(left, right) { override def func = _ * _; override def char = '*' }
case class Div(left:Expr, right:Expr) extends Binary(left, right) { override def func = _ / _; override def char = '/' }
case class Pow(left:Expr, right:Expr) extends Binary(left, right) { override def func = PApplet.pow _; override def char = '^' }
//todo: get rid of PApplet reference