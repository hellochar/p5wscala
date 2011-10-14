package parse.expr

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 10/7/11
 * Time: 5:28 PM
 */

import scala.util.parsing.combinator._
class ExprParser(
  val vars: collection.mutable.Map[String, Float] = collection.mutable.Map("x" -> 12, "y" -> 9),

  val consts:Map[String, Float] = Map(
    "PI" -> math.Pi.toFloat,
    "e" -> math.E.toFloat
  ),

  val funcs:Map[String, Float => Float] = {
    import math._
    Map("sin" -> sin _,
        "cos" -> cos _,
        "log" -> log _,
        "tan" -> tan _,
        "sqrt" -> sqrt _
    ).mapValues(func => (x:Float) => func(x).toFloat)
  }
                  ) extends JavaTokenParsers {

  def parse(s:String):Either[String, Expr] = parse(expr, s) match {
    case Success(e, _) => Right(e)
    case NoSuccess(msg, _) => Left(msg)
  }

  def expr: Parser[Expr] = (ptTerm ~ rep("+" ~ ptTerm | "-" ~ ptTerm)) ^^ { case start~rest => {
    rest.foldLeft(start){ case (accum, "+"~num) => Add(accum, num); case (accum, "-"~num) => Sub(accum, num); }
  }}

  /**
  * Plus/minus term.
  */
  def ptTerm: Parser[Expr] = (mdTerm ~ rep("*" ~ mdTerm | "/" ~ mdTerm)) ^^ { case start~rest => {
    rest.foldLeft(start){ case (accum, "*"~num) => Mul(accum, num); case (accum, "/"~num) => Div(accum, num); }
  }}

  /**
  * Multiply/divide term.
  */
  def mdTerm: Parser[Expr] = (expTerm ~ rep("^" ~> expTerm)) ^^ { case start~rest => {
      (rest foldLeft start){ case (accum, exp) => Pow(accum, exp)}
    }
  }

  /**
  * exponent term.
  */
    def expTerm: Parser[Expr] = opt("-") ~ (functionApplication | num) ^^ {
      case Some("-")~e  => Neg(e)
      case None~e       => e
    }

    def functionApplication: Parser[Expr] = (mapParser(funcs) ~ ("(" ~> expr <~ ")")) ^^ {
      case funcName ~ expr => UDFunc(funcName, expr)
    }

    def num: Parser[Expr] = fp | symbol | ("(" ~> expr <~ ")")

      def fp: Parser[Const] = floatingPointNumber ^^ {n => Const(n.toFloat)}

      def symbol: Parser[Expr] = variable | constant

        def constant: Parser[Const] = mapParser(consts) ^^ (s => Const(consts(s)))

        def variable: Parser[Variable] = mapParser(vars) ^^ (s => Variable(s))

  /**
  * Careful, this is hot mutable shit. maybe caching later?
  */
  case class Variable(name:String) extends Expr {
    def eval = vars(name)
    override def toString = name
  }

  /**
  * could definitely do some caching. Unless the function you're pointing to is some mutable container type...?
  */
  case class UDFunc(name:String, e:Expr) extends Unary(e) {
    override def func = funcs(name)
    override def toString = name+"("+e+")"
  }

  private def mapParser[A](map:collection.Map[String, A]):Parser[String] = map.keys.map(x => x:Parser[String]).reduceOption(_ | _) match {
    case Some(p) => p
    case None => failure("Map parser failed for "+map)
  }
}

//this class doesn't really make sense because the ExprParser is meant to cache the parsed syntax tree, while eval(String, Float*) will reparse each time.
//class VariableExprParser(names:String*) {
//  val vars: collection.mutable.Map[String, Float] = collection.mutable.Map() ++= names.map(_ -> 0f)
//  val mp = new ExprParser(vars);
//
////    val (memFunc, memCache) = org.zhang.lib.memoize(evalStr _)
//
///**
//* Evaluates the given string, replacing the variable names with the specified variable values (values must be passed
//* in the same order that names is passed in to the constructor). Extra variables will be disregarded; variables that
//* are unspecified will be unchanged from the last invocation. You may access the underlying "vars" map to have more
//* control.
//*/
//  def eval(s:String, values:Float*) = {
//    names.zip(values).foreach((vars.update _).tupled)
//    mp.parse(s);
//  }
//
//  override def toString = getClass.getSimpleName+"[vars="+vars+"]";
//}

object ExprParser {
  import java.io.{InputStreamReader, BufferedReader}

  def main(args: Array[String]) {
    val e = new ExprParser();
    val reader = new BufferedReader(new InputStreamReader(System.in))
    while(true) {
      print("Input: ");
      val in = reader.readLine()
      val out = e.parse(e.expr, in)
      println(out)
      out.map(e => println(e.eval))
    }

  }
}