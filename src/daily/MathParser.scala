/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 7/12/11
 * Time: 3:06 PM
 */
package daily

import java.io.{InputStreamReader, BufferedReader}
import scala.util.parsing.combinator._


class MathParser(
  val vars: collection.mutable.Map[String, Float] = collection.mutable.Map(),

  val consts:Map[String, Float] = Map(
    "PI" -> math.Pi.toFloat,
    "e" -> math.E.toFloat
  ),

  val funcs:Map[String, Double => Float] = {
    import math._
    Map("sin" -> sin _,
        "cos" -> cos _,
        "log" -> log _,
        "tan" -> tan _,
        "sqrt" -> sqrt _
    ).mapValues(_ andThen (_.toFloat))
  }
                  ) extends JavaTokenParsers {

  //todo: change defs to vals or lazy vals; make sure there aren't any subtle bugs to be had
  def expression: Parser[Float] = (ptTerm ~ rep("+" ~ ptTerm | "-" ~ ptTerm)) ^^ { case start~rest => {
    rest.foldLeft(start){ case (accum, "+"~num) => accum + num; case (accum, "-"~num) => accum - num; }
  }}

  def ptTerm: Parser[Float] = (factor ~ rep("*" ~ factor | "/" ~ factor)) ^^ { case start~rest => {
    rest.foldLeft(start){ case (accum, "*"~num) => accum * num; case (accum, "/"~num) => accum / num; }
  }}

  def factor: Parser[Float] = (number ~ rep("^" ~ number)) ^^ { case start~rest => {
    rest.foldLeft(start){ case (accum, "^"~exp) => math.pow(accum, exp).toFloat }
  }}

  def number: Parser[Float] = opt("-") ~ (functionApplication | num) ^^ { //i wonder if ordering matters here
    case Some("-")~f => -f
    case None~f => f
  }

  def num: Parser[Float] = fp | symbol | ("(" ~ expression ~ ")") ^^ { case "("~expr~")" => expr}

  def functionApplication: Parser[Float] = (map2Parser(funcs)~"("~expression~")") ^^ {
    case func~"("~expr~")" => func(expr)
  }

  def fp = floatingPointNumber ^^ (_.toFloat)

  def symbol: Parser[Float] = const | variable

  //todo: the parser doesn't update along with the mutable map. write your own mutable parser.
  private def map2Parser[B](map:collection.Map[String, B]):Parser[B] = map.map(_._1:Parser[String]) match {
    case k if k.isEmpty => failure("Map parser failed for "+map)
    case k => k.reduceLeft(_ | _) ^^ map.apply
  }
  
  def variable: Parser[Float] = map2Parser(vars)/*(vars.map(x => x._1:Parser[String]) match {
    case k if k.isEmpty => failure("No variable recognized")
    case k => k.reduceLeft(_ | _) ^^ (vars.apply)
  })*/

  def const:Parser[Float] = map2Parser(consts)//consts.map(x => x._1:Parser[String]).reduceLeft(_ | _) ^^ (consts.apply)

  def parse(s:String):Either[String, Float] = parseAll(expression, s) match {
    case k: Success[Float] => Right(k.get)
    case other: NoSuccess => Left(other.msg)
  }

}
object MathParser {
  def main(args: Array[String]) {
    val pt = new MathParser();
    val reader = new BufferedReader(new InputStreamReader(System.in))
    while(true) {
      print("Input: ");
      val in = reader.readLine()
//      val out = parseAll(expression, in)
      val out = pt.parse(in)

//      out.
      println(out)
    }
  }
}