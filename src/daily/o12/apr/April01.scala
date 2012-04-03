package daily

import util.parsing.combinator._
import processing.core._
import org.zhang.lib.MyPApplet
import org.zhang.geom.{Vec2, Vec3}
import peasy.PeasyCam

class April01 extends MyPApplet with Savable with JavaTokenParsers {

  import PApplet._;
  import PConstants._;

  def withString[R](str:String, func:() => R) = new (() => R) {
    override def toString() = str
    def apply() = func()
  }
//
//  def weightedRandom[T](m:Map[Float, T]):T = {
//    var rand = random(m.keys.sum)
//    for((weight, t) <- m) if(rand < weight) return t else {rand -= weight }
//    return null.asInstanceOf[T]
//  }
//
//  def construction: Parser[Construction] = rep("PRIORITY " ~> wholeNumber ~ rep(prod)) ^^ {l =>
//    new Construction(l flatMap {
//      case pString~prodList => {prodList foreach {_.priority = pString.toInt }; prodList}
//    })
//  }
//
//  def prod: Parser[Production] = (symbol ~ opt(":" ~> cond) ~ "->" ~ rep(successor) ~ opt( floatingPointNumber )) ^^ {
//    case lhs~condOption~"->"~succList~probOption => new Production(lhs, succList, probOption.getOrElse(1), condOption.getOrElse(TrueCond))
//  }
//  def symbol = regex("""[a-zA-Z]+""".r)
//
//  def successor = (symbol ^^ {}) | rule
//  def rule = ("Subdiv("~axis~rep(floatingPointNumber)~"){"~rep(successor)~"}") ^^ {
//    case "Subdiv("~axis~rep(floatingPointNumber)~"){"~rep(successor)~"}"
//  }
//  def axis = "X" | "Y" | "Z" | "XY" | "XZ" | "YZ" | "XYZ"
//
//  class Construction(val prods:Set[Production], axiom:Production) {
//    /**
//     * Returns a production for a given shape
//     * @param s
//     */
//    def makeSelection(s:Shape):Production =
//      weightedRandom(prods.filter(_.lhs == s.symbol).filter(_.cond.map{_(s)}.getOrElse(true)).map(p => p.prob -> p).toMap) //find my productions, filter those by condition, make weighted map, choose
//
//    /**
//     * Selects a random production for a given string
//     * @param symbol
//     * @return
//     */
//    def makeSelection(symbol:String) = weightedRandom(prods.filter(_.lhs == symbol).filter(_.cond.isEmpty).map(p => p.prob -> p).toMap)
//  }
//
//  class Production(val lhs:String, val rhs:String, val prob:Float, val cond:Option[Condition]) {
//    var priority = 0;
//  }
//
//  /**
//   * Any sort of boolean expression. For now we support
//   * visible("[label]", "distant")
//   * occ("all", "active", "[label]", "noparent") -> enum of "none", "part", "full"
//   *
//   * ==, !=
//   */
//  def cond:Parser[Condition] = success(TrueCond) //for now
//
//  type Condition = (Shape => Boolean)
//  object TrueCond extends Condition {
//    def apply(s:Shape) = true
//  }

  case class Scope(p:Vec3 = Vec3(), X:Vec3 = Vec3.X, Y:Vec3 = Vec3.Y, Z:Vec3 = Vec3.Z, scale:Vec3 = Vec3(1)) {
    def matrix = {
      val (sX, sY, sZ) = (X * scale.y, Y * scale.y, Z * scale.z)
      new PMatrix3D(sX.x, sY.x, sZ.x, p.x,
                    sX.y, sY.y, sZ.y, p.y,
                    sX.z, sY.z, sZ.z, p.z,
                    0, 0, 0, 1)
    }
  }

  trait Primitive {
    def draw()
  }
  case class Cube(s:Scope) extends Primitive {
    def draw() {
      matrix(s.matrix) {
        translate(.5f, .5f, .5f)
        box(1)
      }
    }
  }
  case class Cylinder(s:Scope) extends Primitive {
    def draw() {
      matrix(s.matrix) {
        translate(.5f, .5f, 0)
        rotateX(-PI/2)
        cylinder(.5f, 1, .5f, true, .025f)
      }
    }
  }

  class Shape(val symbol:String) {

    private var scopeVar = Scope(Vec3(), Vec3.X, Vec3.Y, Vec3.Z, Vec3(1))
    private var queue = collection.immutable.Queue[Scope]();

    def pushMatrix() {
      queue = queue enqueue scopeVar
    }
    def popMatrix() {
      val (newScope, newQueue) = queue.dequeue
      scopeVar = newScope
      queue = newQueue
    }

    def scope = scopeVar

    def translate(x:Float, y:Float, z:Float) { scopeVar = scopeVar.copy(p = scopeVar.p + Vec3(x, y, z)) }
    def setScale(x:Float, y:Float, z:Float) { scopeVar = scopeVar.copy(scale = Vec3(x, y, z)) }
    def rotateX(deg:Float) {
      val rad = math.toRadians(deg).toFloat
      scopeVar = scopeVar.copy(Y = scopeVar.Y.rotate(Vec3.X, rad), Z = scopeVar.Z.rotate(Vec3.X, rad))
    }
    def rotateY(deg:Float) {
      val rad = math.toRadians(deg).toFloat
      scopeVar = scopeVar.copy(X = scopeVar.X.rotate(Vec3.Y, rad), Z = scopeVar.Z.rotate(Vec3.Y, rad))
    }
    def rotateZ(deg:Float) {
      val rad = math.toRadians(deg).toFloat
      scopeVar = scopeVar.copy(X = scopeVar.X.rotate(Vec3.Z, rad), Y = scopeVar.Y.rotate(Vec3.Z, rad))
    }

    def placePrimitive(name:String) {
      name match {
        case "cube" => primitives += Cube(scope)
        case "cylinder" => primitives += Cylinder(scope)
      }
    }
  }

  /**
   * This might hold "A" -> "[S(1,1,5) T(0,0,5) I(cube)] floor floor floor,
   *                 floor
   */
  var productions:Map[String, String] = Map()

  private def mapParser[A](map:collection.Map[String, A]):Parser[String] = map.keys.map(x => x:Parser[String]).reduceOption(_ | _) match {
    case Some(p) => p
    case None => failure("Map parser failed for "+map)
  }

//  def prod = (symbol ~ "->" ~ """[a-zA-Z0-9, \(\)\*\"{}\|]+""".r) ^^ {
//    case lhs ~ "->" ~ instrsString => productions(lhs) = instrsString
//  }
  def symbol = regex("""[a-zA-Z]+""".r)

  def instrs = rep(instr)
  def instr = func3("T", activeShape.translate _) |
              func3("S", activeShape.setScale _) |
              "[" ^^ (_ => () => activeShape.pushMatrix()) |
              "]" ^^ (_ => () => activeShape.popMatrix()) |
              ("I("~>("""[a-zA-Z0-9]+""".r)<~")" ^^ (s => () => activeShape.placePrimitive(s)))
//  |(productions )

  def fp = floatingPointNumber
  def func3[T](name:String, func:(Float, Float, Float) => T) = ((name+"(")~>((fp <~ ",") ~ (fp <~ ",") ~ fp)<~")") ^^ {
    case a~b~c => () => func(a.toFloat,b.toFloat,c.toFloat)
  }


//  A -> < T(0,0,6) S(8,10,18) I("cube") >
//       T(6, 0, 0) S(7, 13, 18) I("cube") T(0, 0, 16) S(8, 15, 8) I("cylinder")

  var activeShape:Shape = _;
  var configuration = Set[Shape]()
  var primitives = Set[Primitive]()

  lazy val cam = new PeasyCam(this, 100);
  override def setup() {
    size(500, 500, P3D)
    cam
  }

  override def draw() {
    background(204)
    zhang.Methods.drawAxes(g, 250)
    scale(10)
//    (new Cube(Scope())).draw()
    activeShape = new Shape("A")
    primitives = Set()

    parse(instrs, """[ T(0, 0, 6) S(8, 10, 18) I(cube) ] T(6, 0, 0) S(7, 13, 18) I(cube) T(0, 0, 16) S(8, 15, 8) I(cylinder)""") match {
      case Success(instrs, _) => {
        instrs foreach {_()}
      }
      case NoSuccess(str, _) => println("no success! "+str)
    }

    noFill();
    primitives foreach {_.draw()}

    pollSave() //check if the screen should be saved
  }

  override def keyPressed() {
    super.keyPressed(); //pressing space toggles a boolean variable to save the screen
  }
}