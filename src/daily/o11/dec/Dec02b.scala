package daily


/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 12/2/11
 * Time: 11:37 AM
 */
import processing.core._
import org.zhang.lib.MyPApplet
import org.zhang.parse.expr._

class Dec02b extends MyPApplet with Savable { app =>
  import PApplet._; import PConstants._; 

  lazy val cam = new zhang.Camera(this)
  override def setup() {
    size(600, 600)
    cam
  }

  val parser = new ExprParser(
      funcs = Map(
        "atan" -> atan _,
        "sin" -> sin _,
        "cos" -> cos _,
        "tan" -> tan _,
        "abs" -> (abs(_:Float))
      )
    )

  case class Noise1(e:Expr) extends Unary(e, noise(_))
  case class Noise2(left:Expr, right:Expr) extends Binary(left, right, noise(_, _))
  case class Mag(left:Expr, right:Expr) extends Binary(left, right, (x:Float, y:Float) => sqrt(sq(x) + sq(y)))
  case class Angle(left:Expr, right:Expr) extends Binary(left, right, atan2 _)
  case class Max(l:Expr, r:Expr) extends Binary(l, r, max _)
  case class Min(l:Expr, r:Expr) extends Binary(l, r, min _)

  def probabilitiesRaw(d:Int) =
  List(
    (d.toFloat-1)   ->(() =>      Const(randi(-10, 10))                    ),
    (1f)            ->(() =>      Neg(randomExpr(d+1))                     ),
    (1f)            ->(() =>      Noise1(randomExpr(d+1))                  ),
    (3f)            ->(() =>      parser.Variable("x")                     ),
    (3f)            ->(() =>      parser.Variable("y")                     ),
    (d.toFloat-1)   ->(() =>      Const(random(-10, 10))                   )
    //(3f)            ->(() =>      parser.Variable("y")                     ),
  ) ++ List(Add, Sub, Mul, Div, Pow, Noise2, Mag, Angle).map(x =>
    (5f/d)          ->(() =>      x(randomExpr(d+1), randomExpr(d+1))      )
  ) ++ parser.funcs.keys.map{s =>
    (1f)            ->(() =>      parser.UDFunc(s, randomExpr(d+1)))
  }

  /**
   * You can simplify any expression tree whose leaves are all referentially transparent.
   * Leaves that are referentially transparent:
   *  a) All Constants
   *
   * We can recursively go down the nodes, searching for simple trees (trees whose children are leaves), whose leaves are all constants, and converting that
   * tree into just its evaluated constant.
   *
   * As of now, we know the only source of non-transparency comes from Variables. So we can ignore those and then
   * simplify the rest of the tree. Of course this assumes that all of the function arguments in unary and binary are referentially transparent.
   */

  /**
   * This holds a set of Expr whose simplified versions should simply be a toString delegation. Expr's not in this list
   * should have a toString of the Expr's getClass.getSimpleName
   */
  var names = Map[Expr, String]()

  def stringRep(e:Expr) = e match {
    case c:Const => c.eval.toString
    case v:parser.Variable => v.name
    case u:parser.UDFunc => u.name
    case e:Expr if names.keySet(e) => names(e)
    case e:Expr => e.getClass.getSimpleName
  }

  def simplify(e:Expr):Expr =
    e match {
      case v:parser.Variable => v
      case u@Unary(Const(k), _) => Const(u.eval) //simple tree with 1 child
      case b@Binary(Const(x), Const(y), _) => Const(b.eval) //simple tree with 2 children
      case u:Unary => {val n = new Unary(simplify(u.op), u.func); names += (n -> stringRep(u)); n} //Not an immediately simplifiable tree; recurse
      case b:Binary => {val n = new Binary(simplify(b.leftOp), simplify(b.rightOp), b.func); names += (n -> stringRep(b)); n}
      case e:Expr => e //not one of the cases; let it pass
  }

  def hasXAndY(e:Expr):Boolean = {
    def has(b:Expr, e:Expr):Boolean = if(e == b) true else e match {
      case u:Unary => has(b, u.op)
      case bin:Binary => has(b, bin.leftOp) || has(b, bin.rightOp)
      case _ => false
    }
    has(parser.Variable("x"), e) && has(parser.Variable("y"), e)
  }

//  /**
//   * Gets rid of Exprs that will NaN.
//   */
//  def transformExpr(e:Expr) = e match {
//    case
//  }


  //@param depth current nesting of the random expression inside a bigger expression. Ranges from [1 -> infinity]
  def randomExpr(depth:Int):Expr = simplify({
    val probabilitiesRaw = app.probabilitiesRaw(depth)
    val probabilitiesNorm = {
        val total = probabilitiesRaw.map(_._1).sum
        probabilitiesRaw.map(x => (x._1/total, x._2))
      }
      val probabilitiesCum = {
        org.zhang.lib.partials(probabilitiesNorm.toStream).map(s =>
          (s.map(_._1).sum, s.last._2)
        ).toList
      }
    val r = random(1);
    probabilitiesCum.dropWhile(_._1 < r).head._2.apply()
  })

  def setExpr() {
    names = Map()
    expr = randomExpr(1)
    while(!hasXAndY(expr)) setExpr()
  }

  var expr:Expr = _
  setExpr()
  lazy val buf = createGraphics(200, 200, JAVA2D)
  var drawBuf = false

  override def draw() {
    background(0);
    if(!drawBuf) {
      textAlign(CENTER, CENTER)
      def draw(e:Expr) {
        colorMode(RGB)
        stroke(255); fill(255, 190, 0)
        ellipse(0, 0, 25, 25)
        fill(0, 255, 0);
        text(stringRep(e), 0, 0)
        e match {
          case u:Unary => {
            line(0, 0, 0, 25)
            pushMatrix()
            translate(0, 25)
            scale(.5f)
            draw(u.op)
            popMatrix()
          }
          case b:Binary => {
            line(0, 0, -25, 25)
            line(0, 0, 25, 25)
            pushMatrix()
            translate(-25, 25)
            scale(.5f)
            draw(b.leftOp)
            popMatrix()
            pushMatrix()
            translate(25, 25)
            scale(.5f)
            draw(b.rightOp)
            popMatrix()
          }
          case _ => {}
        }
      }

      draw(expr)
    } else {

      buf.beginDraw()
      buf.loadPixels()
      val (ws, hs) = (width / buf.width, height / buf.height)
      for(x <- 0 until width by ws; y <- 0 until height by hs) {
        parser.vars("x") = cam.modelX(x)
        parser.vars("y") = cam.modelY(y)
        val eval = expr.eval
        buf.colorMode(HSB)
        def colorFor(i:Float) = {
          buf.color(i % 255, 200, constrain(i*i * pow(2, map(mouseX, 0, width, -20, 20)), 0, 255))
        }
        buf.pixels((y/hs)*buf.width+(x/ws)) = colorFor(eval)
      }

      buf.updatePixels()
      buf.endDraw()
      resetMatrix()
      image(buf, 0, 0, width, height)
    }

    println(frameRate)
    pollSave() //check if the screen should be saved
  }

  // adjust this value to whatever depth is actually necessary
  val STACK_DEPTH = 512;
  val matrixStack = Array.ofDim[Float](STACK_DEPTH, 6);
  var matrixStackDepth:Int = _;

  // this version will override the built-in version pushMatrix function
  override def pushMatrix() {
    if (matrixStackDepth == 512) {
      throw new RuntimeException("too many calls to pushMatrix()");
    }
    this.g.getMatrix().get(matrixStack(matrixStackDepth));
    matrixStackDepth += 1;
  }

  // this version will override the built-in version popMatrix function
  override def popMatrix() {
    if (matrixStackDepth == 0) {
      throw new RuntimeException("too many calls to popMatrix()" +
                                 "(or too few to pushMatrix)");
    }
    matrixStackDepth -= 1;
    val m = new PMatrix2D();
    m.set(matrixStack(matrixStackDepth));
    this.g.setMatrix(m);
  }
  
  override def keyPressed() {
    super.keyPressed(); //pressing space toggles a boolean variable to save the screen
    if(key == 'z') {
      setExpr()
    }
    if(key == 'q') drawBuf = !drawBuf
  }
}