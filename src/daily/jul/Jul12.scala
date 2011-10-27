package daily
package jul

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 7/12/11
 * Time: 2:38 PM
 */

import processing.core._
import scala.util.parsing.combinator.JavaTokenParsers
import javax.swing.JOptionPane
import controlP5._
import zhang.{Methods, Camera}
import org.zhang.lib.misc.{TravList, Vec2}

class Jul12 extends PApplet with NameApplet {
  import PConstants._; import PApplet._
  
  implicit def v22pv2(v:Vec2) = new PVector(v.x, v.y)

  /**
  EBNF grammar. Should support creation of transform, rotate, scale, (and maybe shearX/Y later) matricies, and be able to combine them with + and *.
  for the future: Arbitrary 3x2 matricies should be buildable

  Expr ::= Term { "+" Term }
  Term ::= Matr { "*" Matr }
  Matr ::= MatrLit | { "(" Expr ")" }
  MatrLit ::= T(num, num) | R(num) | S(num) | X(num) | Y(num) | M(num, num, num, num, num, num)
  num  ::= any float parsable by Float.parseFloat()
  */
  object MatrixParser extends JavaTokenParsers {

    //Parse with failure
    def parse(s:String) = parseAll(expr, s) match {
      case p: NoSuccess => Left(p.msg)
      case p: Success[PMatrix2D] => Right((p.get, s))
    }

    private def byElementSum(m1:PMatrix2D, m2:PMatrix2D) = {  //This implements a by-element sum of two PMatrix2Ds
      m1.set(m1.get(null).zip(m2.get(null)).map{ case (a, b) => a + b}); m1;
    }

    def expr: Parser[PMatrix2D] = (term ~ rep("+" ~ term)) ^^ { case m~list => list.foldLeft(m) {
      case (accum, "+"~next) => byElementSum(accum, next)
    }}
    def term: Parser[PMatrix2D] = (matLiteral ~ rep("*" ~ matLiteral)) ^^ { case m~list => list.foldLeft(m){
      case (accum, "*"~next) => {accum.apply(next); accum}
    }}
    def matLiteral: Parser[PMatrix2D] = {

      def mkMatrix(f:PMatrix2D => Unit) = {val k = new PMatrix2D(); f(k); k}

      /**
      * Single parameter literal.
      */
      def spl(prefix:String, func:PMatrix2D => (Float => Unit)) = ((prefix+"(")~num~")") ^^ {
        case pre~num~")" => mkMatrix(matrix => func(matrix)(num))}

      def trans = ("T("~num~","~num~")") ^^ {case "T("~numX~","~numY~")" => mkMatrix(_.translate(numX, numY))}
      def rot = spl("R", m => x => m.rotate(radians(x)))
      def scl = spl("S", _.scale _)
      def sX = spl("X", _.shearX _)
      def sY = spl("Y", _.shearY _)

      trans | rot | scl | sX | sY
    }

    def num: Parser[Float] = floatingPointNumber ^^ (_.toFloat)
  }

  var S: Set[Geometry] = Set(); //Set of geometries
  var T: Set[PMatrix2D] = Set(); //Set of transformations


  var myCam:Camera = _
  var cp5:ControlP5 = _

  def initGui() {
    cp5 = new ControlP5(this)
    cp5.setAutoDraw(false)

    def addAction[A](c: Controller, action: => A) {
      c.addListener(new ControlListener() {
        def controlEvent(theEvent: ControlEvent) {
          action
        }
      })
    }

    val tfield = cp5.addTextfield("Enter transform", 0, 0, 200, 16);
    addAction(tfield, tryAddTransform(tfield.stringValue()))

    val resetGeo = cp5.addButton("Reset Geometry");         addAction(resetGeo, {resetGeometry()});
    val resetTrans = cp5.addButton("Reset Transforms");     addAction(resetTrans, { resetTransforms() })
    val iterateButton = cp5.addButton("Iterate");           addAction(iterateButton, { iterate() });
    val resetCameraButton = cp5.addButton("Reset Camera");  addAction(resetCameraButton, { resetCamera()})

    def makeButton[A](name:String, action: => A) = {
      val button = cp5.addButton("Draw "+name);
      addAction(button, action);
      button;
    }

    def mkDrawButton(i:Int) = {
      val item = curDrawer(i)
      val button = makeButton(item.toString, {curDrawer.index = i})
      button.setPosition(new PVector(0, 100 + 30 * i))
      button.setAbsolutePosition(new PVector(0, 100 + 30 * i))
    }
    
    (0 until curDrawer.size) foreach(mkDrawButton _)
  }

  def p5HasFocus = cp5.getControllerList.exists(_ match {
    case c: Textfield => c.isFocus
    case _ => false
  })

  def resetCamera() {
    myCam.setScale(width/2);
    myCam.setCenter(0, 0);
  }

  override def setup() {
    size(900, 500);
    rectMode(CORNERS);
    myCam = new Camera(this) {
      noAutoApply()

      def startCamKeys() {
        wasd()
        arrows()
        plusMinus()
      }

      def stopCamKeys() {
        noWasd()
        noArrows()
        noPlusMinus()
      }

      override def uiHandle() {
        if (p5HasFocus) stopCamKeys() //This is to keep the camera from panning and zooming when you're trying to type in a transformation.
        else startCamKeys()
        super.uiHandle()
      }

      override def toString = "(" + getCorner.x +", " + getCorner.y + " -- " + getScale+")"
    }
    resetGeometry();
    resetCamera();
    initGui()
    tryAddTransform("T(.25, .25)*S(.5)")
    tryAddTransform("R(22.5)*T(1.25, .25)")

    textFont(createFont("Arial", 20))
  }

  //Try adding a transform, or display an error message otherwise
  def tryAddTransform(s:String) {
    def mkFailString(s: String) { //If the input is bad, create a text box that displays the error.
      val tf = cp5.addTextfield("fail", 210, 0, 200, 16)
      tf.setUserInteraction(false)
      tf.setValue(s)
      new Thread() {
        override def run() {
          Thread.sleep(5000)
          tf.remove()
        }
      }.start()
    }
    parse(s) match {
      case Left(failString) => mkFailString(failString) //failed
      case Right((mat, str)) => T += new PMatrix2D(mat) { //Works - add the matrix
        override def toString = str; //gives you a meaningful string representation
      }
    }
  }

  def resetGeometry() {
    S = Set();
  }

  def resetTransforms() {
    T = Set();
  }

  /**
  * Attempts to parse the given string into a transformation using rules defined in MatrixParser. Returns a Right(PMatrix2D) on success
  * and a Left(String) on failure (the string holds the failure message)
  */
  def parse(s: String) = MatrixParser.parse(s)

  override def draw() {
    background(20);
//    noStroke();
    pushMatrix();
    myCam.apply()
    fill(255, 128); noStroke;
    ellipse(0, 0, .1f, .1f)
    stroke(255, 128);
    strokeWeight(0); //This ensures that the stroke weight is always one pixel wide.
    line(-8, 0, 8, 0)
    line(0, -8, 0, 8)
    noFill();
    (-5 to 3).map(pow(2, _)).foreach(i => rect(-i, -i, i, i) )
//    rect(-1, -1, 1, 1);
//    rect(-10, -10, 10, 10);

    noFill();
    stroke(60, 230, 25);
    S foreach (_.render)
    popMatrix();

    cp5.draw()
    fill(240);
    textSize(15)
    textAlign(LEFT, BOTTOM)
    text(T.mkString("\n"), 0, height)
//    pollSave("blogimg-")
  }


  def mouseOnP5 = cp5.getControllerList.exists(p => {
      Methods.isInRange(
        mouseVec,
        p.getPosition,
        PVector.add(p.getPosition, new PVector(p.getWidth, p.getHeight))
      )}) //Tests if the mouse is on a P5 control

  var pressed:Option[PVector] = None

  override def mousePressed() {
    if(!mouseOnP5) pressed = Some(myCam.model(mouseVec))
  }

  override def mouseReleased() {
    pressed match {
      case Some(there) => {
        var here = myCam.model(mouseVec)
        if(myCam.screenDist(PVector.sub(here, there)).mag() < 3)  Unit
        else S += new Geometry(curDrawer(there, here))
      }
      case None => Unit
    }
    pressed = None
  }

  trait Drawer extends ((PVector, PVector) => (() => Unit)) {
    override def toString = getClass.getSimpleName.split("\\$")(1)
  }

  object Rects extends Drawer {
    def apply(v1: PVector, v2: PVector) = () => { rect(v1.x, v1.y, v2.x, v2.y) }
  }
  object Circles extends Drawer {
    def apply(v1: PVector, v2: PVector) = {
      val offset = PVector.sub(v2, v1);
      val diam = offset.mag() * 2
      () => { ellipse(v1.x, v1.y, diam, diam) }
    }
  }
  object Lines extends Drawer {
    def apply(v1: PVector, v2: PVector) = () => line(v1.x, v1.y, v2.x, v2.y);
  }
  object curDrawer extends TravList(Seq(Rects, Circles, Lines)) with Drawer {
    def apply(v1: PVector, v2: PVector) = item.apply(v1, v2)
    override def toString = item.toString
  }

  def iterate() { //Move the iteration along.
    S = iterated();
  }

  /*
  a) you have a set of geometric objects (points, lines, polygons, curves, etc) (named S)
  b) you have a set of transformations (rotate, translate, scale, shear, etc) (named T)
  c) you apply each transformation to each object in that set, which gives you the set of transformed objects S' (giving you num(S)*num(T) new objects)
  d) Use S' as the set S in a)
  */
  def iterated(): Set[Geometry] = {
    var Sprime: Set[Geometry] = Set()
    S.foreach(g => T.foreach(t => Sprime += g.transformed(t)))
    return Sprime;
  }

  def vertex(v: PVector) {
    vertex(v.x, v.y);
  }

  class Geometry(val draw:() => Unit) {
    var t = new PMatrix2D; //The transformation this geometry should undertake
//    t.scale(width / 2);

    def render() {
      pushMatrix();
      applyMatrix(t);
      draw();
      popMatrix();
    }

    //Return a new geometry that represents this geometry after being transformed by the given matrix.
    def transformed(p: PMatrix2D) = {
      val g = new Geometry(draw);
      g.t = t.get(); //clone my transform to the new geometry
      g.t.apply(p); //apply the new transform to the new geometry
      g;
    }

  }

}