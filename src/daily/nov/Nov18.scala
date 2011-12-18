package daily
package nov

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 11/18/11
 * Time: 11:16 AM
 */
import processing.core._
import peasy.PeasyCam
import zhang.Methods
import org.zhang.geom.Vec3
import org.zhang.lib.{P5Util, MyPApplet}

/**
 * Generative plants; like Nodes only in 3D and more rigorous.
 */

class Nov18 extends MyPApplet with Savable {
  import PApplet._; import PConstants._;


  lazy val cam = new PeasyCam(this, 500);
  var branches:Traversable[Branch] = List[Branch](new Branch(Vec3.ZERO, Vec3.Z * 100))
  override def setup() {
    size(500, 500, P3D)
    cam
  }

  override def draw() {
    background(255)
    strokeWeight(1);
    Methods.drawAxes(g, 20);
    strokeWeight(3); stroke(20, 210, 60);
    branches.foreach(_.draw())
    
    pollSave() //check if the screen should be saved
  }
  
  override def keyPressed() {
    super.keyPressed(); //pressing space toggles a boolean variable to save the screen
    branches = iterate(branches)
//    println(branches)
  }

  def iterate(b:Traversable[Branch]) = b ++ b.flatMap(x => transform.map(x.child _))

  /**
   * Transforms do not discriminate based on branch; instead, they operate in the branch's cs.
   */
  def transform = org.zhang.lib.random(transforms);

  val transforms = List(
    Range.Double(0, TWO_PI, TWO_PI/5).map(x => Line(Vec3.ZERO, Vec3.fromSpherical(.8f, x.toFloat, PI/2 - PI/8))),
    Range.Double(0, TWO_PI, TWO_PI/3).map(x => Line(Vec3.Z / 2, Vec3.fromSpherical(1f, x.toFloat, PI/2 - PI/8)))
  )

  case class Line(start:Vec3, end:Vec3) extends (Vec3, Vec3)(start, end) {
    def off = end - start
    def map(f:Vec3 => Vec3) = Line(f(start), f(end))
    def foreach[A](f:Vec3 => A) {f(start); f(end)}
  }

  /**
   * I'd rather like to imagine that each Branch actually has its very own coordinate system centered on the start, with the
   * Z coordinate oriented from the start to the end, and of length 1. Then, transforms output vectors that live in an individual branch's cs.
   * The path function also outputs vectors in this branch's CS.
   *
   * @param start Starting point of the branch, interpreted in global drawing coordinates.
   * @param end Ending point of the branch, interpreted in global drawing coordinates.
   * @param path A function that maps the range [0, 1] to a path that begins at the origin and stops at (0, 0, 1).
   */
  class Branch(start:Vec3, end:Vec3, val path:Float => Vec3) extends Line(start, end) {
    def this(l:Line, along:Float => Vec3) = this(l.start, l.end, along)

    def this(start:Vec3, end:Vec3) = this(start, end, Vec3.Z * _)
    def this(l:Line) = this(l, Vec3.Z * _)

    /**
     * A matrix that, given a global point, returns the corresponding point in this branch's cs. The inverse of this matrix
     * transforms the opposite way.
     */
    val csInv = {
      val mat = new PMatrix3D();
      mat.translate(start.x, start.y, start.z);
      mat.apply(P5Util.rotateAtoBMat(Vec3.Z, off))
      mat.scale(off.mag)
      mat
    }
    val cs = {
      val mat = new PMatrix3D(csInv);
      mat.invert();
      mat
    }

    /**
     * Interprets the input vector as a global point and transforms it into a point in this branch's cs.
     */
    def toCS(v:Vec3) = P5Util.transformed(v, cs)

    /**
     * Interprets the input vector as a vector in this branch's cs and transforms it into the corresponding global point.
     */
    def toGlobal(v:Vec3) = P5Util.transformed(v, csInv)

    def child(l:Line) = new Branch(l.map(toGlobal _), path)
    def child(v:Vec3) = new Branch(start, toGlobal(v), path)

    def draw() {
//      line()
//      lines3(Range.Double(0, 1, .1).map(x => along(x.toFloat)))
      lines3(Range.Double(0, 1, .1).map(x => toGlobal(path(x.toFloat)))) //kind of inefficient
//      line(start, end)
    }
  }

}