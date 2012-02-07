package daily


/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 8/11/11
 * Time: 4:17 PM
 */
import processing.core._
import org.zhang.lib.MyPApplet
import toxi.processing.ToxiclibsSupport
import peasy.PeasyCam
import toxi.geom.{Vec2D, Line2D}
import org.zhang.geom.Vec2

class Aug11 extends MyPApplet with Savable {
  import PApplet._; import PConstants._;

  type Line = Line2D
//  implicit def ii2v2[T, U](i:(T, U))(implicit num1: Numeric[T], num2: Numeric[U]) = {
//    new Vec2D(num1.toFloat(i._1), num2.toFloat(i._2))
//  }
  implicit def vec2D2Vec2(l:Vec2D) = Vec2(l.x, l.y)

  var lines = Set[Line]()

  lazy val sup = new ToxiclibsSupport(this)

  override def setup() {
    size(500, 500, P3D)
    new PeasyCam(this, 500)
    tree(new Line(new Vec2D(0, 0), new Vec2D(100, 0)))
  }

  def angle(v:Vec2D) = atan2(v.y, v.x)

  def tree(seed:Line) { lines += seed;
    def isIntersecting(l:Line) = lines.exists(_.intersectLine(l).getType == Line2D.LineIntersection.Type.INTERSECTING)
    def offspring(line:Line) = {
      def randomLine = {
        val a = line.a add line.getDirection.scale(random(line.getLength));
        val b = a.add(Vec2D.fromTheta(line.getDirection.angleBetween(Vec2D.X_AXIS) + (if(random(1)<.5) 90 else -90)).scale(line.getLength*random(.5f, .9f)))
        new Line2D(a, b)
      }
      def randStream:Stream[Line] = Stream.cons(randomLine, randStream)
      randStream.filter(isIntersecting _).take(random(2, 6).toInt).toSet
    }
    var newSet = Set(seed)
    for(i <- 1 to 2) { val s = newSet.flatMap(offspring _); lines ++= s; newSet = s; }
  }

  override def draw() {
    background(252); lights();
    noStroke; fill(190); rect(-500, -500, 1000, 1000);
    fill(128)
    lines foreach(render _)
    pollSave("Aug11-")
  }

  def render(l:Line) {
    pushMatrix()
    translate(l.a);
    rotate(l.getTheta)
    val (len, width, height) = (l.getLength, 30, 90)
    translate(len/2, width/2, height/2)
    box(len, width, height)
    popMatrix()
  }
  
  override def keyPressed() {
    super.keyPressed();
  }
}