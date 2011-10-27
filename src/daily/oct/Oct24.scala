package daily.oct

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 10/24/11
 * Time: 12:43 AM
 */
import processing.core._
import org.zhang.lib.MyPApplet
import daily.Savable
import org.zhang.lib.misc.Vec3
import peasy.PeasyCam

class Oct24 extends MyPApplet with Savable {
  import PApplet._; import PConstants._;

  case class Triangle(v1:Vec3, v2:Vec3, v3:Vec3) {
    lazy val (l1, l2, l3) = (Line(v1, v2), Line(v2, v3), Line(v3, v1))
    lazy val (p1, p2, p3) = (l1.midpoint ofMag 100, l2.midpoint ofMag 100, l3.midpoint ofMag 100)

    def refine = List(
      Triangle(v1, p1, p3),
      Triangle(p1, v2, p2),
      Triangle(p3, p2, v3),
      Triangle(p1, p2, p3)
    )

    lazy val points = List(v1, v2, v3);

    def drawPoints() {
      points.foreach(x => {
        pushMatrix()
        translate(x)
//        translate(x)
        sphere(2)
        popMatrix()
      })
    }

    def drawTri() {
      beginShape();
      points.foreach(vertex _)
      endShape(CLOSE);
    }
  }

  case class Line(v1:Vec3, v2:Vec3) {
    def offset = v2 - v1;
    def midpoint = (v1 + v2) scale .5f;
  }

  var triangles = List[Triangle]()

  def tetrahedron(side:Float = 100) = {
    val angle = 2 * atan(sqrt(2)) //tetrahedral angle
    ((0 to 2).map(x => Vec3.fromSpherical(side, TWO_PI / 3 * x, PI / 2 - angle)) :+ Vec3.fromSpherical(side, 0, PI / 2)).
      combinations(3).map(x => Triangle(x(0), x(1), x(2))).toList
  }

  def octahedron(side:Float = 100) = {
    val centers = Range.Double(0, TWO_PI, PI/2).map(x => Vec3.fromSpherical(side, x.toFloat, 0))
    val centerTwos = centers.sliding(2).toList :+ Seq(centers.last, centers.head)
    List(Vec3.fromSpherical(side, 0, PI/2), Vec3.fromSpherical(side, 0, -PI/2)).flatMap(x => {
      centerTwos.map(s => Triangle(x, s(0), s(1)))
    })
  }


  lazy val cam = new PeasyCam(this, 100)

  override def setup() {
    size(500, 500, P3D)
    sphereDetail(1)
    cam;
//    triangles = tetrahedron;
    triangles = octahedron();
  }

  override def draw() {
    background(64);
//    stroke(255);
    lights();
    noStroke();

//    noFill()
    fill(255);
    
    triangles.
//      filter(_.v1.angleZ > 0).
      foreach(_.drawTri())
    zhang.Methods.drawAxes(g, 20)
    
    pollSave("Oct24-")
  }
  
  override def keyPressed() {
    super.keyPressed();
    if(key == 'd')
      triangles = triangles.flatMap(_.refine)
  }
}