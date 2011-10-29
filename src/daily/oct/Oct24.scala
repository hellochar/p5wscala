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
import org.zhang.geom._
import peasy.PeasyCam

class Oct24 extends MyPApplet with Savable {
  import PApplet._; import PConstants._;

  var mesh = Mesh.sphere(100, 0);

  def drawPoints(t:Triangle) {
    import t._
    points.foreach(x => {
      pushMatrix()
      translate(x)
      sphere(2)
      popMatrix()
    })
  }

  def drawTri(t:Triangle) {
    import t._;
    beginShape();
    points.foreach(vertex _)
    endShape(CLOSE);
  }

  lazy val cam = new PeasyCam(this, 100)

  override def setup() {
    size(500, 500, P3D)
    sphereDetail(1)
    cam; //force it
//    frameRate(5)
  }

  override def draw() {
    background(64);
//    lights();


    stroke(255);
//    noStroke();

    noFill()
//    fill(255);
    
    mesh.triangles.
//      filter(_.v1.angleZ > 0).
      foreach(drawTri _)
    
//    println(triangles.flatMap(_.points).filter(x => (x - Vec3(100, 0, 0)).mag < 10))

    stroke(255, 0, 0)
    mesh.edgesFor(mesh.points.toSeq.apply(millis.toInt / 1000 % mesh.points.size)).foreach(l => line(l.v1, l.v2))
    zhang.Methods.drawAxes(g, 20)
    
    pollSave("Oct24-")
  }

  def refined = new TriangleMesh(spherify(100, mesh.triangles.flatMap(refine _)))

  override def keyPressed() {
    super.keyPressed();
    if(key == 'd') {
      val (n, t) = org.zhang.lib.time(refined)
      mesh = n
      println("Took "+t/1e6+" ms!")
    }
  }
}