package daily
package nov

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 11/2/11
 * Time: 11:52 AM
 */
import processing.core._
import org.zhang.lib.MyPApplet
import peasy.PeasyCam
import zhang.Methods
import org.zhang.geom.Vec3

class Nov2 extends MyPApplet with Savable with SphereUtils {
  import PApplet._; import PConstants._;

  implicit def zhang2pv(z:Vec3) = new PVector(z.x, z.y, z.z)
  implicit def pv2zhang(p:PVector) = Vec3(p.x, p.y, p.z)

  lazy val cam = new PeasyCam(this, 100);
  override def setup() {
    size(500, 500, P3D)
    cam
  }

  private class Point(locc:Vec3) {
    private var loc = locc;
    private val (drop, low) = (.99f, .01f)
    private val tail = new SizedQueue((log(low) / log(drop)).toInt, loc)

    def run() {
      loc = rotate(loc, axis, PI/30)
//      if(frameCount % 15 == 0)
        tail push loc
    }

    def draw() {
      noStroke(); sphereDetail(5); fill(255, 255, 0);
      sphere(loc, 1)

      noFill(); strokeWeight(3);
      beginShape();
      tail.seq.zipWithIndex foreach {case (loc, idx) => {
        val alpha = 255 * pow(drop, idx)
        stroke(255, 255, 0, alpha);
        vertex(loc); }}
      endShape();
    }
  }

  def axis = Vec3.fromSpherical(1, millis / 2000f, (millis / 4350f) % PI)
  private val points = (0 until 1).map(_ => new Point(Vec3.random ofMag random(1, 100)))

  override def draw() {
    background(64); lights()
    points foreach {x => x.run(); x.draw()}
    Methods.drawAxes(g, 20);
    pushMatrix()
    rotateAtoB(Vec3.X, axis) //the x axis now aligned to our axis of rotation
    stroke(0, 0, 0)
    Methods.drawArrow(g, 100, 0, 0)
    popMatrix()
    pollSave("")
  }

  /**
   * axis must be the unit vector. This performs Rodriguez's rotation formula
   */
  def rotate(v:Vec3, axis:Vec3, theta:Float) =
    v * cos(theta) + (axis cross v) * sin(theta) + axis * (axis.dot(v) * (1 - cos(theta)))
  
  override def keyPressed() {
    super.keyPressed();
  }
}