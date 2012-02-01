package daily


/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 10/29/11
 * Time: 12:47 PM
 */

import processing.core._
import org.zhang.lib.MyPApplet
import peasy.PeasyCam
import zhang.Methods
import org.zhang.geom.Vec3

class Oct29 extends MyPApplet with Savable with SphereUtils {

  import PConstants._;

  lazy val cam = new PeasyCam(this, 300)

  override def setup() {
    size(500, 500, P3D)
    cam
  }

  var h1 = Vec3.fromSpherical(100, random(TWO_PI), random(-PI / 2, PI / 2));
  def h2 = Vec3.fromSpherical(100, millis / 1250f, millis / 3000f);
//  var h2 = Vec3.fromSpherical(100, random(TWO_PI), random(-PI / 2, PI / 2));
  var norm = (h1 cross h2) ofMag 100

  override def draw() {
    background(64)
    noFill();

    Methods.drawAxes(g, 20)

    h1 = h1.rotate(norm normalize, 1 / h1.mag)

    stroke(255, 0, 0);
    line(h1)
    stroke(0, 255, 0); line(h2);
    stroke(0, 255, 0);
    line(norm)

    stroke(255, 255, 0)
    greatCircle(h1, h2)
    greatCircle(norm ofMag 110)

    stroke(0);
    gcArc(h1, h2)

    stroke(0, 0, 255);
    smallCircle(h2, h2.mag * (millis / 1000f % TWO_PI))

    pollSave()
  }

  override def keyPressed() {
    super.keyPressed();
    if (key == 'q') {
      h1 = Vec3.fromSpherical(100, random(TWO_PI), random(-PI / 2, PI / 2));
      //      h2 = Vec3.fromSpherical(100, random(TWO_PI), random(-PI/2, PI/2));
    }
  }
}