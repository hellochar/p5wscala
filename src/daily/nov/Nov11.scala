package daily.nov

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 11/11/11
 * Time: 12:24 AM
 */
import processing.core._
import org.zhang.lib.MyPApplet
import peasy.PeasyCam
import zhang.Methods
import daily.{SphereUtils, Savable}
import org.zhang.geom.Vec3

class Nov11 extends MyPApplet with Savable with SphereUtils {
  import PApplet._; import PConstants._; 

  lazy val cam = new PeasyCam(this, 500);
  override def setup() {
    size(500, 500, P3D)
    cam; //force it
  }

  var locs = (0 until 3).map(_ => Vec3.random * 100)
  val vels = locs.map(x => Vec3.random cross x ofMag 1)

  override def draw() {
    background(64)
    Methods.drawAxes(g, 100)
    noStroke(); fill(255 * (1+cos(millis / 1250f))/2, 255 * (1+sin(millis / 2000f))/2, 0); lights(); sphereDetail(30);
    sphere(100)

    locs = locs.zip(vels).map{ case (x, v) => x.rotate(v, PI/2) } //2PI per rotation, PI/2 per frame = 4  frames per rotation.
    fill(255); sphereDetail(6);
    locs foreach (x => sphere(x, 5f))

    pollSave() //check if the screen should be saved
  }
  
  override def keyPressed() {
    super.keyPressed(); //pressing space toggles a boolean variable to save the screen 
  }
}