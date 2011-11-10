package daily
package nov

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 11/9/11
 * Time: 6:11 PM
 */
import processing.core._
import org.zhang.lib.MyPApplet
import peasy.PeasyCam
import zhang.Methods

class Nov9 extends MyPApplet with Savable with SphereUtils with SphereSurfaceUI {
  import PApplet._; import PConstants._; 

  lazy val cam = new PeasyCam(this, 100);
  override def setup() {
    size(500, 500)
    cam;
  }

  override def draw() {
    Methods.drawAxes(g, 20)
    
    pollSave()
  }
  
  override def keyPressed() {
    super.keyPressed();
  }
}