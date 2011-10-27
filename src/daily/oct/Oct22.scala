package daily
package oct

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 10/22/11
 * Time: 2:44 AM
 */
import processing.core._
import org.zhang.lib.MyPApplet

class Oct22 extends MyPApplet with Savable {
  import PApplet._; import PConstants._; 

  override def setup() {
    size(500, 500)
    background(0)
    smooth()
  }

  override def draw() {
    stroke(random(255))
    line(random(width), 0, random(width), height);
    pollSave("")
  }
  
  override def keyPressed() {
    super.keyPressed();
  }
}