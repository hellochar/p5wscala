package daily
package nov

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 11/12/11
 * Time: 7:01 PM
 */
import processing.core._
import org.zhang.lib.MyPApplet

class Nov12 extends MyPApplet with Savable {
  import PApplet._; import PConstants._; 

  override def setup() {
    size(500, 500)
  }

  override def draw() {
    
    
    pollSave() //check if the screen should be saved
  }
  
  override def keyPressed() {
    super.keyPressed(); //pressing space toggles a boolean variable to save the screen 
  }
}