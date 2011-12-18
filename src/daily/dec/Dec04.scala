package daily
package dec

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 12/4/11
 * Time: 4:50 PM
 */
import processing.core._
import org.zhang.lib.MyPApplet
import peasy.PeasyCam

class Dec04 extends MyPApplet with Savable {
  import PApplet._; import PConstants._; 

  lazy val cam = new PeasyCam(this, 100);
  override def setup() {
    size(500, 500, P3D)
    cam;
  }

  val side = 50

  override def draw() {
    background(255);
    fill(128);
    stroke(0);
    lights();
    for(rad <- 0 until 400 by side) {
      val steps = (TWO_PI * rad / side).toInt
      for(angI <- 0 until steps) {
        val ang = map(angI, 0, steps, 0, TWO_PI)
        pushMatrix()
        rotate(ang)
        translate(rad, 0)
        val height = 400 * sq(noise(rad * cos(ang) / 100, rad * sin(ang) / 100, millis() / 1000f))
//        translate(0, 0, height/2)
        box(side/2, side/2, height)
        popMatrix()
      }
    }
    
    pollSave() //check if the screen should be saved
  }
  
  override def keyPressed() {
    super.keyPressed(); //pressing space toggles a boolean variable to save the screen 
  }
}