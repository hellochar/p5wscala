package daily

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 7/1/11
 * Time: 4:27 AM
 */
import processing.core._

class Jul01 extends PApplet {
  import PApplet._;

  override def setup() {
    size(500, 500)
  }

  override def draw() {
    background(frameCount % 255)
    noFill; stroke(0);
//    setRShapeCenter(name, mouseX, mouseY)

//    name.drawVF(this);


    println("before "+Thread.currentThread())
//    super.drawVF()
    println("after "+frameCount+", "+frameRate)
  }

}