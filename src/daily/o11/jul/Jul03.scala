package daily


/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 7/3/11
 * Time: 4:02 PM
 */
import processing.core._

class Jul03 extends PApplet with NameApplet {
  import PApplet._; import PConstants._; 

  val name = getNameShape(400, "ariblk.ttf")
  var bgImg: PGraphics = _
  override def setup() {
    size(1024, 300)
    makeGradient()
  }

  def makeGradient() {
    bgImg = createGraphics(width, height, P2D)
    bgImg.beginDraw()
    for(y <- 0 until bgImg.height) {
      val white = map(pow(map(y, 0, bgImg.height*.8f, 0, 1), .3f), 0, 1, 0, 255)
      bgImg.stroke(white)
      bgImg.line(0, y, bgImg.width, y)
    }
    bgImg.endDraw()
  }

  override def draw() {
    background(bgImg)
    fill(255); noStroke; smooth;
    setRShapeCenter(name, mouseX, mouseY)
    name.draw(this)
  }
}