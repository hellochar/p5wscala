package daily


/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 7/27/11
 * Time: 1:52 AM
 */
import processing.core._
import org.zhang.geom.Vec2
import org.zhang.lib.P5Util
import zhang.Methods

class Jul27 extends PApplet with NameApplet {
  import PApplet._;

  override def setup() {
    size(500, 500)
    background(0)
  }

  override def draw() {
//    background(0)
    ellipse(P5Util.randomVector(this), Vec2(10, 10))
    blendIn()
  }

  def blendIn() {
    type Pixel = (Vec2, Int)
    def act(p:Pixel):Pixel = {
      val loc = p._1
      val center = Vec2(width/2, height/2)
//      return (loc + Vec2.fromPolar(2, map(millis(), 0, 1000, 0, PI/2)), p._2)
      return (loc + (center - loc).setMag(-5), p._2)
//      return (loc + Vec2(.5f, .5f), p._2)
    }
    loadPixels()
    val buffer = Array.fill(pixels.length)(color(0))
    def setBuffer(p:Pixel) {
      val loc = p._1
//      println("loc: "+loc)
      if(Methods.isInWindow(loc.x, loc.y, width-1, height-1)) {
        val index = (loc.y.asInstanceOf[Int] * width + loc.x.asInstanceOf[Int])
        buffer(index) = p._2
      }
    }
    for(i <- 0 until pixels.length) {
      setBuffer(act(Vec2(i%width, i/width), pixels(i)))
    }
    arrayCopy(buffer, pixels)
    updatePixels()
  }
}