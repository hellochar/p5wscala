package daily
package jan

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 1/7/12
 * Time: 4:29 PM
 */

import processing.core._
import org.zhang.lib.{P5Util, MyPApplet}
import org.zhang.geom.Vec2

class Jan07 extends MyPApplet with Savable {app =>
  import PApplet._; import PConstants._;

  var points = makePoints

  def makePoints = (0 until randi(4, 10)) map { _ => (P5Util.randomVector(app), randomColor) }

  override def setup() {
    size(500, 500)
  }

  override def draw() {
    def closestPoint(p:Vec2) = points.minBy{ case (pos, _) => pos distTo p }
    loadPixels()
    for(x <- 0 until width; y <- 0 until height) {
      pixels(y*width+x) = closestPoint(Vec2(x, y))._2
    }
    updatePixels()

    pollSave()
  }

  override def keyPressed() {
    super.keyPressed(); //pressing space toggles a boolean variable to save the screen
    if(key == 'q') points = makePoints
  }
}