package daily

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 2/9/12
 * Time: 2:00 AM
 */

import processing.core._
import org.zhang.lib.MyPApplet
import org.zhang.geom.Vec2

class Feb09 extends MyPApplet with Savable {

  import PApplet._;
  import PConstants._;

  lazy val cam = new zhang.Camera(this)
  override def setup() {
    size(500, 500)
    cam.noAutoApply()
    cam.setCenter(0, 0);
    cam.setViewportWidth(4)
  }

  trait Shader {
    def colorFor(sx:Int, sy:Int):Int
  }
  trait WindowedShader extends Shader {
    def cx:Double
    def cy:Double
    def rx:Double
    def ry:Double

    def convertX(sx:Int) = cx + rx * sx / width;
    def convertY(sy:Int) = cy + ry * sy / height;

    def colorFor(sx:Int, sy:Int) = colorForD  (convertX(sx), convertY(sy))
    def colorForD(x:Double, y:Double):Int
  }
  trait SingleValueShader extends WindowedShader {
    val MAX_ITER = 2000

    def colorFor(iter:Int):Int
    def value(x:Double, y:Double):Int
    def colorForD(x:Double, y:Double) = colorFor(value(x, y))
  }
  trait DefaultColorMap extends SingleValueShader {
    import toxi.color._
    private val cGrad = {
      val clist = new ColorGradient()
      clist.addColorAt(0 * MAX_ITER, TColor.YELLOW)
      clist.addColorAt(.2f * MAX_ITER, TColor.RED)
      clist.addColorAt(.4f * MAX_ITER, TColor.GREEN)
      clist.addColorAt(.8f * MAX_ITER, TColor.CYAN)
      clist.addColorAt(.6f * MAX_ITER, TColor.BLUE)
      clist.addColorAt(1f * MAX_ITER, TColor.BLACK)
      clist.addColorAt(1f * MAX_ITER + 1, TColor.BLACK)
      clist.calcGradient()
    }
    def colorFor(iter:Int):Int = cGrad.get(iter).toARGB
  }
  trait MandelbrotShader extends SingleValueShader {
    def value(cx:Double, cy:Double):Int = {
      var (x, y) = (cx, cy)
      //z(n+1) = z(n)^2 + c
      //(x+yi)*(x+yi) + (cx + cyi) = (x*x - y*y + cx) + (2 * x * y + cy) * i)
      var iter = 0
      while(x*x + y*y < 4 && iter < MAX_ITER) {
        val nx = x*x - y*y + cx
        val ny = 2*x*y + cy
        x = nx
        y = ny
        iter += 1;
      }
      iter
    }
  }

  abstract class MovingWindowShader(var cx:Double, var cy:Double, var rx:Double, var ry:Double) extends WindowedShader

  lazy val shader = new MovingWindowShader(cam.getCorner.x, cam.getCorner.y, cam.getViewportWidth, cam.getViewportHeight) with DefaultColorMap with MandelbrotShader

  override def draw() {
    shader.cx = cam.getCorner.x
    shader.cy = cam.getCorner.y
    shader.rx = cam.getViewportWidth
    shader.ry = cam.getViewportHeight

    loadPixels()
    for(x <- 0 until width; y <- 0 until height) {
      pixels(y*width+x) = shader.colorFor(x, y)
    }
    updatePixels()
    println(frameRate)
    pollSave() //check if the screen should be saved
  }

  override def keyPressed() {
    super.keyPressed(); //pressing space toggles a boolean variable to save the screen
  }
}