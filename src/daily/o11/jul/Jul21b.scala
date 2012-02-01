package daily

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 7/21/11
 * Time: 3:42 AM
 */
import processing.core._
import zhang.Camera

class Jul21b extends PApplet {
  import PApplet._;

  //The idea is that you "bend" an axis into the shape of a parametric curve (probably in 2D)
  //so for instance, if you have just a straight line in the reference axis, and you bend the axis into
  //a sin wave, your straight line would be bent into a sin wave looking thing.

  override def setup() {
    size(500, 500)
    val cam = new Camera(this)
  }

  type Curve = Float => Float
  def derivative(c:Curve, epsilon:Float = 1e-10f) = (x:Float) => (c(x+epsilon) - c(x))/epsilon

  type Parametric = Float => (Float, Float)

  val myParametric = (t:Float) => (sin(t), sqrt(t+cos(t/3)))

  def drawParametric(p:Parametric, start:Float = 0, end:Float = 1) {
    val vtx = Function.tupled((vertex _): (Float, Float) => Unit)
    beginShape()
    (0 until 1000).map(map(_, 0, 1000, start, end)).map(myParametric).foreach(vtx)
    endShape()
  }

  override def draw() {
    background(204);
//    zhang.Methods.strokeWeightScreen(this, 1, c)
    strokeWeight(0)
    noFill()
    drawParametric(myParametric, 0, 100)
  }
}