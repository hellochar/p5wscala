package daily
package jan

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 1/30/12
 * Time: 4:38 PM
 */

import processing.core._
import peasy.PeasyCam
import org.zhang.lib.{P5Util, MyPApplet}
import org.zhang.geom.{Vec2, Vec3}

class Jan30b extends MyPApplet with Savable with SphereUtils {

  import PApplet._;
  import PConstants._;
  implicit def d2f(d:Double) = d.toFloat

  val epsilon = 5e-3f;
  def deriv(func:Float => Vec3) = (t:Float) => (func(t+epsilon) - func(t))

  import org.zhang.lib._
  class Frenet(val pos:Float => Vec3) {
    val tangent =  memoize(deriv(pos) andThen (_.normalize))._1
    val norm = memoize(deriv(tangent) andThen (_.normalize))._1
    val binormal = memoize((t:Float) => tangent(t) cross norm(t))._1

    def origin(t:Float) = pos(t)
    def toWorld(t:Float)(vec:Vec3) = pos(t) + vec.onAxes(binormal(t), norm(t), tangent(t))
  }

  val frenet = new Frenet(memoize((t:Float) => Vec3(sin(t), cos(t), t) * 25)._1)

  def pos2(frame:Frenet)(t:Float) = frame.toWorld(t)(Vec2.fromPolar(15, 5*t).xy)

  lazy val cam = new PeasyCam(this, 100);
  override def setup() {
    size(500, 500, P3D)
    cam;
  }

  override def draw() {
    background(204);
    zhang.Methods.drawAxes(g, 100);
    noFill(); stroke(0);
    lines3(Range.Double(0, 20, .1) map {frenet.pos(_)})
    stroke(255);
    lines3(Range.Double(0, 20, .01) map {pos2(frenet)(_)})
    matrix {
      val t = (millis() / 1000f) % 20
      import frenet._
      translate(origin(t))

      stroke(255, 0, 0)
      line(tangent(t) * 20)

      stroke(0, 255, 0)
      line(norm(t) * 20)

      stroke(0, 0, 255)
      line(binormal(t) * 20)
    }
    pollSave() //check if the screen should be saved
  }

  override def keyPressed() {
    super.keyPressed(); //pressing space toggles a boolean variable to save the screen
  }
}