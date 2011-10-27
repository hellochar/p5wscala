package daily
package jun

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 6/20/11
 * Time: 11:36 PM
 */
import processing.core._
import org.zhang.lib.world.particle.Particle
import zhang.Methods
import org.zhang.lib.misc.Vec2
import org.zhang.lib.world.{BoundedWorld, HashWorld}
import geomerative.{RPoint, RShape}
import org.zhang.lib.{HasCamera, P5Util, WorldApplet}

class Jun20 extends WorldApplet with NameApplet with HasCamera {
  import PApplet._; import PConstants._;
  implicit def rpoint2vec2(p:RPoint) = Vec2(p.x, p.y)

  val world = new HashWorld(this)

  var name:RShape = _

  override def setup() {
    size(1024, 300)
    super.setup()
    name = getNameShape(140)
    name.translate(width/2, height/2)
//    smooth()
    world ++= (0 until 300).map(map(_, 0, 300, 0, 1)).map(i => new Particle(world, name.getPoint(i), Vec2(), Seq()) {
      override def draw(p:PApplet) {
        p.ellipse(x, y, 5, 5)
        val particles = world.inCircle(loc, 40) - this
        particles map(_.loc) foreach(P5Util.line(Jun20.this, loc, _))
//        vel = vel.setMag(0 / (particles.size+1))
      }
    })
  }

  override def draw() {
    background(255);
    fill(0); stroke(0)
    super.draw()
    noFill();
    rect(0, 0, 255, 255);
//    println(frameRate)
  }

}