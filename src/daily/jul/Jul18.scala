package daily.jul

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 7/18/11
 * Time: 10:49 PM
 */
import processing.core._
import org.zhang.lib.WorldApplet
import org.zhang.lib.world.HashWorld
import org.zhang.geom.Vec2
import org.zhang.lib.world.particle.Particle

class Jul18 extends WorldApplet {
  import PApplet._; import PConstants._;

  val world = new HashWorld(this)

  def center = Vec2(width/2, height/2)

  override def setup() {
    size(800, 600)
    world ++= (0 until 5).map(i => new MyParticle(center + Vec2.fromPolar(25, random(TWO_PI))))
  }

  class MyParticle(loc:Vec2) extends Particle(world, loc, Vec2()) {

  }

  override def draw() {
  }
}