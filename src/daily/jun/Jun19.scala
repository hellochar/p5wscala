package daily.jun

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 6/19/11
 * Time: 5:50 AM
 */
import processing.core._
import org.zhang.lib.world.particle.Particle
import org.zhang.geom.Vec2
import org.zhang.lib.world.action.{Action, RunawayAction, ClosestParticleAction}
import org.zhang.lib.{HasCamera, P5Util, WorldApplet}
import org.zhang.lib.world._

class Jun19 extends WorldApplet with HasCamera {

  val world = new HashWorld(this) with BoundedWorld
  import PApplet._; import PConstants._;
  //TowardsAttractorAction
  private sealed class TAA(pow:Float) extends Action {
    def apply(v1: Particle) = {
      def forceFor(pos:Vec2) = Vec2.invR2(v1.loc, pos) * pow
      val f = (v1 match {
        case a: Attractor => world.ofType(classOf[Attractor]) - a
        case _ => world.ofType(classOf[Attractor])
      }).map(_.loc).foldLeft(Vec2(0)){case (force, pos) => force + forceFor(pos)}
//      println(f)
      v1.addForce(f)
    }
  }

  //Attractor 2 Attractor
  private object A2A extends TAA(50)
  //Dot 2 Attractor
  private object D2A extends TAA(8000)

  class Attractor(l:Vec2, v:Vec2) extends Particle(world, l, v, Set(A2A)) {
    override def draw(p: PApplet) {
      p.fill(255);
      P5Util.ellipse(p, loc, 25, 25) //todo: maybe make a cool looking gravity distortion thing.
    }
  }

  class Dot(l:Vec2) extends Particle(world, l, Vec2(0), Set(D2A)) with SpecialBounded {

    override def draw(p:PApplet) {
      super.draw(p);
      val speed = vel.mag
      val r = 23 * speed
      val g = 16 * speed
      p.fill(r, g, 0)
      P5Util.ellipse(p, loc, 3, 3)
    }

    def whenHitsBounds = {
      world -= this
      world += new Dot(P5Util.randomVector(Jun19.this))
    } //ignore boundaries
  }

  override def setup() {
    size(500, 500)
    world ++= (0 until 3).map(i => new Attractor(P5Util.randomVector(this), Vec2.fromPolar(4, random(TWO_PI))))
    world ++= (0 until 250).map(i => new Dot(P5Util.randomVector(this)))
    super.setup
    smooth
  }

  override def draw() {
    background(0); noStroke;
    super.draw()
  }
}