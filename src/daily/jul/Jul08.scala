package daily.jul

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 7/8/11
 * Time: 2:20 PM
 */
import processing.core._
import org.zhang.lib._
import org.zhang.lib.misc.Vec2
import org.zhang.lib.{P5Util, HasMV, WorldApplet}
import org.zhang.lib.world.{HashWorld, BoundedWorld}
import world.action.Action
import world.particle.Particle
import zhang.Camera

class Jul08 extends WorldApplet with HasMV {
  import PApplet._; import PConstants._;

  var world = new HashWorld(this)

  override def setup() {
    println("setup "+getClass.getName)
    size(500, 500)
    super.setup()
    new Camera(this)
  }

  private class MyParticle(locc:Vec2) extends Particle(world, locc, Vec2(), Seq(Noise2VelEvolver)) {

    override def draw(p:PApplet) {
      ellipse(x, y, 5, 5)
    }

    override def run() {
      super.run()
    }
  }

  private sealed trait Vec2Evolver extends Action {
    def func(v:Vec2): Vec2
  }

  def scl = .15f;

  private object Noise2PixelEvolver extends Vec2Evolver {

    def apply(v:Particle) = v.loc = func(v.loc)

    def func(v: Vec2): Vec2 = {
      val func2Vec2 = (f: Float) => ((p: Float) => Vec2(p % width, p / width))(map(f, 0, 1, 0, width * height))
      func2Vec2(noise(v.x * scl, v.y * scl))
    }
  }

  private object Noise2VelEvolver extends Vec2Evolver {
    def velScl = 45

    def apply(v:Particle) = v.vel = func(v.loc);

    implicit def d2f(d:Double) = d.asInstanceOf[Float]
    def func(v:Vec2) = Vec2(noise(v.x * scl, v.y * scl, 1)-.5, noise(v.x * scl, v.y*scl, 2)-.5) * velScl
  }

  private object Noise2ForceEvolver extends Vec2Evolver {
    override def apply(v:Particle) = v.addForce(v.loc)

    def func(v: Vec2) = Noise2VelEvolver.func(v) * .025f;
  }

  override def draw() {
    background(0);
    noFill(); stroke(255); rect(0, 0, width, height)
//    fill(0, 40); rect(0, 0, width, height); noStroke; fill(255)
    fill(255);
    super.draw()
    if(mousePressed) world += new MyParticle(mouseVec)
  }

}