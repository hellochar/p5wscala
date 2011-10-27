package daily.jul

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 7/11/11
 * Time: 11:35 PM
 */
import processing.core._
import org.zhang.lib.world.particle.Particle
import org.zhang.lib.misc.Vec2
import org.zhang.lib.world.{TimedLife, HashWorld}
import scala.util.Random
import org.zhang.lib.{HasMV, P5Util, WorldApplet}
import peasy.PeasyCam
import zhang.Camera

class Jul11 extends WorldApplet with HasMV {
  import PApplet._; import PConstants._;

  val world = new HashWorld(this)
//  val cam:PeasyCam = new PeasyCam(this, 500)

  override def setup() {
    size(500, 500)
    background(0)
    new Camera(this)
  }

  override def draw() {
    background(0)
    noStroke()
//    stroke(255, 64)
    fill(255, 120)
    super.draw()
  }

  def vertex(v:Vec2) { vertex(v.x, v.y) }

  val defaultThick = (k:Float) => pow(k, 1/6f)

  override def mousePressed() {
//    for(i <- 0 to 5)
      world += new MyParticle(mouseVec, random(TWO_PI), defaultThick, 4, 7000)
  }

  def chooseAngle(parent:MyParticle) = parent.angleMode match {
    case 0 =>
      parent.angle + (if(rand.nextBoolean()) PI/2 else -PI/2) + radians(random(-4, 4))
    case 1 =>
      parent.angle + radians(if(rand.nextBoolean()) 25 else -25)
  }

  def birthMult = random(.05f, .1f)

  val rand = new Random()
  val speedInit = 4
  class MyParticle(locc:Vec2, dir:Float, val thick:(Float) => Float, val thickMult:Float, val millisAlive:Float)
    extends Particle(world, locc, Vec2.fromPolar(speedInit, dir), Seq())
    with TimedLife {
    var nextBirth = millisAlive * birthMult
//    val angleMode = random(2).asInstanceOf[Int]
    val angleMode = 0
    var segments:Seq[(Vec2, Vec2)] = Seq()

    if(thickMult < 1) remove()

    def this(parent:MyParticle) =
      this(parent.loc,
           chooseAngle(parent),
           parent.thick,
           parent.thickness / 3,
           parent.millisAlive / 2)

    def thickness = thickMult * thick(map(lifeTime, 0, millisAlive, 1, 0))

    def getSegment = (loc + Vec2.fromPolar(thickness, angle + PI / 2), loc - Vec2.fromPolar(thickness, angle + PI / 2))

    override def draw(p:PApplet) {
//      val s = getSegment

      beginShape(TRIANGLE_STRIP)
      segments.foreach{ case (a, b) => {vertex(a); vertex(b);} }
      endShape()
//      P5Util.line(Jul11.this, s._1, s._2)
//      P5Util.ellipse(Jul11.this, loc, 5, 5)
    }

    var dead = false

    override def remove() {
      dead = true
    }

    def onSnow: Boolean = {
//      brightness(get((x+vel.x).asInstanceOf[Int], (y+vel.y).asInstanceOf[Int])) > 100
      brightness(get((x).asInstanceOf[Int], (y).asInstanceOf[Int])) > 0
    }

    override def run() {
      if(!dead) {
        if(onSnow) { remove(); return; }
        super.run()
        vel.setMag(vel.mag * random(.95f, 1f))
        if (!dead && lifeTime > nextBirth) {
          world += new MyParticle(this)
          nextBirth += random(64, 128)
        }
        segments +:= getSegment
      }
    }
  }


}