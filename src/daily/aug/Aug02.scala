package daily.aug

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 8/2/11
 * Time: 1:04 AM
 */
import processing.core._
import org.zhang.geom.Vec2
import org.zhang.lib.world.action.Action
import org.zhang.lib.world.particle.Particle
import org.zhang.lib.{P5Util, WorldApplet}
import org.zhang.lib.world._

class Aug02 extends PApplet with WorldApplet {
  import PApplet._; import PConstants._;

  def wrap(v:Vec2) = v.doToBoth(zhang.Methods.wrap _)(Vec2(width, height))
  def indexOf(v2:Vec2) = { val v = wrap(v2); val i = (v.y.asInstanceOf[Int])*width+v.x.asInstanceOf[Int]; if(i > width*height) println("WTF: "+i+", "+v2+", "+v); i }

  trait TorusWorld extends BoundedWorld {
    override def defaultHitsBounds(v: Velocity) {
      v.loc = wrap(v.loc)
    }
  }

  val world = new HashWorld(this) with TorusWorld

  lazy val cells:Array[Cell] = Array.ofDim(width*height)
  def cells(v:Vec2):Cell = cells(indexOf(v))

  class Cell(val ix:Int, val iy:Int, var value:Vec2) extends Entity(world, 5) with Location {
    val loc = Vec2(ix, iy)
    private var newValue:Vec2 = Vec2()

    lazy val neighbors = ((-1 to 1).map(dx =>
      (-1 to 1).map(dy =>
        Vec2(dx, dy))).flatten)
      .map(v => cells(loc + v))

    override def run() {
      //for every neighbor and yourself, sum up the value vectors, divide by 9, set that as your new value
      newValue = neighbors.map(_.value).reduceLeft(_ + _) / 9
    }

    override def update() {
      value = newValue
    }

    override def draw(p:PApplet) {
      val c = color(map(value.angle, 0, TWO_PI, 0, 255), 255, map(value.mag, 0, 25, 0, 255))
      p.pixels(iy*width+ix) = c
    }
  }
  //There are a bunch of particles, and then there's a vector field denoting the force that a particle should feel
  //on every iteration. The particle also affects the vector field in some way -
  // a) some sort of energy conservation thing
  //The field also has some sort of stablization or something?

  object VFAction extends Action {
    def apply(v1: Particle) = {
      val i = indexOf(v1.loc)
      v1.addForce(cells(i))
      cells(i).value /= 2
    }
  }

  override def setup() {
    size(500, 500)
    colorMode(HSB)
    world ++= (0 until width).map(x => (0 until height).map(y => {
      val k = new Cell(x, y, Vec2(x, y).invR2(5, Vec2(width/2, height/2)))
      cells(y*width+x) = k;
      k})).flatten
//    world ++= (0 until 100) map(_ => new MyParticle())
  }

  class MyParticle extends Particle(world, P5Util.randomVector(this), Vec2(), Seq(VFAction)) {

    override def draw(p:PApplet) {
      ellipse(x, y, 5, 5)
    }
  }

  override def draw() {
    loadPixels
    super.draw()
    updatePixels
    println(frameRate)
  }

}