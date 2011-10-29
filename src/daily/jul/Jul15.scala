package daily
package jul

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 7/15/11
 * Time: 1:09 PM
 */
import processing.core._
import org.zhang.lib.world.HashWorld
import org.zhang.geom.Vec2
import org.zhang.lib.world.particle.Particle
import org.zhang.lib.{P5Util, WorldApplet}
import geomerative.{RG, RPoint}

class Jul15 extends NameApplet with WorldApplet {
  import PApplet._; import PConstants._;

  val world = new HashWorld(this)

  val name = getNameShape(110, "ariblk.ttf", CENTER, "Distractions")

  override def setup() {
    size(1024, 200)
    super.setup();
    name.translate(width/2, height * .7f)
    world ++= points.map{ case (p, t) => new MyParticle(Vec2(p.x, p.y), Vec2(t.x, t.y)) }
  }

  def points: Seq[(RPoint, RPoint)] = {
    val num = 400;
    for(i <- 0 until num)
      yield (name.getPoint(map(i, 0, num, 0, 1)),
             name.getTangent(map(i, 0, num, 0, 1)))
  }
                              //name.getPoints
                              //name.getHandles

  class MyParticle(p:Vec2, val tangent:Vec2) extends Particle(world, p, Vec2(), Seq()) {
    override def draw(p:PApplet) {
//      ellipse(loc, Vec2(5, 5));
      world.inCircle(loc, 36).foreach(f => line(loc, f.loc))
      line(loc, loc + tangent.setMag(5))
    }
  }

  override def draw() {
//    background(random(255), random(255), random(255));
    var withAlpha = createGraphics(width, height, JAVA2D)
    beginRecord(withAlpha);
    background(255, 0)
    noFill; stroke(0, 128);
//    setRShapeCenter(name, mouseX, mouseY)
//    world.zip(points).foreach{ case (e:MyParticle, (point, fl)) => e.loc = Vec2(point.x, point.y)}
//    setRShapeCenter(name, random(width), random(height))
//    name.transform(random(width)-name.getWidth()/2, random(height), name.getWidth(), name.getHeight())
//    name.drawVF()
    super.draw()
    endRecord();
    withAlpha.save("distractions.png")

//    loadPixels()
//    for(x <- )
//    updatePixels()

//    filter(BLUR, 2.5f)
  }
}