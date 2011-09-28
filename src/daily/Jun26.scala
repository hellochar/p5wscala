package daily

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 6/26/11
 * Time: 12:58 AM
 */
import processing.core._
import org.jbox2d.collision.AABB
import org.jbox2d.dynamics.World
import org.jbox2d.common.Vec2
import zhang.JBoxRenderer

class Jun26 extends JBoxRenderer {
  import PApplet._; import PConstants._;

  val myWorld = new MyWorld(initWorld(-100, -100, 100, 100, 0, 12))

  override def setup() {
    size(500, 500)
  }

  override def draw() {
  }

}

class MyWorld(world:World) {
  def getWorld = world
  def run() {

  }
  def draw(r:JBoxRenderer) {
    if(r.getWorld == world) r.render()
  }
}

abstract class MyEntity {

  def getBody
  def run()
  def draw(r:JBoxRenderer)
}