package daily

import processing.core._
import org.zhang.lib.{WorldApplet, MyPApplet}
import org.zhang.lib.misc.Vec2
import org.zhang.lib.world.{Entity, Velocity, HashWorld}

/**
* Create a little environment full of critters that try to dominate over each other. Critters
* are made up of some collection of shapes; each shape is one of three colors. When different
* critters touch each other, the shapes that are touching will interact according to a rock-paper-
* scissors type thing such that the losing color will get destroyed while the winning color stays.
*
* Critters will create offspring every once in a while; the offspring will have mostly the same
* color pattern as their parents, but with the occasional mutation. This may be a mutation of
* the color of a shape, of the size/dimension of the shape. It might include losing a shape
* or adding a new one. (Consider having critters that have levels of symmetry)
*
* Critters need resources to survive - right now just food and water. [As of 4:38 PM 8/30/2011, food
* randomly spawns and there are various pools of water.] Critters won't really notice each other.
*/
class Aug30 extends MyPApplet with Savable with WorldApplet {
  import PApplet._; import PConstants._;

  val world = new HashWorld(this)

  override def setup() {
    size(500, 500)

  }

  override def draw() {
    pollSave("Aug30-")
  }
  
  override def keyPressed() {
    super.keyPressed();
  }

  case class Shape(rad:Float, t:Type) {

  }

  sealed abstract class Type(val beats:Type, val c:Int)
  object Red extends Type(Green, 0xFFFF0000)
  object Green extends Type(Blue, 0xFF00FF00)
  object Blue extends Type(Red, 0xFF0000FF)
  implicit def type2Color(t:Type) = t.c
  val types = Set(Red, Green, Blue)

  class Critter private (var loc:Vec2, var vel:Vec2 = Vec2(), val shapes:Seq[Shape]) extends Entity(world) with Velocity {
    def this(locc:Vec2, s:Shape) = this(locc, Vec2(), Seq(s))

    private var hunger, thirst = 0;

    override def run() {
      //find the nearest food
    }

    override def draw(p: PApplet) {

    }

    override def update() {
      super.update()
    }
  }

  class Food(val loc:Vec2, var amt:Float = 500) {
    
  }
}