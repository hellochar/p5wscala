package daily

import processing.core._
import org.zhang.lib.MyPApplet
import org.zhang.geom.Vec3
import peasy.PeasyCam

class May10 extends MyPApplet with Savable {

  import PApplet._;
  import PConstants._;

  class SetGrowable(s:Set[Node]) {
    def grow(g:Growth) = s.flatMap(_.grow(g))
  }
  implicit def setNode2setGrowable(s:Set[Node]) = new SetGrowable(s)

  class Node(var pos:Vec3, var children:Set[Node] = Set()) {

    def draw() {
      children foreach {x => at(pos) {
          rotateAtoB(Vec3.Z, x.pos - pos)
        val z = x.pos distTo pos
          cylinder(z/20, z, z/20, true, 1);
          //line(pos, x.pos)
        }
      }
      children foreach {_.draw()}
    }

    //Returns the newly added children to this Node.
    def grow(g:Growth) = {
      val c = g(this) //c is the set of new children to add to this Node
      children ++= c
      c
    }
  }

  type Growth = Node => Set[Node]

  trait GrowthConstructor[T] {
    def apply(t:T): Growth

    def randomGrowth = apply(randomParams)
    protected def randomParams:T
  }
  object OffsetGrowth extends GrowthConstructor[Vec3] {
    def apply(offset:Vec3) = n => Set(new Node(n.pos + offset))

    protected def randomParams = Vec3.random
  }
  object FlowerGrowth extends GrowthConstructor[(Vec3, Vec3, Float, Int)] {
    def apply(t:(Vec3, Vec3, Float, Int)) = t match { case (up, side, ang, num) => {
        val perp = (up cross side).normalize
        val first = up.rotate(perp, ang)
        n => ((0 until num) map {i => new Node(n.pos + first.rotate(up.normalize, i*TWO_PI/num))}).toSet
      }
    }
    def randomParams = (Vec3.random, Vec3.random, random(0f, PI), randi(2, 8))
  }

  val main = new Node(Vec3())

  lazy val cam = new PeasyCam(this, 100);
  override def setup() {
    size(500, 500, P3D)
    cam
    main.grow(OffsetGrowth(Vec3(0, 0, 100))).grow(FlowerGrowth((Vec3.Z * 60, Vec3.X, PI/4, 6))).grow(FlowerGrowth(Vec3.Z * 40, Vec3.Y, PI/4, 3)).grow(OffsetGrowth(Vec3(0, 0, 10)))
  }

  override def draw() {
    background(204)
    zhang.Methods.drawAxes(g, 100)
    ambient(32)
    directionalLight(196, 196, 196, Vec3(1, 0, -1))
    noStroke()
    main.draw()
    pollSave() //check if the screen should be saved
  }

  override def keyPressed() {
    super.keyPressed(); //pressing space toggles a boolean variable to save the screen
  }
}