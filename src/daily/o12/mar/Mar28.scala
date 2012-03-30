package daily

import processing.core._
import org.zhang.lib.MyPApplet
import org.zhang.geom.Vec3
import peasy.PeasyCam

class Mar28 extends MyPApplet with Savable {

  import PApplet._;
  import PConstants._;

  class Point(var v:Vec3) {
    def add(v:Vec3) = {this.v += v; this}
    def sub(v:Vec3) = {this.v -= v; this}
    def times(f:Float) = {this.v *= f; this}

    def x = v.x
    def y = v.y
    def z = v.z

    def x_=(xx:Float) { v = v.copy(x = xx) }
    def y_=(yy:Float) { v = v.copy(y = yy) }
    def z_=(zz:Float) { v = v.copy(z = zz) }

    override def toString = v.toString
  }
  implicit def p2v(p:Point) = p.v

  class Face(val points:Seq[Point]) /*extends Transformable[Face]*/ {
//    def transform(t: Transform) = Face(points map (_.transform(t)))

    protected def mutations:Set[() => Unit] = Set(
      () => {},
      () => {val norm = normal * random(.1f, .5f); points.foreach(_ add norm) }
    )

    def normal = ((points(1) - points(0)) cross (points.last - points(0))).normalize

    def draw() {
//      stroke(0, 255, 0);
      fill(128, 128);
      lines3(points.map{_.v}, true)
    }

    def mutate() {
      if(!mutations.isEmpty)
        org.zhang.lib.random(mutations)()
    }
  }

  class Roof(points:Seq[Point]) extends Face(points) {
    override def mutations = super.mutations + (() => {
      def rotateLeft[T](s:Seq[T], num:Int):Seq[T] = if(num == 0) s else rotateLeft(s.tail :+ s.head, num-1)
      val Seq(pt1, pt2, _*) = rotateLeft(points, randi(points.size - 1))
      val amount = random(.5f, 1.5f)
      pt1.z *= amount
      pt2.z *= amount
    })
  }
  class Floor(points:Seq[Point]) extends Face(points) {
    override val mutations = Set.empty[() => Unit]
  }

  trait Volume {
    def draw()
    def mutate()
  }

  trait Node extends Volume {
    def seq:Seq[Volume]

    def draw() {
      seq foreach {_.draw()}
    }

    def mutate() {
      seq foreach {_.mutate()}
    }
  }

  trait Cube extends Volume {
    def points:Seq[Point]
  }
  class LeafCube(val points:Seq[Point]) extends Cube {
//    val points = for(x <- List(0, 1); y <- List(0, 1); z <- List(0, 1)) yield new Point(Vec3(x, y, z))

//    implicit def seqInts2seqPts(s:Seq[Int]) = s map { points }
    implicit def int2Point(i:Int) = points(i)
    val (faces, roof, floor) = {
      val s = Seq(
        new Face(Seq(0, 2, 3, 1)),
        new Face(Seq(7, 3, 2, 6)),
        new Face(Seq(4, 6, 7, 5)),
        new Face(Seq(4, 0, 1, 5))
      )
      val roof = new Roof(Seq(1, 3, 7, 5))
      val floor = new Floor(Seq(0, 2, 6, 4))
      (s :+ roof :+ floor, roof, floor)
    }

    def draw() {
      faces foreach (_.draw())
    }

    def mutate() {
      faces.foreach(_.mutate())
    }
  }
  class BranchCube extends Cube with Node {
    var seq = Seq()
    def mutate() {

    }

    def points = null
  }

  var cube = new Cube()
  lazy val cam = new PeasyCam(this, 100);
  override def setup() {
    size(500, 500, P3D)
    cam
  }

  override def draw() {
    background(204)
    zhang.Methods.drawAxes(g, 25);
    matrix {
      scale(25);
      cube.draw()
    }
    pollSave() //check if the screen should be saved
  }

  override def keyPressed() {
    super.keyPressed(); //pressing space toggles a boolean variable to save the screen
    cube = new Cube()
  }
}