package daily

import processing.core._
import org.zhang.lib.MyPApplet
import org.zhang.geom.Vec3
import peasy.PeasyCam

class Mar28 extends MyPApplet with Savable {

  import PApplet._;
  import PConstants._;
  import org.zhang.lib._;
//
//  class Point(var v:Vec3) {
//    def add(v:Vec3) = {this.v += v; this}
//    def sub(v:Vec3) = {this.v -= v; this}
//    def times(f:Float) = {this.v *= f; this}
//
//    def x = v.x
//    def y = v.y
//    def z = v.z
//
//    def x_=(xx:Float) { v = v.copy(x = xx) }
//    def y_=(yy:Float) { v = v.copy(y = yy) }
//    def z_=(zz:Float) { v = v.copy(z = zz) }
//
//    override def toString = v.toString
//  }
//  implicit def p2v(p:Point) = p.v
//
//  class Face(val points:Seq[Point]) /*extends Transformable[Face]*/ {
////    def transform(t: Transform) = Face(points map (_.transform(t)))
//
//    protected def mutations:Set[() => Unit] = Set(
//      () => {},
//      () => {val norm = normal * random(-.8f, .8f); points.foreach(_ add norm) }
//    )
//
//    def normal = ((points(1) - points(0)) cross (points.last - points(0))).normalize
//
//    def draw() {
////      stroke(0, 255, 0);
//      fill(128, 128);
//      lines3(points.map{_.v}, true)
//    }
//
//    def mutate() {
//      if(!mutations.isEmpty)
//        org.zhang.lib.random(mutations)()
//    }
//  }
//
//  class Roof(points:Seq[Point]) extends Face(points) {
//    override def mutations = super.mutations + (() => {
//      def rotateLeft[T](s:Seq[T], num:Int):Seq[T] = if(num == 0) s else rotateLeft(s.tail :+ s.head, num-1)
//      val Seq(pt1, pt2, _*) = rotateLeft(points, randi(points.size - 1))
//      val amount = random(.5f, 1.5f)
//      pt1.z *= amount
//      pt2.z *= amount
//    })
//  }
//  class Floor(points:Seq[Point]) extends Face(points) {
//    override val mutations = Set.empty[() => Unit]
//  }


  def calcNormal(points:Seq[Vec3]) = {
    ((points(1) - points(0)) cross (points.last - points(0))).normalize
  }
  /**
   * The points are indexed like this
   * @param points
   */
  case class Cube(points:Seq[Vec3]) {
    require(points.length == 8)

    implicit def int2vec3(i:Int) = points(i)
    implicit def ti2tvec3(t:Seq[Int]) = t.map(points)

    def apply(i:Int) = points(i)

    /**
     * All faces should be defined using the LEFT-HAND rule for their outward pointing normal
     */
    val roof = Seq(1, 5, 7, 3)
    val floor = Seq(0, 2, 6, 4)
    val left = Seq(0, 1, 3, 2)
    val right = Seq(4, 6, 7, 5)
    val near = Seq(3,7,6,2)
    val far = Seq(1,0,4,5)

    val sides = Seq(left, near, right, far)
    val faces = sides :+ roof :+ floor

    def isRoofAngled = (calcNormal(roof) - Vec3.Z).withinBounds(1e-4f)

    def replaced(map:Map[Int, Vec3]) = {
      var np = points
      map foreach { case (i, v) =>
        np = np.updated(i, v)
      }
      Cube(np)
    }

    def shouldPrune =
      ((calcNormal(roof) angleBetween Vec3.Z) > PI/4) ||
      ((v:Vec3) => abs(v.x) < .1 || abs(v.y) < .1 || abs(v.z) < .1)(points(0) - points(7))


    def draw() {
      faces foreach (lines3(_))
    }

  }
  object Cube {
    def apply():Cube = apply(for(x <- List(0, 1); y <- List(0, 1); z <- List(0, 1)) yield Vec3(x, y, z))
  }

  def slopeRoof(c:Cube, amount:Float = random(.5f, 1.5f)) = {
    import c._
    def rotateLeft[T](s:Seq[T], num:Int):Seq[T] = if(num == 0) s else rotateLeft(s.tail :+ s.head, num-1)
    val Seq(pt1, pt2, _*) = rotateLeft(roof, randi(roof.size))
    Map(pt1 -> pt1.copy(z = pt1.z * amount),
        pt2 -> pt2.copy(z = pt2.z * amount))
  }

  def extrude(c:Cube, amount:Float = random(-1, 1)) = {
    import c.{points => _, _}
    val points = org.zhang.lib.random(sides)
    val normal = calcNormal(points.map(c(_)))
    val norm = normal * random(-.8f, .8f);
    points.map(p => p -> (c(p) + norm)).toMap
  }

  def cut(c:Cube, s1:Seq[Int], s2:Seq[Int], f:Float = random(.2f, .8f)) = {
    import c._
    val lerp = (x:Vec3, y:Vec3) => x + (y - x) * f
    //left will hold the left face for the right cube; similarly, it's the right face for the left cube
    val face = (s1 zip s2) map{ case (x, y) => lerp(x, y) }

    Set(s1.zip(face).toMap, s2.zip(face).toMap)
  }
  def cutX(c:Cube, f:Float = random(.2f, .8f)) = cut(c, Seq(0, 2, 3, 1), Seq(4, 6, 7, 5), f)
  def cutY(c:Cube, f:Float = random(.2f, .8f)) = cut(c, Seq(0, 1, 4, 5), Seq(2, 3, 6, 7), f)
//  def cutZ(c:Cube, f:Float = random(.2f, .8f)) = cut(c, Seq(1, 3, 7, 5), Seq(0, 2, 6, 4), f)

  def randomTransform(c:Cube):Set[Cube] = {
    implicit def m2sc(m:Map[Int, Vec3]) = Set(c.replaced(m))
    implicit def ms2sc(s:Set[Map[Int, Vec3]]) = s.map{c.replaced(_)}

    var transforms = Set[Set[Cube]](extrude(c), cutX(c), cutY(c), /*cutZ(c),*/ Set())
    /*if(!c.isRoofAngled)*/ transforms += slopeRoof(c)

//    randi(0, 5) match {
//      case 0 => slopeRoof(c)
//      case 1 => extrude(c)
//      case 2 => cutX(c)
//      case 3 => cutY(c)
//      case 4 => cutZ(c)
//      case 5 => Set()
//    }


    /**
     * House = {
     *  Roof[z = [.8, 1]]
     *  Side[]
     *  Side[]
     *  Side[]
     *  Side[]
     *  Side[]
     *
     * }
     */



    org.zhang.lib.random(transforms)
  }

  var cubes:Set[Cube] = Set(Cube())
  lazy val cam = new PeasyCam(this, 100);
  override def setup() {
    size(500, 500, P3D)
    cam
  }

  override def draw() {
    background(204)
//    lights()
    zhang.Methods.drawAxes(g, 25);
    matrix {
      scale(25);
//      fill(128, 128); stroke(0, 255, 0)
      fill(255); noStroke();
      lights(); rotateX(.1f); rotateY(.1f);
      cubes foreach {_.draw()}
    }
    pollSave() //check if the screen should be saved
  }

  override def keyPressed() {
    super.keyPressed(); //pressing space toggles a boolean variable to save the screen
    if(key == 'z') {
      cubes = cubes flatMap {randomTransform(_)} filterNot (_ shouldPrune)

      if(cubes == Set()) cubes = Set(Cube())
      else {
        val newOrigin = cubes.flatMap{_.points}.minBy(_ mag2)
        cubes = cubes map {c => Cube(c.points map {_ - newOrigin})}
      }
    } else if(key == 'r') {
      cubes = Set(Cube())
    }
  }
}