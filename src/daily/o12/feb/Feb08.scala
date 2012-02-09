package daily

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 2/8/12
 * Time: 9:16 PM
 */

import processing.core._
import org.zhang.lib.MyPApplet
import org.zhang.geom.Vec2

class Feb08 extends MyPApplet with Savable {

  import PApplet._;
  import PConstants._;

  class Mutable[T](var elem:T) {
    def get = elem
    def set(t:T) {
      elem = t
    }
  }
  implicit def mutable2Elem[T](m: Mutable[T]) = m.get
  implicit def elem2Mutable[T](t: T) = new Mutable(t)
  implicit def mutSeq2elemSeq[T](m: Seq[Mutable[T]]) = m.map(_.get)
  implicit def zhang2pv(z:Vec2) = new PVector(z.x, z.y)
  implicit def pv2zhang(p:PVector) = Vec2(p.x, p.y)

  def sgn(t:Float) = if(t<0) -1 else if(t == 0) 0 else 1

  case class Ray(loc:Vec2, norm:Vec2) {
    def draw() {
      line(loc, loc + norm * 1000 / cam.getScale)
    }
  }
  case class Intersection(loc:Vec2, norm:Vec2)
  trait Object {
    def intersect(r:Ray):Option[Intersection]
    def draw()
  }
  case class FunObject(f:Float => Vec2, numSamples:Int = 10) extends Object {
    def sampled = (0 to numSamples).map(_/numSamples.toFloat).sliding(2).map(x => Line(f(x(0)), f(x(1))))
    def intersect(r: Ray) = sampled.flatMap(_.intersect(r)).toSeq.sortBy(_.loc distTo r.loc).headOption

    def draw() {
      sampled foreach {_.draw()}
    }
  }

  /**
   * For now this class represents a line of infinite extent. We'll change it to be finite later.
   * @param a
   * @param b
   */
  case class Line(a:Mutable[Vec2], b:Mutable[Vec2]) extends Object {
    def offset = b - a

    /**
     * Projects the given point onto this line's infinite extent.
     * @param p
     * @return
     */
    def proj(p:Vec2) = ((p-a) proj offset) + a
    def closestDist(p:Vec2) = (proj(p) - p).mag

    def intersect(r: Ray) = {
      val cp = proj(r.loc)
      val ortho = r.loc - cp
      if((r.norm angleBetween ortho) > PI/2) { //we have a hit
        val loc = cp + (r.norm proj offset.normalize) * (ortho.mag / (r.norm proj ortho.normalize).mag)
        val far = ((loc - a) dot offset.normalize)

        if(far < 0 || far > offset.mag)
          None
        else
          Some(Intersection(loc, ortho.normalize))
      } else {
        None
      }
    }

    def draw() {
      line(a, b)
      noFill()
      stroke(255, 0, 0);
      ellipse(a, 5, 5);

      stroke(120, 120, 255)
      ellipse(b, 5, 5)
    }
  }

  var objects:Seq[Object] = Seq()
  var rayRoots:Seq[Ray] = Seq()
  lazy val cam = new zhang.Camera(this)
  override def setup() {
    size(500, 500)
    smooth()
    cam;
  }

  override def mouseVec = cam.model(super.mouseVec)

  /**
   * The Seq is a list of rays and the corresponding intersections at which they stopped; the second ray element
   * is optionally the exiting ray that never hits anything (be it because no objects intersected or that the bounds ended),
   * or None if the ray stopped before it could exit
   * @param ray
   * @param bound
   * @return
   */
  def followRay(ray:Ray, bound:Int) = {
    def followRayWithout(ray:Ray, bound:Int, ignored:Option[Object]):(Seq[(Ray, Intersection)], Option[Ray]) = {
      if(bound == 0) (Seq(), None)
      else {
        val totObj = ignored.map{i => objects.filter(i!=)}.getOrElse(objects)
        (totObj flatMap {x => x.intersect(ray).map((_, x))} sortBy {_._1.loc distTo ray.loc}).headOption match {
          case Some((i@Intersection(loc, norm), obj)) => {
            val newRay = Ray(loc, (-ray.norm) + (ray.norm - (ray.norm proj norm)) * 2)
            val (s, r) = followRayWithout(newRay, bound - 1, Some(obj))
            ((ray, i) +: s, r)
          }
          case None => {
            (Seq(), Some(ray))
          }
        }
      }
    }
    followRayWithout(ray, bound, None)
  }

  var lineStart:Option[Vec2] = None
  var rayStart:Option[Vec2] = None
  var selected:Option[Mutable[Vec2]] = None

  override def mousePressed() {
    lineStart = None
    rayStart = None
    selected = None
    if(mouseButton == LEFT) {
      selected = objects.collectFirst{
        case Line(a, b) if a.distTo(mouseVec) < 3 => a
        case Line(a, b) if b.distTo(mouseVec) < 3 => b
      }
      if(selected.isEmpty && keyCode == SHIFT)
        lineStart = Some(mouseVec)
    }
    else if(mouseButton == RIGHT)
      rayStart = Some(mouseVec)
  }

  override def mouseDragged() {
    if(selected.isEmpty && lineStart.isEmpty && rayStart.isEmpty) {
      objects :+= Line(mouseVec, new Mutable(cam.model(Vec2(pmouseX, pmouseY))))
    }
  }

  override def mouseReleased() {
    lineStart.filter(_.distTo(mouseVec) > 5) foreach {start =>
      objects :+= Line(start, mouseVec)
    }
    rayStart.filter(_.distTo(mouseVec) > 5) foreach {start =>
      rayRoots :+= Ray(start, (mouseVec - start).normalize)
    }

    lineStart = None
    rayStart = None
    selected = None
  }

  override def draw() {
    background(0)
    strokeWeight(1 / cam.getScale);

    stroke(255)
    objects foreach (_.draw())

    lineStart foreach { s =>
      stroke(255, 128);
      line(s, mouseVec)
    }
    rayStart foreach { s =>
      colorMode(HSB)
      stroke(rayRoots.length * 25 + 25, 128);
      line(s, mouseVec)
      colorMode(RGB)
    }
    selected foreach {s =>
      noFill(); stroke(255);
      ellipse(s, 15, 15);
      s.set(mouseVec);
    }

    noFill();
    colorMode(HSB)
    rayRoots.zipWithIndex foreach { case (ray, idx) =>
      followRay(ray, 100) match {
        case (s, last) => {
          s foreach { case (ray, i) =>
            stroke(idx*25, 255, 255);
            line(ray.loc, i.loc);
            fill(255, 128); noStroke();
            ellipse(i.loc, 5, 5)
            stroke(255)
            line(i.loc, i.loc + i.norm * 15)
          }
          last foreach {x =>stroke(idx*25, 255, 255); x.draw()}
        }
      }
    }
    colorMode(RGB)
//    objects flatMap {_.intersect(ray)} foreach {i =>
//      fill(255); noStroke();
//      ellipse(i.loc, 5, 5)
//      stroke(64, 255, 64)
//      line(i.loc, i.loc + i.norm * 15)
//    }

    pollSave() //check if the screen should be saved
  }

  override def keyPressed() {
    super.keyPressed(); //pressing space toggles a boolean variable to save the screen
  }
}