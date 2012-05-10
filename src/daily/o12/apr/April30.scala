package daily

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 1/30/12
 * Time: 2:44 AM
 */

import processing.core._
import org.zhang.lib.MyPApplet
import peasy.PeasyCam
import org.zhang.geom.{Vec3, Vec2}

class Bezier[T <: Mutable[Vec2]](var points: Seq[T]) {
  def lerp(a: Vec2, b: Vec2, t: Float) = a + (b - a) * t;

  protected def bezier(pts: Seq[Vec2])(t: Float): Option[Vec2] = pts match {
    case Seq(a, b) => Some(lerp(a, b, t))
    case s if s.length < 2 => None;
    case s => bezier(s.sliding(2).map {
      x => lerp(x(0), x(1), t)
    }.toSeq)(t)
  }

  def curveAt = bezier(points) _

  def sampled = Range.Double(0, 1, .01) flatMap {
      x => curveAt(x.toFloat)
    }
}

trait BezierApplet[T <: Mutable[Vec2]] extends MyPApplet { app =>

  def construct(v:Vec2):T

  def bez: Option[Bezier[T]]
  var selected: Option[T] = None

  @inline
  val POINT_RAD = 10

  def drawBezier() {
    bez foreach { k =>
      lines2(k.sampled)
    }
  }

  override def draw() {
    background(0);
    smooth();

    bez foreach { bez =>
      noFill();
      //draw thin bezier
      stroke(255, 128);
      strokeWeight(1);
      drawBezier()

      //draw pulse
      fill(255);
      noStroke();
      bez.curveAt((millis() / 1800f) % 1) foreach {
        ellipse(_, 10, 10)
      }

      //draw the hull of all the points
      noFill();
      stroke(255);
      strokeWeight(3);
      lines2(bez.points);

      //draw individual points
      noStroke();
      fill(255, 220, 0);
      bez.points foreach {
        ellipse(_, POINT_RAD * 2, POINT_RAD * 2)
      }

      //draw selection bar
      selected foreach {
        noFill; stroke(255); strokeWeight(1); ellipse(_, POINT_RAD * 4, POINT_RAD * 4)
      }
    }
  }

  override def mousePressed() {
    bez foreach { bez =>
      selected = bez.points.find(x => (x.get distTo mouseVec) < POINT_RAD)
      if (!selected.isDefined) {
        selected = Some(construct(mouseVec))
        bez.points :+= selected.get;
      }
    }
  }

  override def mouseDragged() {
    selected foreach {
      _.set(mouseVec)
    }
  }

}

class MutableRadial(t:Vec2, width:Float, height:Float) extends Mutable[Vec2](t) {
  val curve = new Bezier[Mutable[Vec2]](Seq(new Mutable(Vec2(0, height/2)), new Mutable(Vec2(width, height/2))))

  //mutates curve's points to be a)sorted by x, b) have the same "point" at the start and the end
  def reorder() {
    curve.points = curve.points.sortBy(_.x)
    curve.points.head.set(Vec2(0, curve.points.head.y))
    curve.points.last.set(Vec2(width, curve.points.head.y))
  }

  def radius(theta:Float) = {
    val p = curve.curveAt(theta/PConstants.TWO_PI).get
    PApplet.pow(2, PApplet.map(p.y, 0, height, -5, 5));
  }
}

class April30 extends BezierApplet[MutableRadial] with Savable {
  app =>

  import PApplet._;
  import PConstants._;

  def construct(v: Vec2) = new MutableRadial(v, RadialControl.width, RadialControl.height)

  val bez = Some(new Bezier[MutableRadial](Seq()))

  override def setup() {
    size(400, 300)
    PApplet.runSketch(Array("3D Surface"), Draw3D);
    PApplet.runSketch(Array("Radial Control"), RadialControl);
  }

  override def draw() {
    selected foreach {s =>
      s.reorder();
    }
    super.draw()
//    println(bez.get.points)
//    println(bez.get.curveAt(.5f))
  }

  override def mousePressed() {
    val selNow = selected;
    super.mousePressed();
    if(selected != selNow) { //selection has changed
      RadialControl.selected = None
    }
  }

  def lerpRadius(a: Float, b: Float, t: Float) = a + (b - a) * t;

  protected def bezierRadius(pts: Seq[Float])(t: Float): Option[Float] = pts match {
    case Seq(a, b) => Some(lerp(a, b, t))
    case s if s.length < 2 => None;
    case s => bezierRadius(s.sliding(2).map {
      x => lerp(x(0), x(1), t)
    }.toSeq)(t)
  }

  def radiusAt(theta:Float) = bezierRadius(bez.get.points.map(_.radius(theta))) _

  def sampledRadiusFunc(theta:Float) = Range.Double(0, 1, .01) flatMap {
    x => radiusAt(theta)(x.toFloat)
  }
  val (sampledRadius, sampledRadiusMap) = org.zhang.lib.memoize(sampledRadiusFunc _)

  private object RadialControl extends BezierApplet[Mutable[Vec2]] {
    def bez = app.selected.map{_.curve}
    def construct(v: Vec2) = new Mutable(v)

    override def setup() {
      size(400, 300);
    }

    override def draw() {
      super.draw();

      stroke(255); strokeWeight(.5f);
      horiz(height/2)
    }

    override def mouseDragged() {
      super.mouseDragged();
      sampledRadiusMap.clear()
    }

    override def mousePressed() {
      super.mousePressed();
      sampledRadiusMap.clear()
    }
  }

  private object Draw3D extends MyPApplet {
    lazy val cam = new PeasyCam(this, 500);

    override def setup() {
      size(400, 300, P3D);
      cam;
    }

    override def draw() {
      background(255);
      noStroke();
      fill(220);
      lights();
      zhang.Methods.drawAxes(g, 100);
      bez foreach { bez =>
        val bezNow = bez.sampled.map(v => Vec2(v.x, app.height - v.y));
        (0 to 40).map(x => map(x.toFloat, 0, 40, 0, TWO_PI)).sliding(2).foreach {
          theta =>
            beginShape(QUAD_STRIP);
            (bezNow zip sampledRadius(theta(0)) zip sampledRadius(theta(1))) foreach {
              case ((p, t0), t1) =>
                vertex(p.as3D(Vec2.fromPolar(t0, theta(0)).xy, Vec3.Z))
                vertex(p.as3D(Vec2.fromPolar(t1, theta(1)).xy, Vec3.Z))
            }
            endShape();
        }
      }
    }
  }
}