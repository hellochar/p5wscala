package daily

import processing.core._
import org.zhang.lib.{P5Util, MyPApplet}
import org.zhang.geom.{Vec2, Vec3}

class Mar26 extends MyPApplet with Savable {

  def pmouseVec = Vec2(pmouseX, pmouseY)

  import PApplet._;
  import PConstants._;

  type Transform = PMatrix3D

  trait Transformable[T <: Transformable[T]] {
    def transform(t: Transform):T
  }

  sealed trait Point extends Transformable[Point] {
    def v:Vec3
  }
  case class PointVal(v:Vec3) extends Point {
    def transform(t: Transform) = PointVal(P5Util.transformed(v, t))
  }
  class PointVar(var v:Vec3) extends Point {
    def transform(t: Transform) = new PointVar(P5Util.transformed(v, t))
  }
  case class PointProxy(p:Point) extends Point {
    def v = p.v
    def transform(t: Transform) = PointProxy(p.transform(t))
  }
  implicit def p2v3(p:Point) = p.v
//  implicit def v32p(v:Vec3) = Point(v)

  case class Face(points:Seq[Point]) extends Transformable[Face] {
    def transform(t: Transform) = Face(points map (_.transform(t)))

    def draw() {
      stroke(0, 255, 0);
      fill(128, 128);
      lines2(points map {_.v.as2D(Vec3.X, Vec3.Y)}, true)
    }
  }

  private var points:Set[PointVar] = Set()

  private var faces:Set[Face] = Set()

  private lazy val mkl = {val mkl = new zhang.MultiKeyListener(); addKeyListener(mkl); mkl}
  private var curUI = newUI()
  override def setup() {
    size(500, 500)
  }
  override def draw() {
    background(0)
    if(curUI.getClass != newUI().getClass) {
      curUI.end()
      curUI = newUI();
    } else {
      curUI.draw()
    }

    //draw things
    points foreach { p =>
      noStroke(); fill(255)
      ellipse(p.x, p.y, 15, 15)
    }

    faces foreach { f => f.draw() }

    pollSave() //check if the screen should be saved
  }

  def newUI() = if(mkl.isPressed(java.awt.event.KeyEvent.VK_SHIFT)) new FaceUI() else new PointUI()

  trait UI {
    def mouseMoved() {}

    def mousePressed() {}
    def mouseReleased() {}
    def mouseClicked() {}

    def mouseDragged() {}

    def draw() {}

    def end() {}
  }
  trait PointSelectUI extends UI {
    protected var selected:Option[PointVar] = None

    override def draw() {
      super.draw()
      selected = points.find(x => (x distTo mouseVec.withZ(0)) < 30)
      selected foreach {p =>
        noFill(); stroke(255, 0, 0)
        ellipse(p.x, p.y, 30 * 2, 30 * 2)
      }
    }
  }
  class PointUI() extends PointSelectUI {
    override def mouseClicked() {
      if(selected.isEmpty) points += new PointVar(mouseVec.xy)
    }

    override def mouseDragged() {
      selected foreach {p => p.v += (mouseVec - Vec2(pmouseX, pmouseY)).withZ(0)}
    }
  }
  class FaceUI() extends PointSelectUI {
    private var points = Seq[Point]()
    override def mouseClicked() {
      selected.foreach{ points :+= _ }
    }
    override def mouseDragged() {
      selected foreach {_.v += (mouseVec - Vec2(pmouseX, pmouseY)).withZ(0)}
    }

    override def draw() {
      super.draw()
      (new Face(points)).draw()
    }

    override def end() {
      faces += Face(points)
    }
  }

  override def mouseMoved() {
    curUI.mouseMoved()
  }
  override def mousePressed() {
    curUI.mousePressed()
  }
  override def mouseReleased() {
    curUI.mouseReleased()
  }
  override def mouseClicked() {
    curUI.mouseClicked()
  }
  override def mouseDragged() {
    curUI.mouseDragged()
  }

  override def keyPressed() {
    super.keyPressed(); //pressing space toggles a boolean variable to save the screen
  }
}