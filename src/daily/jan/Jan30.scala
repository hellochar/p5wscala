package daily
package jan

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 1/30/12
 * Time: 2:44 AM
 */

import processing.core._
import org.zhang.lib.MyPApplet
import org.zhang.geom.Vec2

class Jan30 extends MyPApplet with Savable {

  import PApplet._;
  import PConstants._;

  class Mutable[T](var elem:T) {
    def get = elem
    def set(t:T) {
      elem = t
    }
  }
  implicit def mutable2Elem[T](m: Mutable[T]) = m.get
  implicit def mutSeq2elemSeq[T](m: Seq[Mutable[T]]) = m.map(_.get)

  def lerpFunc = if(keyPressed) ((x:Float) => {(sin(x*TWO_PI)+1)/2}) else (identity[Float] _)

  def lerp2(a:Vec2, b:Vec2, t:Float) = a + (b - a) * lerpFunc(t);
  def lerp(a:Vec2, b:Vec2, t:Float) = a + (b - a) * t;
  def bezier(pts:Seq[Vec2])(t:Float):Option[Vec2] = pts match {
    case Seq(a, b) => Some(lerp2(a, b, t))
    case s if s.length < 2 => None;
    case s => bezier(s.sliding(2).map{x => lerp(x(0), x(1), t)}.toSeq)(t)
  }

  override def setup() {
    size(500, 500)
  }

  def curBez = bezier(pts) _

  def drawBezier() {
    lines2(Range.Double(0, 1, .01) flatMap {x => curBez(x.toFloat)})
  }

  var pts = Seq[Mutable[Vec2]]()
  var selected:Option[Mutable[Vec2]] = None

  override def draw() {
    background(0); smooth();
    noFill(); stroke(255, 128); strokeWeight(1);
    drawBezier()

    fill(255); noStroke();
    curBez((millis() / 1800f) % 1) foreach { ellipse(_, 10, 10) }

    noFill(); stroke(255); strokeWeight(3);
    lines2(pts);

    noStroke(); fill(255, 220, 0);
    pts foreach { ellipse(_, 10, 10)}

    selected foreach { noFill; stroke(255); strokeWeight(1); ellipse(_, 20, 20) }

    pollSave() //check if the screen should be saved
  }

  override def mousePressed() {
    selected = pts.find(x => (x.get distTo mouseVec) < 5)
    if(!selected.isDefined) {
      selected = Some(new Mutable(mouseVec))
      pts :+= selected.get;
    }
  }

  override def mouseDragged() {
    selected foreach { _.set(mouseVec)}
  }

  override def keyPressed() {
    super.keyPressed(); //pressing space toggles a boolean variable to save the screen
  }
}