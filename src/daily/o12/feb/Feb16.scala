package daily

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 2/16/12
 * Time: 3:42 PM
 */

import processing.core._
import org.zhang.geom.Vec2
import org.zhang.lib.{P5Util, MyPApplet}

class Feb16 extends MyPApplet with Savable {

  import PApplet._;
  import PConstants._;

  case class Circle(pos:Vec2, rad:Float, c:Int) {
    def draw() {
      fill(c);
      noStroke();
      ellipse(pos, rad*2, rad*2)
    }
  }

  /**
   * returns a normalized number from 0 - 1
   * @param seq
   */
  def area(seq:Seq[Circle]):Float = seq.map(x => sq(x.rad)*PI).sum / (width * height)

  lazy val circles = Iterator.iterate(Seq(Circle(P5Util.randomVector(this), 25, color(0, 255, 255)))){seq =>
    println("on "+seq.length+" with area "+area(seq))
    seq :+
    (Iterator.continually{
      val loc = P5Util.randomVector(this)
      val min = seq.minBy(c => (c.pos distTo loc) - c.rad)
      (loc, (min.pos distTo loc) - min.rad)
    }.find(_._2 > 0) match {
      case Some((pos, rad)) => Circle(pos, seq.head.rad min rad, color((seq.length / 10f) % 255, 255, 255))
    })
  }.dropWhile(s => s.length < 5000 && area(s) < .85f).next()

  override def setup() {
    size(500, 500)
    colorMode(HSB)
    smooth()
  }

  override def draw() {
    background(0)
    circles foreach (_.draw())
    pollSave() //check if the screen should be saved
  }

  override def keyPressed() {
    super.keyPressed(); //pressing space toggles a boolean variable to save the screen
  }
}