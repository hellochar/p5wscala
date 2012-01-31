package daily
package jan

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 1/26/12
 * Time: 3:24 PM
 */

import processing.core._
import org.zhang.lib.MyPApplet

class Jan26 extends MyPApplet with Savable {

  import PApplet._;
  import PConstants._;

  def easeInOut(func:Float => Float) = (x:Float) => (if((x * 2) < 1) func(x*2) else (2 - func(2 - 2*x))) / 2
  def reverse(func:Float => Float) = (x:Float) => func(1 - x)
  def linear(scl:Float) = (_:Float) * scl
  def exp(exp:Float) = pow((_:Float), exp)
  val sine = (x:Float) => (1 + PApplet.sin((x - 1/2f) * PI)) / 2

  override def setup() {
    size(500, 500)
  }

  def piecewise(tuples:(Float => Float, Float)*) = {
    (x:Float) => {
      val (less, more) = tuples.partition(_._2 < x) //_1 is a list of tuples that are less than the one we want, _2 is a list of tuples that contains what we want
      val lowFloat = less.lastOption.map(_._2).getOrElse(0f)
      val highFloat = more.head._2
      more.head._1(map(x, lowFloat, highFloat, 0, 1))
    }
  }

  def render(func:Float => Float) = lines2((0 until width) map {x => (x.toFloat, map(func(map(x, 0, width, 0, 1)), 0, 1, height, 0))})

  override def draw() {
    background(0); stroke(255); noFill(); smooth();
    val pow = map(mouseX, 0, width, 0, 10)
    println(pow)
    val func = {
      val piece = easeInOut(exp(pow)) andThen (map(_, 0, 1, .2f, .8f))
      piecewise((piece, .5f), (reverse(piece), 1f))
    }
    render(func)
    def drawText(x:Float) {
      vert((x * width).toInt)
      val value = func(x)
      fill(255)
      text(value, x * width, map(value, 0, 1, height, 0))
    }
    drawText(.25f);
    drawText(.5f);
    drawText(.75f);
    drawText(mouseX.toFloat / width)


    pollSave() //check if the screen should be saved
  }

  override def keyPressed() {
    super.keyPressed(); //pressing space toggles a boolean variable to save the screen
  }
}