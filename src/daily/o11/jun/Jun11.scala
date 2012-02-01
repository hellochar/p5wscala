package daily


/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 6/11/11
 * Time: 8:01 PM
 * To change this template use File | Settings | File Templates.
 */

import geomerative._
import processing.core._

class Jun11 extends PApplet {
  import PApplet._
  import PConstants._

  var dots:Seq[Dot] = _

  var text:RShape = _;
  val DOTS_NUM = 10000
  val FREQUENCY = .03f;
  val AMPLITUDE = 160;
  override def setup() {
    size(1024, 300)
    graphics = createGraphics(width, height, P2D);
    graphics.beginDraw()
    RG.init(this)
    text = RG.getText("Xiaohan Zhang", findFont("RAGE.TTF"), 140, CENTER)
    text.translate(width/2, height*.6f)

    dots = (0 until DOTS_NUM) map(i => map(i, 0, DOTS_NUM, 0, 1)) map( f => {Dot(text.getPoint(f), text.getTangent(f),
      mkSin(AMPLITUDE, FREQUENCY), f)})
    graphics.smooth
    graphics.background(0)
//    stroke(255,0,0)
    graphics.noStroke
    graphics.colorMode(HSB)
    graphics.endDraw
  }
  var graphics:PGraphics = _;

  def mkSin(amp:Float, freq:Float) = () => amp*sin(PI*.85f+freq*millis()/1000f)

  var saving = false
  override def keyPressed() {
    if(key == ' ') saving = !saving
  }

  override def draw() {
    val gg = g;
    g = graphics;
    g.beginDraw

    background(255, 15)
//    dots foreach(d => d.distFromCenter = map(mouseX, 0, width, 0, 100))

    dots foreach(d => d.step)
    dots foreach(d => d.draw)
    g.endDraw
    g = gg;
    image(graphics, 0, 0)
    if(saving) saveFrame("Jun11-####.png");
//    println(value)
//    saveFrame()
  }

  val dotSize = 10
  case class Dot(center:RPoint, tangent:RPoint, var distFromCenter:() => Float, hue:Float) {
    tangent.rotate(-PI/2)
    var loc:RPoint = center
    def getLoc(dist:Float) = {
      val move = new RPoint(tangent); move.normalize(); move.scale(dist)
      val l = new RPoint(center); l.translate(move)
      l
    }
    def step() {
      loc = getLoc(distFromCenter())
    }
    def draw() {
      fill(hue*255, 255, 255)
      ellipse(loc.x, loc.y, dotSize, dotSize)
    }
  }
}