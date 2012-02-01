package daily

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 6/12/11
 * Time: 2:51 AM
 * To change this template use File | Settings | File Templates.
 */

import controlP5.{ControlEvent, ControlListener, ControlP5}
import java.awt.event.{MouseWheelEvent, MouseWheelListener}
import processing.core._
import zhang.Camera

class Jun12 extends PApplet {
  import PApplet._; import PConstants._;
  trait ControlType {
    def m:Float
    def n:Float
    def n1:Float
    def n2:Float
    def n3:Float

    def draw()
  }

  object P5ControlType extends ControlType {
    def draw() {
      background(0)
      cam.apply
      zhang.Methods.printHierarchy(frame)
//      zhang.Methods.strokeWeightScreen(Jun12, 2, cam);
      redrawShape();
      resetMatrix
    }
    val p5: ControlP5 = new ControlP5(Jun12.this);
    val cam:Camera = new Camera(Jun12.this); cam.noAutoApply(); cam.za(1.06f);
    private val SH = 15;
    private val SW = width / 2;
    private val inset = 5;
    private val DH = SH + inset
    val mS = p5.addSlider("m", 0, 30, 24, 0, 0, SW, SH);
    val nS = p5.addSlider("n", 0, 30, 24, 0, DH, SW, SH);
    val n1S = p5.addSlider("n1", 2, 10, 6, 0, 2 * DH, SW, SH);
    val n2S = p5.addSlider("n2", 0, 10, 6, 0, 3 * DH, SW, SH);
    val n3S = p5.addSlider("n3", 0, 10, 5.56f, 0, 4 * DH, SW, SH)
    val redrawListener = new ControlListener() {
      def controlEvent(theEvent: ControlEvent) {
        redrawShape();
      }
    }
//    List(mS, nS, n1S, n2S, n3S) foreach (_.addListener(redrawListener))

    def n3 = n3S.getValue
    def n2 = n2S.getValue
    def n1 = n1S.getValue
    def m = mS.getValue
    def n = nS.getValue
  }
  object MouseControlType extends ControlType {
    var m:Float = 15;
    var n:Float = 15;
    Jun12.this.addMouseWheelListener(new MouseWheelListener() {
      def mouseWheelMoved(e: MouseWheelEvent) {
        m = m+e.getWheelRotation
      }
    })
    def draw() = redrawShape()

    def n3 = map(mouseX, 0, width, 0, 1)
    def n2 = map(mouseY, 0, height, 0, 1)

    def n1 = .8f

  }

  var ctype:ControlType = _;

  override def setup() {
    size(900, 600)
    background(0)

    ctype = P5ControlType
    redrawShape
  }

  override def draw() {
    ctype.draw
  }

  def redrawShape() {
    pushMatrix
    stroke(255); smooth();
    translate(width/2, height/2);
    noFill()
    render();
    popMatrix()
  }

  def render(radiusMin:Float = 35, iterations:Int = (150*(ctype.m+ctype.n)).asInstanceOf[Int]) {
    beginShape
    //radiusMin = mult * modformula
    //modformula = 2^(-1/n1)
    //mult = ?
    //radiusMin = mult * 2^(-1/n1) => mult = radiusMin / (2^(-1/n1))
    val mult = radiusMin / pow(2, -1/ctype.n1)
    for(i <- 0 until iterations) {
      val angle = map(i, 0, iterations, 0, TWO_PI)
      val rad = mult * modformula(angle, ctype.m, ctype.n, ctype.n1, ctype.n2, ctype.n3)
      vertex(rad*cos(angle), rad*sin(angle))
    }
    endShape(CLOSE)
  }

  //For the given parameters, returns the radiusMin according to the modformula.
  //A lower bound on the radiusMin is, taking cos(m*theta) = sin(m*theta) = 1,
  //r_min = 2^(-1/n1)
  def modformula(theta:Float, m:Float, n:Float, n1:Float, n2:Float, n3:Float) =
    pow(pow(abs(cos(m*theta)), n2) + pow(abs(sin(n*theta)), n3), -1/n1)

}