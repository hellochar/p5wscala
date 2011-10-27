package daily
package jun

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 6/12/11
 * Time: 4:41 PM
 * To change this template use File | Settings | File Templates.
 */

import geomerative.{RShape, RPoint, RG}
import processing.core._
import collection.Seq
import controlP5.ControlP5
import java.awt.event.{MouseWheelEvent, MouseWheelListener}
import javax.swing.SwingUtilities

class Jun12b extends PApplet {
  import PApplet._; import PConstants._;

  type OffspringAlgorithm = () => Candidate

  object ByPoint extends OffspringAlgorithm {
    val p5 = new ControlP5(Jun12b.this)
    val xS = p5.addSlider("x-bias", -500, 15);
    p5.setAutoDraw(false)

    def x = xS.getValue
    def bias = log(abs(x) + 1) * math.signum(x)

    def apply = new Candidate(curCand.list.map(jitter(_)).zipWithIndex.map(_ match {
      case (candidate, index) => {
        val GOALpoint = goal.list(index);
        val oldpoint = curCand.list(index);
        if (candidate.dist(GOALpoint) + bias < oldpoint.dist(GOALpoint)) candidate else GOALpoint
      }
    }))

    def keyPressed() {
      val moveAmt = (xS.getMax - xS.getMin) / 10
      if (key == 'z') {
        xS.setValue(xS.getValue - moveAmt)
      } else if (key == 'x') {
        xS.setValue(xS.getValue + moveAmt)
      }
    }
  }

  trait PList {
    def list:Seq[RPoint]
    def distanceSeq:Seq[Float] = list.zip(goal.list).map(_ match { case (mine, goal) => mine.dist(goal) } )
    def draw() {
      beginShape()
      list foreach(p => vertex(p.x, p.y))
      endShape()
    }
  }
  class Candidate(val list:Seq[RPoint]) extends PList { def this(parent:PList) = this(parent.list map(new RPoint(_))) }
  object goal extends PList { var list:Seq[RPoint] = _; def update() {goal.list = (0 to SEGLENGTH) map(i => name.getPoint(map(i, 0, SEGLENGTH, 0, 1)))} }


  val SEGLENGTH:Int = 1000
  def jitter(p:RPoint, jitterRadius:Float = 10) = {
      val ang = random(TWO_PI); val rad = random(jitterRadius);
      new RPoint(p.x+rad*cos(ang), p.y+rad*sin(ang))
  }

  var name:RShape = _;
  var curCand:Candidate = _
  var algorithm:OffspringAlgorithm = _
  import actors._; import Actor._;
  val mailbox:collection.mutable.Queue[() => Any] = collection.mutable.Queue()

  override def setup() {
    size(1280, 300)
    RG.init(this)
    name = RG.getText("Xiaohan Zhang", findFont("RAGE.TTF"), 120, CENTER)
    name.translate(width/2, height/2)
    goal.update()
    curCand = randomCandidate();

    algorithm = ByPoint
    addMouseWheelListener(new MouseWheelListener() {
      def mouseWheelMoved(e: MouseWheelEvent) {
//        mailbox += (() => {
        SwingUtilities.invokeLater(new Runnable() { override def run() {
          val scale = pow(1.06f, e.getWheelRotation)
          name.transform(name.getX, name.getY, name.getWidth * scale, name.getHeight*scale)
//        })
        }});
      }
    })
  }

  def randomCandidate() = new Candidate((0 to SEGLENGTH) map(i => new RPoint(random(width), random(height))))

  override def mousePressed() {
    if(mouseButton == RIGHT)
      background(255)
    if(mouseButton == LEFT)
      curCand = randomCandidate()
  }
  var saving:Boolean = false
  override def keyPressed() {
    ByPoint.keyPressed
    if(key == ' ') saving = !saving
  }

  override def draw() {
//    fill(255, 4); rect(0, 0, width, height);
    mailbox.foreach(_.apply()); mailbox.clear()
    name.transform(mouseX-name.getWidth/2, mouseY-name.getHeight/2, name.getWidth, name.getHeight);
    goal.update()
//    background(255)
    noFill; stroke(0, 40)
//    goal.drawVF
    curCand.draw

    curCand = algorithm()
    if(saving) saveFrame("Jun12b-####.png")
//    else {
//      ByPoint.p5.drawVF()
//    }
  }

}
