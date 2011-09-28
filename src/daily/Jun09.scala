package daily

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 6/9/11
 * Time: 5:01 PM
 * To change this template use File | Settings | File Templates.
 */

import javax.swing.JOptionPane
import processing.core._

class Jun09 extends PApplet {
  import PApplet._; import PConstants._;
  var img:PImage = _

  override def setup() {
    size(500, 250)
    img = createImage(width/2, height/2, RGB)
    render()
  }

  var showW = false;
  override def draw() {
    if(showW) {
      val s = JOptionPane.showInputDialog(frame, "String? ");
      decision = parse(s)
      render()
      showW = false
    }
    image(img, 0, 0, width, height)
  }

  override def keyPressed() {
    if(key == ' ') {
      showW = true
    }
  }

  override def mousePressed() {
    render()
  }

  /**
  * Black pixel = true, white pixel = false.
*/
  def render(decision:Decision = decision, seed: Seq[Boolean] = (0 until img.width) map(i => if(random(1) < .5) false else true)) {
//    println(seed)
    implicit def i2b(i:Int) = if(i == color(255)) false else true
    implicit def b2i(b:Boolean) = if(b) color(0) else color(255)
    img.loadPixels
    (0 until seed.length) foreach(i => img.pixels(i) = seed(i))
//    arrayCopy(img.pixels, 0, seed map(b => b2i(b)) toArray, 0, seed.length)
    def renderLine(y:Int) {
      def |(xx:Int):Int = if(xx >= 0 && xx < img.width) img.pixels((y-1)*img.width+xx) else color(255)
      (0 until img.width) foreach(x => {
        img.pixels(y*img.width+x) = decision.apply(o => i2b(|(x+o)))
      })
    }
//
    for(i <- 1 until img.height) {
      renderLine(i)
    }
    img.updatePixels
    return img;
  }

  var decision = new Decision {
    def apply($:Int => Boolean):Boolean = {
//    $(-1) && $(1) || $(-2) && $(2) || ($(-1) ^ $(0) && $(0) ^ $(1))
    $(-1) && $(2) ||
    $(-2) && $(1) ||
    $(-1) ^  $(1) ||
    $(-2) && $(2)
    }
  }

  trait Decision extends (((Int) => Boolean) => Boolean)
  type Dec = Decision

  case class AndDecision(a:Decision, b:Decision) extends Decision {
    def apply(func:Int => Boolean):Boolean = a(func) && b(func)
  }
  case class OrDecision(a:Dec, b:Dec) extends Decision {
    def apply(func: (Int) => Boolean) = a(func) || b(func)
  }
  case class XorDecision(a:Decision, b:Decision) extends Decision {
    def apply(func: (Int) => Boolean) = a(func) ^ b(func)
  }
  case class NotDecision(a:Decision) extends Decision {
    def apply(func: (Int) => Boolean) = !a(func)
  }
  case class NumDecision(offset:Int) extends Decision {
    def apply(func: (Int) => Boolean) = func(offset)
  }

  /*for any string S, you can have:
    !S
    For any two strings S and T, you can have:
    S&T
    S|T
    S^T
    order of operations - first one is the lowest down in the tree:
    &
    ^
    |

    any number is a string; a number is of the form
    [-]0,1,2...etc.
   */
  def parse(ss:String):Decision = try {
    val s = ss.trim
    def isNumber(s:String) = try { Integer.parseInt(s); true; } catch { case _ => false }
    def numbered(s:String) = Integer.parseInt(s)
    if(isNumber(s)) return NumDecision(numbered(s))
    if(s.startsWith("!")) return NotDecision(parse(s.drop(1)))
    def m2args(c:Char, func:(Dec, Dec) => Dec) = s.indexOf(c) match {
      case i:Int if i >= 0 => func(parse(s.substring(0, i)), parse(s.substring(i+1)))
      case _ => null
    }
    lazy val or = m2args('|', OrDecision(_, _));
    lazy val xor = m2args('^', XorDecision(_, _));
    lazy val and = m2args('&', AndDecision(_, _));
    if(or != null) return or;
    if(xor != null) return xor;
    if(and != null) return and;
    else throw new Exception("Couldn't parse "+s+"!");
    } catch {
      case e: Exception => { e.printStackTrace(); null; }
    }

}