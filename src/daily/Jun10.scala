package daily

import javax.swing.JOptionPane
import processing.core._
import controlP5._

class Jun10 extends PApplet {
  import PConstants._;

  object p5 {
    var cp5: ControlP5 = _
    var textfield: Textfield = _

    def init() {
      cp5 = new ControlP5(Jun10.this)
      textfield = cp5.addTextfield("rule", 0, 0, sideLen - 20, 20)
      textfield.setText("-1 ^ 1")
      textfield.setAutoClear(false)
    }

  }
  var img:PImage = _
  var decision:Decision = _
  val cp5Height = 200
  val sideLen = 400

  override def setup() {
    size(sideLen, sideLen+cp5Height)
    p5.init
    p5.textfield.addListener(new ControlListener() {
      def controlEvent(e: ControlEvent) {
        update()
      }
    })
    img = createImage(100, 100, RGB)
    update()
  }

  def update() {
    try {
      decision = parse(p5.textfield.getText)
      render()
    } catch {
      case p: ParseException => {
        fill(255, 255, 0);
        text(p.getMessage, width/2, 40)
      }
    }
    image(img, 0, cp5Height, sideLen, sideLen);
  }

  override def draw() {
    image(img, 0, cp5Height, sideLen, sideLen);
  }

  override def mousePressed() {
    render();
  }

  /**
  * Black pixel = true, white pixel = false.
*/
  def render(seed: Seq[Boolean] = (0 until img.width) map(i => if(random(1) < .5) false else true)) {
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

  case class ParseException(s: String) extends Exception(s)
  /*for any string S, you can have:
    !S, which represents the negation
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
  def parse(ss:String):Decision = {
    val s = ss.trim
    def isNumber(s:String) = try { Integer.parseInt(s); true; } catch { case _ => false }
    def numbered(s:String) = Integer.parseInt(s)
    if(isNumber(s)) return NumDecision(numbered(s))
    if(s.startsWith("!")) return NotDecision(parse(s.drop(1)))
    def m2args(c:Char, func:(Dec, Dec) => Dec) = s.indexOf(c) match {
      case i:Int if i >= 0 => func(parse(s.substring(0, i)), parse(s.substring(i+1)))
      case _ => null
    }
    lazy val or = m2args('|', OrDecision);
    lazy val xor = m2args('^', XorDecision);
    lazy val and = m2args('&', AndDecision);
    if(or != null) return or;
    if(xor != null) return xor;
    if(and != null) return and;
    else throw ParseException("Couldn't parse "+s+"!");
  }
}