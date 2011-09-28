package daily

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 7/20/11
 * Time: 8:53 PM
 */
import processing.core._
import org.zhang.lib.misc.Vec2
import zhang.Camera

class Jul20 extends NameApplet {
  import PApplet._; import PConstants._; 

  var img:PImage = _
  def render() {
    background(255)
    smooth();
    strokeWeight(1);
    noFill();
    translate(width / 2, height / 2)
    drawMountain()
    img = get()
  }

  override def setup() {
    size(1024, 200)
    myFont = createFont("ariblk", 220, true, "p5wscala".toCharArray)
//    img = beginRecord(JAVA2D, "TESTOUTPUT");
    render()

  }

  def center = Vec2(width/2, height/2)

  def findGoodOffset(range:Float, epsilon:Float = 1e-5f, start:Float = 0) = {
    val sampleInterval = .001f; //maybe calculate this with respect to epsilon
    var offset = start;
    while(abs(noise(offset)-noise(offset+range)) > epsilon) offset += sampleInterval
    offset
  }

  val drawRing = {
    val vtx = Function.tupled((vertex _): (Float, Float) => Unit)
    val offset = findGoodOffset(TWO_PI)
    val iters = 1000
    lazy val radius = width*4
    lazy val points = (0 until iters).map(map(_, 0, iters, 0, TWO_PI)).map(t => Vec2.fromPolar(map(noise(t + offset), 0, 1, 0, radius), t))

    () => {
      beginShape()
      points.foreach(vtx)
      endShape()
    }
  }

  def drawMountain(iters: Int = 500, scl:Float = .98f) {
    pushMatrix()
    (0 until iters).foreach(i => { drawRing(); scale(scl); })
    popMatrix()
  }
  var masked:PImage = _
  var myFont:PFont = _
  override def draw() {
    background(255)
    val mask = createGraphics(width, height, JAVA2D)
    mask.beginDraw()
    mask.fill(255);
    mask.background(0);

//    mask.ellipse(mouseX, mouseY, 50, 50)
    mask.textFont(myFont)
    mask.textAlign(CENTER, CENTER)
    mask.text("p5wscala", mouseX, mouseY)
    mask.endDraw();
    masked = img.get
    masked.mask(mask)
    masked.loadPixels()
    masked.pixels = for(i <- masked.pixels) yield (if(alpha(i) > 0) color(0, 255 - brightness(i)) else i)
    masked.updatePixels()
    image(masked, 0, 0)
    println(frameRate)
//    pollSave("p5wscala-")
  }

  override def mousePressed() {
    masked.save("p5wscala.png")
//    println(mouseX, mouseY)
  }
}