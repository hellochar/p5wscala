package daily
package sep

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 9/15/11
 * Time: 10:41 PM
 */
import processing.core._
import org.zhang.lib.MyPApplet
import org.zhang.lib.misc.{LorenzAttractor, Vec3}

class Sep15 extends MyPApplet with Savable {
  import PApplet._; import PConstants._; 

  val path = LorenzAttractor().makePath(Vec3(1));
  lazy val sliding = path.sliding(width, 10)

  override def setup() {
    size(500, 500)
    new zhang.Camera(this);
  }

  override def draw() {
    background(0); stroke(255); noFill; rect(0, 0, width, height);
    def draw(c:Int, y:Float, list:Seq[Float]) = { stroke(c); noFill();
      pushMatrix();
      translate(0, y); beginShape(); for(x <- 0 until list.length) vertex(x, 10*list(x)+height/6); endShape();
      line(0, height/6, width, height/6);
      popMatrix();
    }
    
    //I want to have a list of parameters to give to draw alongside the list;
    val cs = List(color(255, 0, 0), color(0, 255, 0), color(0, 0, 255))
    val ys = List(0, height*1/3f, height * 2/3f)
    sliding.next.unzip3 match {
      case (a,b,c) => {
        draw(cs(0), ys(0), a)
        draw(cs(1), ys(1), b)
        draw(cs(2), ys(2), c)
      }
    }
    pollSave("Sep15-")
    println(frameRate);
  }
  
  override def keyPressed() {
    super.keyPressed();
  }
}