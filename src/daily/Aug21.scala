package daily

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 8/21/11
 * Time: 2:26 PM
 */
import processing.core._
import org.zhang.lib.MyPApplet
import ddf.minim._
import analysis.FFT

class Aug21 extends MyPApplet with Savable {
  import PApplet._; import PConstants._;

  lazy val min = new Minim(this)
  lazy val fft = new FFT(1024, 44100)
  lazy val buf = Array.ofDim[Float](1024)

  override def setup() {
    size(1024, 200)
    noLoop()
    fft.setFreq(440, fft.specSize - 1);

  }

  override def mousePressed() {
    redraw();
  }

  override def draw() {
//    val k = org.zhang.lib.time {
//      fft.inverse(buf);
//    }
//    println(k._2)
    java.util.Arrays.fill(buf, 0)
    fft.inverse(buf);
    noFill(); stroke(255); background(0);
    beginShape(); buf.zipWithIndex.foreach{ case (f, i) => vertex(i, map(f, -1, 1, height/2 + 1, height/2 - 1)) }; endShape()

    dispFFT()

    println("min: " + buf.min + " and max: "+buf.max)
//    pollSave("Aug21-")
  }

  def dispFFT() {
  }
  
  override def keyPressed() {
    super.keyPressed();
  }
}