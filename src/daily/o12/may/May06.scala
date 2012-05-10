package daily

import processing.core._
import org.zhang.lib.MyPApplet
import peasy.PeasyCam
import org.zhang.geom.Vec3

class May06 extends MyPApplet with Savable {

  import PApplet._;
  import PConstants._;
  import toxi.sim.grayscott._;

  val SIZE = 100
  lazy val gs = new GrayScott(SIZE, SIZE, true)//new toxi.sim.grayscott.GrayScott(width, height, true)
  lazy val heights = Array.fill(SIZE*SIZE)(0f)
  lazy val cam = new PeasyCam(this, 100);
  override def setup() {
    size(500, 500, P3D)
//    gs.setCoefficients(.0214f, .074f, .19f, .13f)
    gs.setCoefficients(.024f, .074f, .04f, .0421f)
//    gs.setRect(0, 0, SIZE/2, SIZE/2);
    for(x <- 0 until 1) { val (x, y) = (random(SIZE-20).toInt, random(SIZE-20).toInt); gs.setRect(x, y, x+20, y+20) }
    heights
    cam
  }

  override def draw() {
    background(64)
    zhang.Methods.drawAxes(g, 100)
    for(x <- 0 until 10) gs.update(1)
    for(x <- 0 until gs.v.length) {
      heights(x) += gs.v(x)
    }

    noStroke(); fill(255);
    ambient(32);
    directionalLight(128, 128, 128, Vec3(1, 0, -1).normalize)

    beginShape(QUADS)
    for(x <- 0 until SIZE-1; y <- 0 until SIZE-1) {
      def vtx(x:Int, y:Int) {
        vertex(x-SIZE/2, y-SIZE/2, heights(y*SIZE+x))
      }
      //point(x-width/2, y-height/2, gs.v(y*width+x) * 10)
      vtx(x, y)
      vtx(x+1, y)
      vtx(x+1, y+1)
      vtx(x, y+1)
    }
    endShape()

    pollSave() //check if the screen should be saved
  }

  override def keyPressed() {
    super.keyPressed(); //pressing space toggles a boolean variable to save the screen
  }
}