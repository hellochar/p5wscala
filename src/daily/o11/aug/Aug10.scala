package daily


/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 8/10/11
 * Time: 1:54 PM
 */
import processing.core._
import org.zhang.lib.MyPApplet
import peasy.PeasyCam
import org.zhang.geom.Vec3

class Aug10 extends MyPApplet with Savable {
   import PConstants._;

  override def setup() {
    size(500, 500, P3D)
    new PeasyCam(this, 500)
    frameRate(2)
  }

  def rndVec(ext:Float) = {def r = random(-ext, ext); Vec3(r, r, r)}
  val locs = (0 until 1200).map(i => rndVec(10000)) :+ Vec3()//Vec3.fromSpherical(random(500), random(TWO_PI), random(-PI/2, PI/2)))
  val connects = Map(locs.map(loc => loc -> {
    locs.filter(loc !=).filter(loc.distTo(_) < 2500)
  }):_*)

  override def draw() {
    background(255)
    pollSave("aug10-")
    lights()
    locs.foreach(loc => {
      pushMatrix()
      translate(loc)
      noStroke(); fill(128);
      sphere(12)
      popMatrix()
      stroke(0, 80); strokeWeight(2)
      connects(loc).foreach(line(loc, _))
    })
  }

  override def keyPressed() {
    super.keyPressed();
  }
}