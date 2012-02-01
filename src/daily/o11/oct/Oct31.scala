package daily


/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 10/31/11
 * Time: 10:10 PM
 */
import processing.core._
import org.zhang.lib.MyPApplet
import peasy.PeasyCam
import org.zhang.geom._
import toxi.geom.Vec3D

//todo: fix a NaN error that keeps popping up
class Oct31 extends MyPApplet with Savable with SphereUtils with SphereSurfaceUI {
  import PApplet._; import PConstants._;
  implicit def zhang2toxi(z:Vec3) = new Vec3D(z.x, z.y, z.z)
  implicit def toxi2zhang(t:Vec3D) = Vec3(t.x, t.y, t.z)
  implicit def zhang2pv(z:Vec3) = new PVector(z.x, z.y, z.z)
  implicit def pv2zhang(p:PVector) = Vec3(p.x, p.y, p.z)

  def dt = .1f

  lazy val cam = new PeasyCam(this, 100)
  val things = (0 until 100) map (_ => new Thing(randomColor))
  override def setup() {
    size(500, 500, P3D)
    cam;
  }

  def randomVector = Vec3.random * 100
  var nanned = false

  private var idx = 0;
  class Thing(var col:Int, var loc:Vec3 = randomVector, var vel:Vec3 = Vec3()) {
    private var tail:Seq[Vec3] = Seq(loc)
    private var force:Vec3 = Vec3()
    val id = idx; idx += 1;
    def tailSize = 50

    def run() {

    }

    def update() {
//      vel +=
    }

    def chase(want:Vec3) {
      val dist = distS(loc, want) * .1f;
      loc = loc.rotate((loc cross want), dist / loc.mag);
      tail = (loc +: tail) take 50
    }

    override def toString = id+", loc: "+loc

    def repel() {
      val others = things filter (_.loc != loc)
      val locs = others map (_.loc)
      val cross = locs map (loc cross)
      val newLocs = for(x <- cross) yield {
        val v = loc.rotate(x, 1 / loc.mag)
        if(v.x.isNaN && !nanned) {
          System.out.println("NaNNed!")
          println(this)
          println("x: "+x+", x.normalize: "+x.normalize+", loc: "+loc)
          loc.rotate(x, 1 / loc.mag);
        };
        v
      }
      val sum = (newLocs fold Vec3())(_ + _)
      loc = sum ofMag loc.mag
      if(!nanned && loc.x.isNaN) {
        System.out.println("First NaN!")
        println("\tlocs: "+locs)
        println("\tcross: "+cross)
        println("\tnewLocs: "+newLocs)

        nanned = true;
      }
    }

    def draw() {
      noFill();

      beginShape();
      tail.zipWithIndex foreach {case (loc, idx) => {stroke(col, map(idx, 0, tailSize, 255, 0)); vertex(loc); }}
      endShape();

      noStroke(); fill(col);
      sphere(loc, 5);
    }
  }

  override def draw() {
    background(64)
//    Methods.drawAxes(g, 200)
    noStroke(); fill(255); lights();
    sphereDetail(30)
    sphere(100)
    sphereDetail(10)
    things foreach (_.draw())
//    println("Frame "+frameCount+"\n"+things.mkString("\n")+"\n")
//    things foreach (_.repel())

    getIntersect(mouseX, mouseY, 100) match {
      case Some(intersect) => {
        smallCircle(intersect ofMag 101, 6)
        things foreach (_.chase(intersect))
      }
      case None => ()
    };

    pollSave("")
  }
  
  override def keyPressed() {
    super.keyPressed();
    if(key == 'r') {
      things foreach (_.loc = randomVector)
    }
  }
}