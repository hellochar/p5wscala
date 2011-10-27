package daily
package oct

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 10/23/11
 * Time: 4:09 PM
 */
import processing.core._
import org.zhang.lib._;
import org.zhang.lib.MyPApplet;
import org.zhang.lib.misc.Vec3
import peasy.PeasyCam

class Oct23 extends MyPApplet with Savable {
  import PApplet._; import PConstants._;

  @inline
  val REPELPOWER = 1000

  @inline
  val SPHERESIZE = 100

  var things = (0 until 100).map(_ => new Thing(Vec3.fromSpherical(SPHERESIZE, random(TWO_PI), random(-PI/2, PI/2)), Vec3())).toSet
  val dt = .1f;

  case class Thing(var loc:Vec3, var vel:Vec3) {
    override def hashCode() = java.lang.System.identityHashCode(this)

    private var force = Vec3()
    def closest = {
//      val list = (things - this).toList.sortBy(k => (k.loc - loc).mag2);
//      val listDist = list.map(x => (x.loc - loc).mag2) //98, 100, 102, 102.5, 140, 141, 144
//      val dif = partialDif(listDist.toStream).toList //2, 2, .5, 40, 1, 3
//      val pts = partials(dif.toStream).map(average(_)) //[[2], [2, 2], [2, 2, .5], [2, 2, .5, 40], [2, 2, .5, 40, 1]
////      val res7 = pts.map(average(_));
////      val dif2 = partialDif(dif.toStream).toList
//      list.slice(0, listDist.indexWhere(x => abs(x) > SPHERESIZE*10) + 1)
      (things - this).filter(k => (k.loc - loc).mag < 50)
    }

    def act() {
      force = ((things - this).map(k => ((o:Vec3) => o.scale(REPELPOWER / o.mag2))(loc - k.loc)) foldLeft Vec3()){ _ + _}
    }

    def update() {
      vel += force * dt;
      loc = (loc + vel * dt) ofMag SPHERESIZE
      force = Vec3();
    }

    def draw() {
      noStroke();
      translate(loc)
      sphere(1);
      translate(-loc)
      stroke(0);
      closest.map(x => line(loc, x.loc))
    }
  }

  lazy val cam = new PeasyCam(this, SPHERESIZE * 3f)
  override def setup() {
    size(500, 500, P3D)
    cam;
  }

  override def draw() {
//    println("frame "+frameCount+"!")
    background(64);
    zhang.Methods.drawAxes(g, 25)
    things foreach (_.act())
    things foreach (_.update())

    noStroke; fill(255);
    things foreach (_.draw())
    fill(0);
//    sphere(SPHERESIZE - 2);
//    things foreach (println _)
    pollSave("Oct23-")
  }
  
  override def keyPressed() {
    super.keyPressed();
  }
}