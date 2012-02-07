package daily


/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 2/1/12
 * Time: 1:50 PM
 */

import processing.core._
import org.zhang.lib.MyPApplet
import org.zhang.geom.Vec3

object Feb01Main extends App {
  PApplet.main(Array("daily.Feb01"))
}

class Feb01 extends MyPApplet with Savable with SphereUtils {

  import PApplet._;
  import PConstants._;

  case class Ray(start:Vec3, norm:Vec3) {
    if(abs(1 - norm.mag) > 1e-4f) sys.error("Norm isn't normalized!");
    /**
     * Converts this ray into a line and calculates the point on that line that is closest to the given pt.
     * If that point doesn't actually exist on this ray, this method returns a Left() containing the point.
     * Otherwise, this will return a Right() containing the point.
     * @param pt
     */
    def closestPoint(pt:Vec3):Either[Vec3, Vec3] = {
      //we first offset the pt such that it's as if the ray is starting at the origin.
      val ptOff = pt - start;
      //we then project ptOff onto the norm; this will direct ptOff straight to its place on the normal.
      val proj = ptOff proj norm;
      //if ptOff dot norm is negative, then they're going in "opposite" directions, and we have a Left.
      //Otherwise it's a right
      if((ptOff dot norm) < 0) Left(proj + start)
      else                     Right(proj + start) //add start to go back to the world coordinate system.
    }
  }

  case class Intersection(loc:Vec3, norm:Vec3)

  trait Object {
    def intersect(r:Ray):Option[Intersection]
  }

  case class Sphere(loc:Vec3, rad:Float) extends Object {
    def intersect(r:Ray) = {
      //this amounts to finding the closest that the ray gets to the loc and seeing if that's less than rad. If there is,
      //we have an intersection.
      r.closestPoint(loc) match {
        case Left(p) => None//not a match
        case Right(p) => {
          if((p distTo loc) > rad) None
          else {
            //closest point is within the sphere. We know that r.norm is in fact normal to (p - loc), so we traverse backwards a distance
            //rad - (p distTo loc).
            val iLoc = p - r.norm * (rad - (p distTo loc));
            //we also know that the normal of any point on a sphere is in fact just that point from the origin, so we have
            val iNorm = (iLoc - loc).normalize
            Some(Intersection(iLoc, iNorm));
          }
        }
      }
    }
  }

  case class Camera(loc:Vec3, lookAt:Vec3, up:Vec3) {
    assert(abs(1-up.mag) < 1e-3f);
    assert(abs((lookAt-loc).dot(up)) < 1e-3f);

    //Returns a vector that points into the screen.
    def screenNorm = (lookAt - loc).normalize
  }
  case class Viewport(fovy:Float, fovx:Float)

  private sealed trait Light {
    def color:Int

    /**
     * Float from [0, 1], where 0 means that there's no contribution from this light and 1 means there's full contribution
     * @return
     */
    def contrib(i:Intersection):Float
    def colorFor(i:Intersection):(Float, Float, Float) = {
      val c = contrib(i);
      (red(color)*c, green(color)*c, blue(color)*c)
    }
  }
  private case class Ambient(color:Int) extends Light {
    def contrib(i:Intersection) = 1
  }
  private case class Directional(color:Int, norm:Vec3) extends Light {

    def contrib(i: Intersection) = 0f max (i.norm dot norm)
  }

  var cam = Camera(Vec3.Z * 200, Vec3(), -Vec3.Y);
  var viewport = Viewport(PI/3, PI/3)

  private var scene:Seq[Object] = (0 until 10).map(_ => Sphere(Vec3.random * 100, 10))//Seq(Sphere(Vec3(0, 0, 5), 3));
//  private def scene:Seq[Object] = Seq(Sphere(Vec3.fromSpherical(2, millis()/1000f, millis()/2500f), 2))
  private var myLights:Seq[Light] = Seq(Ambient(color(128)), Directional(color(128, 0, 0), Vec3(1, 0, 1)));

  override def setup() {
    size(300, 300)
  }

  override def draw() {
    var graphics:PGraphics = null
    if(keyPressed:Boolean) {
      val pg = createGraphics(width/3, height/3, JAVA2D);
      graphics = pg;

      /**
       * Given the screen x/y coordinate, this method returns the color at the given coordinate.
       * @param x
       * @param y
       * @return
       */
      def raytrace(sx:Int, sy:Int):Int = {
        val ray = {
          //x cross y = z => z cross x = y => y cross z = x.
          //We know Z is -screenNorm, and Y is up
          //we are in a left-handed coordinate system.
          val z = -cam.screenNorm
          val y = cam.up
          val x = y cross z

          val dThetaX = map(sx, 0, width, -viewport.fovx/2, viewport.fovx/2)
          val dThetaY = map(sy, 0, height, -viewport.fovy/2, viewport.fovy/2)
          //

          val norm = -z + y * sin(dThetaY) + x * sin(dThetaX);
          Ray(cam.loc, norm.normalize);
        }
        scene.flatMap{ s => s.intersect(ray).map((s, _)) } match {
          case Seq((obj, isect), _) => color(255);//((v:Vec3) => color(v.x, v.y, v.z))(myLights.map(_.colorFor(isect)).foldLeft(Vec3())(_ + Vec3(_)))
          case _ => color(0);
        }
      }

      pg.beginDraw();
      pg.loadPixels()
      for(x <- (0 until pg.width); y <- 0 until pg.height) {
        pg.pixels(y*pg.width+x) = raytrace(x, y)
  //      println("Done "+x+", "+y)
      }
      pg.updatePixels()
      pg.endDraw();
    } else {
      val pg = createGraphics(width, height, P3D).asInstanceOf[PGraphics3D];
      graphics = pg;

      pg.beginDraw();
      pg.background(204);

      pg.camera(cam.loc.x, cam.loc.y, cam.loc.z,
                cam.lookAt.x, cam.lookAt.y, cam.lookAt.z,
                cam.up.x, cam.up.y, cam.up.z)

      pg.perspective(viewport.fovy, viewport.fovx / viewport.fovy, .01f, 10000);

      myLights foreach {_ match {
        case Ambient(c) => pg.ambientLight(red(c), green(c), blue(c))
        case Directional(c, n) => pg.directionalLight(red(c), green(c), blue(c), n.x, n.y, n.z)
      }}

      pg.noStroke(); pg.fill(255);
      scene foreach {_ match {
        case Sphere(loc, rad) => {pg.pushMatrix; pg.translate(loc.x, loc.y, loc.z); pg.sphere(rad); pg.popMatrix; }
      }}

      pg.endDraw();

    }
    image(graphics, 0, 0, width, height)

    pollSave() //check if the screen should be saved
  }

  override def keyPressed() {
    super.keyPressed(); //pressing space toggles a boolean variable to save the screen
  }
}