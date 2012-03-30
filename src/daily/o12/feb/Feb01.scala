package daily


/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 2/1/12
 * Time: 1:50 PM
 */

import processing.core._
import org.zhang.lib.MyPApplet
import org.zhang.geom.{Vec2, Vec3}

object Feb01Main extends App {
  PApplet.main(Array("daily.Feb01"))
}

class Feb01 extends MyPApplet with Savable with SphereUtils {

  randomSeed(1)
  noiseSeed(1)

  val raytracer = new RayTracer();
  import raytracer._
  import PApplet._;
  import PConstants._;

  case class Sphere(locc:Vec3, radd:Float) extends raytracer.Sphere(locc, radd) {
    val (oa, oz) = (random(5), random(5))
//    def colorFor(norm:Vec3) = Color(1 - pow(noise(norm.angle+oa, norm.angleZ+oz), 4))
    def colorFor(norm:Vec3) = Color(1)
  }

  override def millis() = {
    millisVar
  }

//  def cam = Camera(Vec2.fromPolar(200, millis()/1000f).xz, Vec3(), -Vec3.Y);
  var cam = new Camera(Vec2.fromPolar(200, millis()/1000f).xz, Vec3(), -Vec3.Y);
  def viewport = Viewport(PI/3, PI/3 * width / height)

  private var scene:Seq[Object] = (0 until 5).map(_ => new Sphere(Vec3.random * 100, 10))//Seq(Sphere(Vec3(0, 0, 5), 3));
//  private def scene:Seq[Object] = Seq(Sphere(Vec3.fromSpherical(2, millis()/1000f, millis()/2500f), 2))
//  private var myLights:Seq[Light] = Seq(
  private var myLights:Seq[Light] = Seq(
    Ambient(Color(.5f)),
    Directional(Color(.5f, 0, 0), Vec3(1, 0, 1).normalize),
    Point(Color(0, .5f, 0), Vec3(), .01f));

  private var millisVar = 0;
  private var msAvg = -1f;
  lazy val pg = createGraphics(width, height, P2D);
  override def setup() {
    size(300, 300)
    colorMode(RGB, 1, 1, 1)
  }

  override def draw() {
    millisVar += 16;
//    cam.loc
    cam.loc = Vec2.fromPolar(map(sin(millis()/3000f), -1, 1, 20, 200), millis()/1000f).xz
    var graphics:PGraphics = null
    if(!(keyPressed:Boolean)) {
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

          val dThetaX = map(sx, 0, pg.width, -viewport.fovx/2, viewport.fovx/2)
          val dThetaY = map(sy, 0, pg.height, -viewport.fovy/2, viewport.fovy/2)
          //

          val norm = -z + y * sin(dThetaY) + x * sin(dThetaX);
          Ray(cam.loc, norm.normalize);
        }
        scene.flatMap{ s => s.intersect(ray).map((s, _)) }.sortBy{_._2.loc distTo cam.loc}.headOption match {
          case Some((obj, isect)) => {

            val sum = myLights.map(_.contribution(isect)).foldLeft(Color(0))(_ + _)
            def fogExp = 1
//            def fogExp = exp(-((isect.loc distTo cam.loc) / 200))
//            println("fogExp:"+fogExp)
            val fs = sum * fogExp
            color(fs.r, fs.g, fs.b)
          }
          case None => color(.8f)
        }
      }

      pg.beginDraw()
      pg.loadPixels()
      val iter = (0 until pg.width).flatMap {x => (0 until pg.height).map {(x, _)}}.par
      val nt = org.zhang.lib.time(iter.foreach {
              case (x, y) => pg.pixels(y*pg.width+x) = raytrace(x, y)
            })._2 / 1e6f
      if(msAvg < 0) msAvg = nt;
      else          msAvg = nt * .5f + msAvg * .5f;
      println(frameCount+"\t"+msAvg)
      pg.updatePixels()
      pg.endDraw();
//      saveFrame("Feb01-####.jpg")
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
        case Ambient(c) => pg.ambientLight(c.r*255, c.g*255, c.b*255)
        case Directional(c, n) => pg.directionalLight(c.r*255, c.g*255, c.b*255, -n.x, -n.y, -n.z)
        case Point(c, l, quad) => {
          pg.lightFalloff(0, 0, quad);
          pg.pointLight(c.r*255, c.g*255, c.b*255, l.x, l.y, l.z)
        }
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

class RayTracer {
  import processing.core.PApplet._, processing.core.PConstants._;
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
      val dot = ptOff dot norm;
      //we then project ptOff onto the norm; this will direct ptOff straight to its place on the normal.
      val proj = norm * dot;
      //if ptOff dot norm is negative, then they're going in "opposite" directions, and we have a Left.
      //Otherwise it's a right
      if(dot < 0) Left(proj + start)
      else        Right(proj + start) //add start to go back to the world coordinate system.
    }
  }

  case class Color(r:Float, g:Float, b:Float) {
    def *(f:Float) = Color(r*f, g*f, b*f)
    def *(c:Color) = Color(r*c.r, g*c.g, b*c.b)
    def +(c:Color) = Color(r+c.r, g+c.g, b+c.b)
  }
  object Color {
    def apply(g:Float):Color = Color(g, g, g)
  }

  case class Intersection(loc:Vec3, norm:Vec3, color:Color) {
    assert(abs(1-norm.mag)<.01f)
  }

  trait Object {
    def intersect(r:Ray):Option[Intersection]
  }

  abstract class Sphere(val loc:Vec3, val rad:Float) extends Object {
    def colorFor(norm:Vec3):Color
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
            Some(Intersection(iLoc, iNorm, colorFor(iNorm)));
          }
        }
      }
    }
  }
  case class Plane(norm:Vec3, ptOn:Vec3) extends Object {
    def intersect(r: Ray) = {
      //if ray's direction dotted with norm is the same sign as
      None
    }
  }

  class Camera(var loc:Vec3, var lookAt:Vec3, var up:Vec3) {
    assert(abs(1-up.mag) < 1e-3f);
    assert(abs((lookAt-loc).dot(up)) < 1e-3f);

    //Returns a vector that points into the screen.
    def screenNorm = (lookAt - loc).normalize
  }
  case class Viewport(fovy:Float, fovx:Float)

  sealed trait Light {
    def color:Color

    /**
     * Float from [0, 1], where 0 means that there's no contribution from this light and 1 means there's full contribution
     * @return
     */
    def contrib(i:Intersection):Float
    def contribution(i:Intersection):Color = {
      color * contrib(i) * i.color
    }
  }
  case class Ambient(color:Color) extends Light {
    def contrib(i:Intersection) = 1
  }
  case class Directional(color:Color, norm:Vec3) extends Light {
    assert(abs(1-norm.mag)<.01f)
    def contrib(i: Intersection) = 0f max (i.norm dot norm)
  }
  case class Point(color:Color, loc:Vec3, quad:Float) extends Light {
    def contrib(i: Intersection) = max(0f, (i.norm dot -(i.loc - loc).normalize)) * 1 / (quad * (i.loc - loc).mag)// / ((i.loc - loc).mag2 * falloff))
  }

}