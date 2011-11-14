import java.awt.event.{MouseWheelEvent, MouseWheelListener}
import javax.swing.SwingUtilities
import org.zhang.geom.{Vec3, Vec2}
import org.zhang.lib.{MyPApplet, HasMV}
import peasy.PeasyCam
import processing.core.PApplet._
import processing.core.PConstants._
import processing.core.{PVector, PMatrix3D, PApplet}

//in file daily/package.scala
package object daily {
  val FONTS_DIR = "C:\\Windows\\Fonts\\"
  def findFont(name:String):String = FONTS_DIR+name
//    List(name, name.toUpperCase, name.toLowerCase, name.capitalize).foldLeft(null)((ret, name) =>
//      if(ret != null) ret else try {
//        FONTS_DIR+name+".ttf"
//  }catch {
//    case e: RuntimeException if e.getMessage.contains("Rename the file or change your code.") => {}
//  })

  class SizedQueue[A](val size:Int, init:A*) {
    private var seqq = init.toSeq

    def push(elem:A) {
      seqq = (elem +: seqq).take(size)
    }

    def seq = seqq
  }

  trait NameApplet extends PApplet with HasMV with Savable {
    import geomerative._

    RG.init(this)

    def addScrollWheelListener(name: RShape) {
      addMouseWheelListener(new MouseWheelListener() {
        def mouseWheelMoved(e: MouseWheelEvent) {
          SwingUtilities.invokeLater(new Runnable() {
            override def run() {
              val scale = pow(1.06f, e.getWheelRotation)
              name.transform(name.getX, name.getY, name.getWidth * scale, name.getHeight * scale)
            }
          });
        }
      })
    }

    def setRShapeCenter(name:RShape, x:Float, y:Float) = name.transform(x-name.getWidth/2, y-name.getHeight/2, name.getWidth, name.getHeight);

    def getNameShape(size:Int, fontName:String = "RAGE.TTF", alignment:Int = CENTER, name:String = "Xiaohan Zhang") = RG.getText(name, findFont(fontName), size, alignment)

    @deprecated("use MyPApplet instead")
    def line(v1:Vec2, v2:Vec2) { line(v1.x, v1.y, v2.x, v2.y) }
    @deprecated("Use MyPApplet instead")
    def ellipse(v1:Vec2, dims:Vec2) { ellipse(v1.x, v1.y, dims.x, dims.y) }

  }

  trait Savable extends PApplet {
    var saving = false;
    private[this] lazy val trulyOnline = try { //PApplet.online doesn't catch appletviewer.
      System.getProperty("user.dir")
      false
    } catch {
      case e: SecurityException => true
    }

    def pollSave(prefix:String = "") {
      if(saving && !trulyOnline) {
        val name = getClass.getName.drop(6) //daily.nov.Nov11 -> nov.Nov11
        val begin = "C:\\Users\\hellochar\\Documents\\dev\\NetBeansIntelliJ\\Daily\\out\\"+name.replace(".", "\\").toLowerCase+"-out\\"
        val path = begin+prefix+this.getClass.getSimpleName.filter('$'!=)+"-"+nf(frameCount, 4)+".png"
        g.save(path)
      }
    }

    abstract override def keyPressed() {
      super.keyPressed();
      if(key == ' ') saving = !saving;
    }
  }

  /**
   * This class provides methods to draw geometry in spherical coordinates such as great circles and small circles.
   */
  trait SphereUtils extends MyPApplet {

    /**
     * Draw the great circle segment that connects h1 and h2; the radius of the sphere is assumed to be
     * h1.mag.
     * @param h1 One endpoint of the segment
     * @param h2 Other endpoint of the segment
     */
    def gcArc(h1: Vec3, h2: Vec3) {
      pushMatrix();
      applyMatrix(org.zhang.lib.P5Util.rotatePlaneAtoBMat(Vec3.X, Vec3.Z, h1, h1 cross h2)) //todo: what if h1 and h2 are opposite? the cross becomes zero.
      arc(0, 0, h1.mag * 2, h1.mag * 2, 0, h1 angleBetween h2);
      popMatrix();
    }

    /**
     * Draws a great circle that includes both points, with radius = |h1|.
     * @param h1 One vector on the great circle
     * @param h2 Another vector on the great circle
     */
    def greatCircle(h1: Vec3, h2: Vec3) {
      greatCircle((h1 cross h2), h1.mag) //todo: what if h1 and h2 are linearly dependent?
    }

    /**
     * Draws a great circle created by intersecting a sphere of radius |norm| with a plane described
     * by norm.
     * @param norm Vector pointing normal to the plane
     */
    def greatCircle(norm:Vec3) { greatCircle(norm, norm.mag) }

    /**
     * Draws a great circle created by intersecting a sphere of radius rad with a plane described by norm.
     * @param norm Vector pointing normal to the plane.
     * @param rad Radius of the sphere.
     */
    def greatCircle(norm: Vec3, rad:Float) { //todo: what if norm is the zero vector?
      pushMatrix()
      rotateAtoB(Vec3.Z, norm)
      ellipse(0, 0, 2 * rad, 2 * rad)
      popMatrix()
    }

    /**
     * Draws the set of points a great-circle distance <code>dist</code> away from the vector <code>v</code>.
     * @param v Center of the circle
     * @param dist "radius" of the circle
     */
    def smallCircle(v: Vec3, dist: Float) {
      pushMatrix()
      rotateAtoB(Vec3.X, v)
      /* We have dist = rad * theta; so theta = dist / rad
       draw a circle of radius r = sin(theta) on the YZ plane, translated in the positive X direction by an amount
       x = cos(theta)
      */
      val rad = dist / v.mag
      translate(v.mag * cos(rad), 0, 0)
      rotateAtoB(Vec3.X, Vec3.Z)
      ellipse(0, 0, 2 * v.mag * sin(rad), 2 * v.mag * sin(rad))
      popMatrix()
    }

    def sphere(loc:Vec3, rad:Float) {
      pushMatrix()
      translate(loc)
      sphere(rad)
      popMatrix()
    }
  }

  /**
   * This trait provides a method to associate a position on the screen with a position on the surface of a sphere.
   */
  trait SphereSurfaceUI extends MyPApplet {
    import toxi.geom.{Vec3D, Sphere, Ray3D}
    import processing.core.PApplet._;
    implicit private def zhang2toxi(z:Vec3) = new Vec3D(z.x, z.y, z.z)
    implicit private def toxi2zhang(t:Vec3D) = Vec3(t.x, t.y, t.z)

    def cam:PeasyCam

    /**
     * This method returns the location of the intersection of a ray pointing from the current PeasyCam state towards
     * the current screen X and Y coordinates with a sphere of radius rad and center loc. If there is no intersection,
     * this method returns None.
     * @param rad Radius of sphere to test against
     * @param screenX x-coordinate of the screen to cast ray at
     * @param screenY y-coordinate of the screen to cast ray at
     * @param loc Center of the sphere to test against (defaults to the origin)
     */
    def getIntersect(screenX:Float, screenY:Float, rad:Float, loc:Vec3 = Vec3.ZERO):Option[Vec3] = {
      /* By default, the perspective is:
       * FoV = PI / 3
       * aspect = width / height = 1
       * So we have a total of PI/3 angular distance in both directions. The angle as a function
       * of mouse position is given by angle = arctan(x), where x = -PI/3 at mouseX = 0 and x = PI/3 at mouseX = width.
       * Since x and mouseX are linearly related we can just convert using map (x = map(mouseX, 0, width, -1/sqrt(3), 1/sqrt(3)).
       * We do the same calculation for y (only using mouseY and height). Now, our offset vector simply has components x and y
       * in the x and y axes of the screen. We then convert this vector into its corresponding a 3D world vector, add it to
       * the screen normal, and we have our direction for the ray to go in.
       * So,
       * a) calulate x and y
       * b) create the vec2
       * c) convert the vec2 into a vec3
       * d) add vec3 to screen normal
       *
       * In order to convert the vec2 into a vec3, we must find the x and y vectors in our current camera view. This amounts
       * to transforming Vec3.X/Y by our current camera transformation. PeasyCam provides a getRotations method that can give
       * us a matrix that converts from the camera to the world.
       */
      val (sx, sy) = (map(screenX, 0, width, -1/sqrt(3), 1/sqrt(3)), map(screenY, 0, height, -1/sqrt(3), 1/sqrt(3)));
      val rots = cam.getRotations()
      val camMatrix = new PMatrix3D()
      camMatrix.rotateX(rots(0))
      camMatrix.rotateY(rots(1))
      camMatrix.rotateZ(rots(2))
  //    val camMatrix = getMatrix();
  //    camMatrix.invert();
      val (tx, ty) = (camMatrix.mult(new PVector(1, 0), null), camMatrix.mult(new PVector(0, 1), null))

  //    stroke(255, 255, 0);
  //    line(tx ofMag 200);
  //    stroke(0, 255, 255)
  //    line(ty ofMag 200);

      tx.mult(sx); ty.mult(sy);
      tx.add(ty); //the linear combination is now held in tx.
      val dirOffset = tx; //The final, total offset of the ray

      val camPos = cam.getPosition
      val lookAt = cam.getLookAt
      val r = new Ray3D(camPos(0), camPos(1), camPos(2),
        new Vec3D(lookAt(0)-camPos(0), lookAt(1)-camPos(1), lookAt(2)-camPos(2)).normalize().add(dirOffset.x, dirOffset.y, dirOffset.z)) //start a ray at camera and going towards the center of the screen modified by the offset

      val sphere = new Sphere(loc, rad);
      val intersects = sphere.intersectRay(r)
      if(intersects == null) None
      else {
        val forward = intersects.filter(_ >= 0); //todo: optimize?
        if(forward.isEmpty) None
        else Some(r.getPointAtDistance(forward(0)))
      }
    }
  }
}