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
      if(saving && !trulyOnline) saveFrame(prefix+this.getClass.getSimpleName.filter('$'!=)+"-####.png")
    }

    abstract override def keyPressed() {
      super.keyPressed();
      if(key == ' ') saving = !saving;
    }
  }


  trait SphereUtils extends MyPApplet {
    import PApplet._

    /**
     * Precondition: norm is orthogonal to v.<br />
     * We imagine the great circle described by the norm vector. v will
     * be on the great circle. This method will return a Vec3, on the great circle, a distance norm.mag away from
     * v (using spherical distance, not euclidean). Interpret the magnitude of v to be the radius of the sphere. <br /><br />
     * This method is the spherical equivalent to adding a velocity vector to a location vector; the location vector is v, and the
     * velocity vector has magnitude |norm| and direction perpendicular to norm.
     * @param v Vector on the great circle
     * @param norm Vector describing the great circle
     * @return A Vec3 also on the great circle a distance norm.mag away from v, or v itself if norm is the zero vector.
     */
    def move(v: Vec3, norm: Vec3) = if(norm.isZero || v.isZero) v else {
        //    val a = rotateAtoBMat(Vec3.X, v); //so we can treat the X axis as v.
        //    a.apply(rotateAtoBMat(transformed(Vec3.Z, a), norm));
        val rot = rotatePlaneAtoBMat(Vec3.X, Vec3.Z, v, norm);
        val theta = norm.mag / v.mag;
        val nx = Vec2.fromPolar(v.mag, theta).xy
        //if(!a.invert()) sys.error("a didn't invert! it is "+a.get(null))
        //run nx through a and return it
        transformed(nx, rot)
      }

    /**
     * Returns the spherical distance between v1 and v2; the sphere is assumed to have a radius |v1|. This is the
     * spherical analogue to the Euclidean distance (v2 - v1).mag
     * @param v1 One point on the sphere
     * @param v2 Second point on the sphere.
     */
    def distS(v1:Vec3, v2:Vec3) = v1.mag * (v1 angleBetween v2)

    /**
     * Precondition: vX and vY are linearly independent, v is on the plane spanned by vX and vY, vX and vY are orthogonal.
     * Given a vector v on a plane spanned by vX and vY, this method decomposes that vector into its vX and vY components,
     * returning the value in a Vec2.
     */
    def to2D(v:Vec3, vX:Vec3, vY:Vec3) = {
      if(abs(vX dot vY) > .01f) sys.error("vX not ortho to vY! "+vX+", "+vY+", "+vX.dot(vY))
      if(!v.proj(vX cross vY).withinBounds(.01f)) sys.error("v is not on the plane spanned by vX, vY! "+vX+", "+vY+", "+v.proj(vX cross vY))
      //since they're orthogonal, we just have to do a projection
      //v = x1 * v1 + x2 * v2; v . v1 = x1 * v1 . v1 + 0 => x1 = v . v1 / (v1 . v1)
      Vec2(v.dot(vX) / vX.mag2, v.dot(vY)/ vY.mag2)
    }

    /**
     * Draw the great circle segment that connects h1 and h2; the radius of the sphere is assumed to be
     * v1.mag
     * @param h1 One endpoint of the segment
     * @param h2 Other endpoint of the segment
     */
    def gcArc(h1: Vec3, h2: Vec3) {
      pushMatrix();

      //    val zToNorm = rotateAtoBMat(Vec3.Z, h1 cross h2)
      //    val xtoh1 = rotateAtoBMat(transformed(Vec3.X, zToNorm), h1);
      //    xtoh1.apply(zToNorm);
      //    applyMatrix(xtoh1); //xtoh1 correctly transforms vectors.

      applyMatrix(rotatePlaneAtoBMat(Vec3.X, Vec3.Z, h1, h1 cross h2))

      arc(0, 0, h1.mag, h1.mag, 0, h1 angleBetween h2);
      popMatrix();
    }

    /**
     * Draws a great circle that includes both points, with radius = |h1|.
     * @param h1 One vector on the great circle
     * @param h2 Another vector on the great circle
     */
    def greatCircle(h1: Vec3, h2: Vec3) {
      greatCircle((h1 cross h2), h1.mag)
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
    def greatCircle(norm: Vec3, rad:Float) {
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