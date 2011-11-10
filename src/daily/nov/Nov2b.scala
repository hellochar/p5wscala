package daily
package nov

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 11/2/11
 * Time: 3:38 PM
 */
import processing.core._
import org.zhang.lib.MyPApplet
import org.zhang.geom.{Vec2, Vec3}
import zhang.Methods
import peasy.PeasyCam

/**
 * In this sketch I want to be able to get a framework or show a proof of concept of adding velocities and forces in spherical coordinates. Consider
 * if you have the points p = (1, 0, 0), r1 = (0, 1, 0) and r2 = (0, 0, 1). p wants to get "repelled" by both of these points. For each point r1 or r2,
 * I can imagine a transform that represents the repelling force that that point exerts on p. Translations on the sphere are represented as rotations
 * around some principle axis. So, we can represent this repelling force by a rotation transform; this in turn can be represented using either an axis angle
 * or a quaternion.
 *
 *
 * I can also pretend like we're in cartesian land and do repelling in 3D coordinates that way; this gives me the correct direction of the final vector.
 * The rotation vector describing this rotation is the final repel vector crossed with my position, of proper magnitude.
 *
 * For any single point on the sphere, we call the "local plane" of that point to be the plane tangent to the sphere at that point.
 * We call this point the "center" of the local plane. We can represent any point p on the sphere onto a local plane by taking p's
 * projection onto the local plane and then scaling it to have magnitude equal to the great circle distance between p and the local plane's center.
 * We can then do operations on on such points as if they were in simple 2D cartesian coordinates. To convert a point from its local plane representation
 * back into the spherical surface, we express the 2d point in its equivalent 3d coordinates (e.g. using to3D), calling it P, and cross the (normalized center
 * of the local plane in 3D coordinates) with P. The resulting 3D vector V is a rotation vector, normal to both the center and the direction in which you want to
 * move from the center, and of magnitude equal to the quantity to move from the center. We can get the corresponding point by calling move(center, V).
 *
 * Specifically, this allows us to handle vectors calculated in local plane coordinates since the butt of the vector is situated at the local plane's origin, which
 * coincides with its center, and the head of the vector is situated at a point, which we can then convert to 3D.
 *
 * How to handle antipodal point? The antipodal point will also be at the origin. So the LocalPlane isn't perfectly 1-1
 */
class Nov2b extends MyPApplet with Savable with SphereUtils with SphereSurfaceUI {
  import PApplet._; import PConstants._;

  case class LocalPlane(center:Vec3) {
    val matr = rotateAtoBMat(Vec3.Z, center);
    val vX = transformed(Vec3.X, matr)
    val vY = transformed(Vec3.Y, matr) //vX and vY are guaranteed to be orthogonal and of unit length
    val norm = center.normalize

    def toPlane(vecs:Traversable[Vec3]):Traversable[Vec2] = vecs map (toPlane _)

    /**
     * This projects any point on a sphere onto this local plane. The given vector is assumed to have magnitude equal to the center's magnitude.
     */
    def toPlane(v:Vec3):Vec2 = to2D(v.projPlane(norm), vX, vY) ofMag distS(v, center)

    def toRotation(v:Vec2) = norm cross v.onPlane(vX, vY)

    def toSphere(v:Vec2) = move(center, toRotation(v))
  }

  def POW = map(mouseX, 0, width, 0, 5000f)

  def forceOnPlane(me:Vec3, others:Traversable[Vec3]) = (locPlane.toPlane(others).map(o => o ofMag (-POW / o.mag2)) fold Vec2())(_ + _)

  var loc = Vec3(100, 0, 0);
  var vel = Vec2();
  def locPlane = LocalPlane(loc)

  def force2D = forceOnPlane(loc, others)
  def DT = .1f//map(mouseX, 0, width, 0, 1f)//.05f;

  val others = (0 until 10) map (_ => Vec3.random * 100)

  lazy val cam = new PeasyCam(this, 200);
  override def setup() {
    size(500, 500, P3D)
    cam
  }

  override def draw() {
    background(64)
//    Methods.drawAxes(g, 20)
    lights(); noStroke(); fill(255); sphereDetail(30);
    sphere(100);

    fill(0, 255, 0);
    sphereDetail(8);
    others map {x => sphere(x, 5)}

    println(loc.mag)

    { //do loc's logic
      fill(255, 0, 0);
      sphere(loc, 5);

      stroke(255, 255, 0);
      pushMatrix()
      rotateAtoB(Vec3.Z, loc);
      translate(0, 0, 102)
      line(force2D * 100)
      popMatrix()
      vel += force2D * DT;
      loc = locPlane.toSphere(vel * DT);
//      loc = locPlane.toSphere(force2D * DT)
    }

    pollSave()
  }
  
  override def keyPressed() {
    super.keyPressed();
    if(key == 'r') loc = Vec3.random * 100;
  }
}