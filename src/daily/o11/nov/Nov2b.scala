package daily


/**
* Created by IntelliJ IDEA.
* User: hellochar
* Date: 11/2/11
* Time: 3:38 PM
*/
import processing.core._
import org.zhang.geom._
import zhang.Methods
import peasy.PeasyCam
import org.zhang.lib.{P5Util, MyPApplet}

/**
* <p>For any single point on the sphere, we call the "local plane" of that point to be the plane tangent to the sphere at that point.
* We call this point the "center" of the local plane. We can represent any point p on the sphere onto a local plane by taking p's
* projection onto the local plane and then scaling it to have magnitude equal to the great circle distance between p and the local plane's center.
* We can then do operations on on such points as if they were in simple 2D cartesian coordinates. To convert a point from its local plane representation
* back into the spherical surface, we express the 2d point in its equivalent 3d coordinates (e.g. using to3D), calling it P, and cross the (normalized center
* of the local plane in 3D coordinates) with P. The resulting 3D vector V is a rotation vector, normal to both the center and the direction in which you want to
* move from the center, and of magnitude equal to the quantity to move from the center. We can get the corresponding point by calling move(center, V). </p>
*
* <p>Specifically, this allows us to handle vectors calculated in local plane coordinates since the butt of the vector is situated at the local plane's origin, which
* coincides with its center, and the head of the vector is situated at a point, which we can then convert to 3D.</p>
*
* <p>How to handle antipodal point? The antipodal point will also be at the origin. So the LocalPlane isn't perfectly 1-1.</p>
*
* <p>The LocalPlane is actually an azimuthal projection.
*/
case class LocalPlane(center:Vec3) { //todo: what if center is zero?
  import P5Util._

  /**
   * This matrix rotates the +Z vector to the center.
   */
  val matr = rotateAtoBMat(Vec3.Z, center);
  /**
   * x-axis basis vector.
   */
  val vX = transformed(Vec3.X, matr)
  /**
   * y-axis basis vector.
   */
  val vY = transformed(Vec3.Y, matr) //vX and vY are guaranteed to be orthogonal and of unit length
  val norm = center.normalize

  /**
   * Same as toPlane(vs)
   */
  def apply(vs:Traversable[Vec3]) = toPlane(vs)

  /**
   * Same as toPlane(v)
   */
  def apply(v:Vec3):Vec2 = toPlane(v)

  /**
   * Same as toSphere(v)
   */
  def apply(v:Vec2):Vec3 = toSphere(v)

  def toPlane(vecs:Traversable[Vec3]):Traversable[Vec2] = vecs map (toPlane _)


  /**
   * This projects any point on a sphere onto this local plane. The given vector is assumed to have magnitude equal to the center's magnitude.
   */
  def toPlane(v:Vec3):Vec2 = v.projPlane(norm).as2D(vX, vY) ofMag distS(v, center)

  /**
   * Given a point on this local plane, toRotation returns a rotation vector that will transform the center into the 3D representation on the sphere.
   */
  def toRotation(v:Vec2) = norm cross v.as3D(vX, vY)

  def toSphere(v:Vec2) =
  {val r = toRotation(v); center.rotate(r normalize, r.mag / center.mag)}

  /**
   * Given a point in a different LocalPlane, returns the corresponding point in this one. The returned Vec2 represents
   * the same point on the sphere in this LocalPlane as the given Vec2 does in the given LocalPlane.
   * @param v Point in the other LocalPlane
   * @param other Other LocalPlane
   * @return That same point represented in this LocalPlane
   */
  def convert(v:Vec2, other:LocalPlane) = toPlane(other.toSphere(v)) //todo: some shit is crazy here.

  /**
   * Given a point in a different LocalPlane, returns a point in this one of equal magnitude but pointing at the same spherical
   * point the old one did.
   */
  def convertAngle(v:Vec2, other:LocalPlane) = convert(v, other) ofMag v.mag
//    move(center, toRotation(v))

//  /**
//   * Returns a matrix that converts 3D vectors of the form (x, y, 0)
//   */
//  def invTransformMat = {
//    val mat = matr.get()
//    mat.translate(0, 0, center.mag)
//    mat;
//  }
}

/**
* In this sketch I want to be able to get a framework or show a proof of concept of adding velocities and forces in spherical coordinates. Consider
* if you have the points p = (1, 0, 0), r1 = (0, 1, 0) and r2 = (0, 0, 1). p wants to get "repelled" by both of these points. For each point r1 or r2,
* I can imagine a transform that represents the repelling force that that point exerts on p. Translations on the sphere are represented as rotations
* around some principle axis. So, we can represent this repelling force by a rotation transform; this in turn can be represented using either an axis angle
* or a quaternion.
*/
class Nov2b extends MyPApplet with Savable with SphereUtils with SphereSurfaceUI {
   import PConstants._;

  def POW = 1000

  def forceOnPlane(me:Vec3, others:Traversable[Vec3]) = (locPlane.toPlane(others).map(o => Vec2.invR2(Vec2(), o) * -POW) fold Vec2())(_ + _)

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
    Methods.drawAxes(g, 200)
    lights(); noStroke(); fill(255); sphereDetail(30);
    sphere(100);

    fill(0, 255, 0);
    sphereDetail(8);
    others map {x => sphere(x, 5)}
    val plane = locPlane; //loc's local plane for this frame.

    { //do loc's logic
      fill(255, 0, 0);
      sphere(loc, 5);

      stroke(255, 255, 0);
      pushMatrix()
      rotateAtoB(Vec3.Z, loc);
      translate(0, 0, 102) //we're now in loc's local plane.
      line(force2D * 100)
      stroke(255, 0, 255);
      line(vel * 100)
      //draw grid:
      //  draw horizontal lines
      //  draw vertical lines
      stroke(255); noFill()
      //The plane extends 100PI in each direction (each GC has a circumference of 2PI*radius; radius = 100 and we have
      //both positive and negative coordinates so each coordinate goes from -100PI to 100PI.
//      for(coord <- Range.Double(-100*PI, 101*PI, (200*PI) / 20).map(_.toFloat)) {
//        line(coord, -100*PI, coord, 100*PI)
//        line(-100*PI, coord, 100*PI, coord)
//      }
      for(rad <- Range.Double(0, 100*PI+10, 100*PI / 10).map(_.toFloat)) { //draw concentric circles. the +1 on the end makes the range inclusive on the upper bound.
        ellipse(0, 0, rad*2, rad*2)
      }
      for(ang <- Range.Double(0, TWO_PI, TWO_PI / 10).map(_.toFloat)) { //draw gc-segments
        line(Vec2.fromPolar(100*PI, ang))
      }

      //draw others
      others.map(plane.toPlane _).foreach(x => sphere(x.xy, 5))

      popMatrix()

      stroke(0)
      line(plane.toRotation(vel) ofMag 200)

      vel += force2D * DT;
      loc = plane.toSphere(vel * DT) ofMag 100;

      //we now have a new local plane. We need to convert vel from the old plane into the new one.
      val newPlane = locPlane; //mutability lulz
//      Predef.print("before: "+vel.mag) //can't use normal print because that is resolved to Component.print(graphics)
      vel = newPlane.convertAngle(vel, plane) //we want to change the direction but keep the magnitude
//      println(", after: "+vel.mag)

    }

    pollSave()
  }

  override def keyPressed() {
    super.keyPressed();
    if(key == 'r') loc = -Vec3.Z * 100;
    if(key == 's') vel = Vec2();
  }
}