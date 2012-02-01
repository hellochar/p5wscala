package daily


/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 6/30/11
 * Time: 3:58 PM
 */
import processing.core._
import geomerative._
import scala.util.Random

class Jun30 extends PApplet with NameApplet {
  import PApplet._; import PConstants._; 

  var name:RShape = _
  override def setup() {
    size(1024, 300)
    name = getNameShape(100, "ariblk.ttf", CENTER, "X I A O H A N")
    println("name's name: "+name.name)
    val points = name.children.foreach(f => println("name: "+f.name+", children: "+f.countChildren()+", paths: "+f.countPaths()))
//    name.translate(width/2, height/2)
    smooth()
  }

//  def separation = map(mouseX, 0, width, 3, 10).asInstanceOf[Int]

  val separation = 10

  override def draw() {
    val rangeX = 0 until width by separation
      val rangeY = 0 until height by separation
    background(0);
//    noFill; stroke(0);
    setRShapeCenter(name, mouseX, mouseY)
    noStroke; fill(255, 55, 0)
//    val timeStart = System.currentTimeMillis()
//    for(i <- rangeX) {
//      for(j <- rangeY) {
////        val rad = if(name.contains(new RPoint(i, j))) separation*.45f else 3
//        val rad = if(containsFast(name, new RPoint(i, j))) separation*.45f else 3
//        ellipse(i, j, rad, rad)
//      }
////      println(i)
//    }
//    val timeEnd = System.currentTimeMillis()
//    println(frameCount+": "+(timeEnd - timeStart))
//    val points = name.getPointsInPaths
//    for(pointsArr <- points) {
//      for(p <- pointsArr) {
//        ellipse(p.x, p.y, 5, 5)
//      }
//    }
//    name.drawVF(this)
    val r = new Random(1243);
    def random(v:Float) = r.nextFloat()*v;
//    name.children.foreach{ fill(random(255), random(255), random(255)); _.drawVF() }
//    name.children.zipWithIndex.foreach{_ match { case (shape, index) => { fill(index*15); shape.drawVF() }}}
    val paths = name.children.map(_.paths).flatten
    paths.map(_.getHandles).flatten.zipWithIndex.foreach(_ match { case (rpoint, i) => {text(i, rpoint.x, rpoint.y); ellipse(rpoint.x, rpoint.y, 5, 5) }})
//    paths.foreach{fill(random(255)); stroke(random(255), random(255), 0); _.drawVF()}
  }


  /**
 * Use this to return a specific tangent on the curve.  It returns true if the point passed as a parameter is inside the shape.  Implementation taken from: http://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html
 * @param p  the point for which to test containement..
 * @return boolean, true if the point is in the path.
 * */
  def containsFast(r:RShape, p: RPoint): Boolean = {
    var testx: Float = p.x
    var testy: Float = p.y
    val tleft = r.getTopLeft
    val bright = r.getBottomRight
    var xmin: Float = tleft.x
    var xmax: Float = bright.x
    var ymin: Float = tleft.y
    var ymax: Float = bright.y
    if ((testx < xmin) || (testx > xmax) || (testy < ymin) || (testy > ymax)) {
      return false
    }
    var pointpaths: Array[Array[RPoint]] = r.getPointsInPaths //Grab an array holding all of the point arrays in the name
    if (pointpaths == null) {
      return false
    }
//    def slowPopulate() = {
//      var verts: Array[RPoint] = pointpaths(0); //Get the first shape's points (X's points)
//        var k: Int = 1
//        while (k < pointpaths.length) {
//          //for every point array
//            verts = append(verts, new RPoint(0F, 0F)).asInstanceOf[Array[RPoint]] //add a (0, 0) point to the end of the array
//            verts = concat(verts, pointpaths(k)).asInstanceOf[Array[RPoint]] //add all of the points to the array
//            k += 1;
//        }
//      append(verts, new RPoint(0F, 0F)).asInstanceOf[Array[RPoint]]
//    }
//
//    val verts = slowPopulate()
//
//    //basically the previous method flattens the 2d array into a massive 1d array, with a (0, 0) point after each shape
//    if (verts == null) {
//      return false
//    }
    var verts: Array[RPoint] = pointpaths(0);
    {
      var k: Int = 1;
      while (k < pointpaths.length) {
        {
          verts = append(verts, new RPoint(0F, 0F)).asInstanceOf[Array[RPoint]]
          verts = concat(verts, pointpaths(k)).asInstanceOf[Array[RPoint]]
        }
        ({
          k += 1; k
        })
      }
    }

    verts = append(verts, new RPoint(0F, 0F)).asInstanceOf[Array[RPoint]]

    if (verts == null) {
      return false
    }

    var nvert: Int = verts.length
    var i: Int = 0
    var j: Int = 0
    var c: Boolean = false;
    {
      i = 0
      j = nvert - 1
      while (i < nvert) {
        {
          if (((verts(i).y > testy) != (verts(j).y > testy)) && (testx < (verts(j).x - verts(i).x) * (testy - verts(i).y) / (verts(j).y - verts(i).y) + verts(i).x)) {
            c = !c
          }
        }
        j = ({
          i += 1; i
        })
      }
    }
    return c
  }

}