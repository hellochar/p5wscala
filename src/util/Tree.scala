package util

import processing.core.PConstants._
import processing.core.{PApplet, PGraphics, PImage}

class Tree(val height: Float, val extent: Float, val baseRadius: Float, trunkColor: Int = 0xFF7D320A, greenMin: Float = 128, greenMax: Float = 255, val leavesNum: Int = 50, leafHeightMin: Float = .7f, leafHeightMax: Float = 1) {

  var leafTexture: PImage = null

  private implicit def tupleF3ToAddable(t: (Float, Float, Float)) = new AddOp(t)
  implicit def d2f(d: Double) = d.toFloat

  import math._


  val lhmin = leafHeightMin * height
  val lhmax = leafHeightMax * height

  lazy val leafs = (0 until leavesNum).map(i => new Leaf)

  protected class Leaf/*(owner:Branch)*/ {

    val myColor = randGreen

    val baseLow = (0f, 0f, random(lhmin, lhmax))
    val baseHigh = (0f, 0f, random(baseLow._3, lhmax))

    val avgLH = (lhmin + lhmax) / 2
    val extentHigh = fromSpherical(random(extent / 2, extent), random(0, Pi * 2), random(0, Pi / 2)) +(0, 0, avgLH)
    val extentLow = fromSpherical(random(extent / 2, extent), random(0, Pi * 2), -random(0, Pi / 2)) +(0, 0, avgLH)


    def draw(g: PGraphics) {
      import g._
      if (leafTexture == null) {
        fill(myColor)
        import g.{vertex => vtx3}
        def vertex(t: (Float, Float, Float) /*, u:Float, v:Float*/) {
          vtx3(t._1, t._2, t._3)
        }

        vertex(baseLow);
        vertex(baseHigh);
        vertex(extentHigh);
        vertex(extentLow);
      }
      else {
        import g.{vertex => vtx5}
        texture(leafTexture)
        textureMode(NORMAL)
        def vtx(t: (Float, Float, Float), u: Float, v: Float) {
          vtx5(t._1, t._2, t._3, u, v)
        }
        vtx(baseLow, 0, 1);
        vtx(baseHigh, 0, 0);
        vtx(extentHigh, 1, 0);
        vtx(extentLow, 1, 1)
      }
    }
  }

  def randGreen = mkGreen(random(greenMin, greenMax))


  def draw(p: PApplet) {
    draw(p.g)
  }

  def draw(g: PGraphics) {
    import g.{height => _, _}
    noStroke();

    def drawTrunk() {
      fill(trunkColor)
      //todo: better implementation of trunk
      pushMatrix();
      translate(0, 0, height / 2);
      val sq2 = sqrt(2)
      box(baseRadius / sq2, baseRadius / sq2, height)
      popMatrix();
    }

    drawTrunk()
    beginShape(QUADS)
    leafs.foreach(_.draw(g))
    endShape()
  }


  //===============UTILITY FUNCTIONS/CLASSES====================
  /**
  * Create a Vec3 from the given spherical coordinates. r is the radius, t is the azimuth angle (synonymous with theta in Vec2),
  * and angleZ is the elevation angle (also called latitude), generally ranging from -PI/2 (for the negative Z axis) to PI/2 (for the positive Z axis)
  *
  */
  private def fromSpherical(r: Float, t: Float, angleZ: Float) = {
    import math._; (r * (cos(t) * cos(angleZ)).toFloat, r * (sin(t) * cos(angleZ)).toFloat, r * sin(angleZ).toFloat)
  }

  private def random(low: Float, high: Float) = (math.random.toFloat) * (high - low) + low

  private def mkGreen(greenAmt: Float) = ((greenAmt.toInt max 0) min 255) << 8 | 0xFF << 24

  private class AddOp(t: (Float, Float, Float)) extends (Float, Float, Float)(t._1, t._2, t._3) {
    def +(o: (Float, Float, Float)) = (t._1 + o._1, t._2 + o._2, t._3 + o._3)

    def -(o: (Float, Float, Float)) = (t._1 - o._1, t._2 - o._2, t._3 - o._3)
  }

}
