package daily
package dec

/**
* Created by IntelliJ IDEA.
* User: hellochar
* Date: 12/19/11
* Time: 11:36 AM
*/

import processing.core._
import zhang.Methods
import peasy.PeasyCam
import org.zhang.geom.{Vec2, Vec3}
import org.zhang.lib.{P5Util, MyPApplet}
import javax.swing.SwingUtilities
import java.awt.event.MouseEvent
import toxi.color.{TColor, ColorGradient}
import saito.objloader.OBJModel

class Dec19 extends MyPApplet with Savable { app =>
  import PApplet._; import PConstants._
  implicit def man2vec3(m:Man) = m.pos

  /**
   * Azimuth ranges [0, TWO_PI] and altitude ranges (-PI/2, PI/2) (where PI/2 is straight up and -PI/2 is straight down)
   */
  class Man(var gpos:Vec2, var azimuth:Float, var altitude:Float = 0) {

    def speed = 5f

    def height = 30

    /**
     * The man's feet in 3D global coordinates
     */
    def pos = Landscape.to3D(gpos); //.xy + Vec3.Z * Landscape.gridAt(gpos)

    def facePos = pos + Vec3.Z * height

    /**
     * The orientation (the direction his face is facing) of the man as a unit vector.
     */
    def otn = Vec3.fromSpherical(1, azimuth, altitude)
  }

  object Landscape {
    val zScl:Float = 1000
    def gridAt(pos:Vec2) = noise(125 + pos.x / 1000, -60 + pos.y / 1000) * zScl

    def to3D(v:Vec2) = v.xy + Vec3.Z * gridAt(v)

    val cGradient = new ColorGradient()
    cGradient.addColorAt(0*zScl, TColor.BLACK)
    cGradient.addColorAt(.1f*zScl, TColor.newHex("000C3B"))
    cGradient.addColorAt(.2f*zScl, TColor.newHex("0827A1"))
    cGradient.addColorAt(.201f*zScl, TColor.newHex("FBFFB3"))
    cGradient.addColorAt(.21f*zScl, TColor.newHex("75C90E"))
    cGradient.addColorAt(.31f*zScl, TColor.newHex("75C90E"))
  //  cGradient.addColorAt(1f*zScl, TColor.newHex("197500"))
    cGradient.addColorAt(.45f*zScl, TColor.newHex("197500"))
    cGradient.addColorAt(.65f*zScl, TColor.newHex("785400"))
    cGradient.addColorAt(.75f*zScl, TColor.newHex("8F3700"))
    cGradient.addColorAt(.80f*zScl, TColor.newHex("DBA786"))
    cGradient.addColorAt(.85f*zScl, TColor.newHex("F2E9E9"))
    cGradient.addColorAt(1*zScl, TColor.WHITE)
    val cList = cGradient.calcGradient(); //call calcGradient to get a ColorList object that you can actually get intermediate colors from

    def getColor(z:Float) = //returns the color for the given height
      if(z > 0 && z < cList.size()) cList.get(z.asInstanceOf[Int])
      else if(z < 0) TColor.BLACK
      else TColor.WHITE

    val model = new OBJModel(app, "FirstTree.obj", "absolute", TRIANGLES);
    class Tree(val pos:Vec2, val size:Float = random(5, 20), val rot:Float = random(TWO_PI)) {

      def draw() {
        matrix {
          translate(to3D(pos))
          rotateX(-PI/2)
//          rotateZ(rot)
          scale(size)
          model.draw()
        }
      }
    }
    val trees = (0 until 15) flatMap {_ =>
      val center = Vec2.fromPolar(random(2500), random(TWO_PI));
      (0 until randi(12, 40)) map {_ => new Tree(center + Vec2.fromPolar(random(400), random(TWO_PI))) } }

    def draw() {
      val blockSize = 100f
      val drawNum = 20
      //draw the map around you
      noStroke(); fill(128);
      for(x <- (-drawNum to drawNum) map {_ * blockSize + Methods.roundToNearest(you.x, blockSize)};
          y <- (-drawNum to drawNum) map {_ * blockSize + Methods.roundToNearest(you.y, blockSize)}) matrix {
        val ptList = List((0f, 0f), (0f, blockSize), (blockSize, blockSize), (blockSize, 0f)) map { case (dx, dy) => (x+dx, y+dy, gridAt(Vec2(x+dx, y+dy))) }
        beginShape()
        ptList foreach {x => fill(getColor(x._3).toARGB); vertex(x)}
        endShape(CLOSE)
      }
      trees foreach (_.draw())
    }
  }

  object you extends Man(Vec2(), 0, 0)

  lazy val cam = new PeasyCam(this, 100);
  lazy val mkl = new zhang.MultiKeyListener()
  lazy val robot = new java.awt.Robot()
  val centerLocation = new java.awt.Point();
  val mouseLocation = new java.awt.Point();
  var isRecentering = false
  override def setup() {
    size(800, 600, P3D)
    cam
    addKeyListener(mkl)
    textFont(createFont("Arial", 100))

    /*import java.awt.event._
    addMouseMotionListener(new MouseAdapter() {
      override def mouseMoved(e:MouseEvent) {
        val frameloc = frame.getLocationOnScreen
        if(e.getXOnScreen == frameloc.x + width/2 && e.getY == frameloc.y + height/2) {
          //caused by robot; stop here
        }
        else {
          you.azimuth += Methods.wrap((e.getX - width/2) * .001f, TWO_PI)
          you.altitude += constrain((e.getY - height/2) * .001f, -PI/2, PI/2);
          robot.mouseMove(frameloc.x + width/2, frameloc.y + height/2)
        }
      }
    })*/

  }

  var camOn = false

  def cross[A, B](a:Seq[A], b:Seq[B]):Seq[(A, B)] = a flatMap { aElement => b.map{(aElement, _)} }


  override def draw() {
    background(255);

    if(!camOn) {
      val fp = you.facePos
      val look = fp + Vec3.fromSpherical(1, you.azimuth, you.altitude)
      camera(fp.x, fp.y, fp.z,
             look.x, look.y, look.z,
             0, 0, -1)
    }

    lights()
    pointLight(255, 255, 255, you.facePos)
    Methods.drawAxes(g, 2000)

    Landscape.draw()

    if(camOn) matrix {
      fill(255, 0, 0)
      translate(you.pos)
      cylinder(8, you.height, 2)

      fill(255, 255, 0)
      translate(0, 0, you.height)
      applyMatrix(P5Util.rotateAtoBMat(Vec3.X, you.otn))
//      cylinder(4, 5, 1)
      box(8, 4, 4)
      strokeWeight(3)
      stroke(180, 255, 99)
      line(0, 0, 10)
      strokeWeight(1)
    }


    import java.awt.event.KeyEvent._
    val angleMap = Map(VK_W -> 0f, VK_A -> -PI/2, VK_D -> PI/2, VK_S -> PI)
    angleMap.filter(x => mkl.isPressed(x._1)).values.map(Vec2.fromPolar(1, _)).foldLeft(Vec2())(_ + _).normalize match {
      case Vec2(0, 0) => ()
      case x => you.gpos += Vec2.fromPolar(you.speed, x.angle + you.azimuth)
    }

    def drawTexts() {
      textMode(MODEL)
      textSize(100); textAlign(CENTER, CENTER)
      def mkText(neg:Boolean, axis:Vec3, color:Int, name:String) {
        val pos = (axis * 100) * (if(neg) -1 else 1)
        val norm = (-axis).normalize
        fill(color)
        matrix {
          translate(pos)
          applyMatrix(P5Util.rotatePlaneAtoBMat(Vec3.X, Vec3.Z, axis cross Vec3.Z, norm))
          text((if(neg) "-" else "+")+name, 0, 0)
        }
      }
      cross(
        List(
          (Vec3.X, color(255, 0, 0), "X"),
          (Vec3.Y, color(0, 255, 0), "Y"),
          (Vec3.Z, color(0, 0, 255), "Z")),
        List(false, true)) foreach { case ((v, color, name), neg) => mkText(neg, v, color, name) }
    }
//    drawTexts()

//    stroke(0)
//    line(you.facePos, you.facePos + you.otn * 1000)

    resetMatrix();
    textSize(12)
    textAlign(LEFT, TOP); fill(0)
    text("camOn: "+camOn, 0, 0)


    pollSave() //check if the screen should be saved
  }


  override def mouseMoved(e:MouseEvent) {
    if(camOn) super.mouseMoved(e);
    else {
// ignore event if it's triggered from recentering
        if (isRecentering) {
            isRecentering = false;
        } else {
            // find relative mouse movement
            val dx = e.getX() - mouseLocation.x;
            val dy = e.getY() - mouseLocation.y;

            val thetaX = radians(dx) * .7f;
            val thetaY = radians(dy) * .7f;
//            println("dx: " + dx + " -> " + thetaX + ", dy: " + dy + " -> " + thetaY);

            // rotate view
//            cam.rotateX(thetaX);
//            cam.rotateY(thetaY);
          you.azimuth = (you.azimuth + thetaX) % TWO_PI;
          you.altitude = constrain(you.altitude - thetaY, -PI/2, PI/2)

            recenterMouse();
        }

        mouseLocation.x = e.getX();
        mouseLocation.y = e.getY();
    }
  }

  override def mouseDragged(e:MouseEvent) {
    if(camOn)
      super.mouseDragged(e)
    else
      mouseMoved(e)
  }

  override def keyPressed() {
    super.keyPressed()
//    if(key == ' ') {
//      camOn = !camOn;
//      cam.setActive(camOn)
//    }
  }

  def recenterMouse() {
    if (robot != null && this.isShowing()) {
        centerLocation.x = this.getWidth() / 2;
        centerLocation.y = this.getHeight() / 2;
        // convert centerLocation from frame's coordinate system to screen's coordinates.
        SwingUtilities.convertPointToScreen(centerLocation, this);

        isRecentering = true;
        robot.mouseMove(centerLocation.x, centerLocation.y);
    }
  }
}