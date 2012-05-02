package daily

import processing.core._
import peasy.PeasyCam
import org.zhang.geom.{Vec2, Vec3}
import org.zhang.lib.{P5Util, MyPApplet}

class Mar12 extends MyPApplet with Savable {

  import PApplet._;
  import PConstants._;

  val clife = new ConwayLife(25, 25);
  clife.randomize()
  val hist = new CAHistory(clife)

  val cubeSize = 10

  var camPos = Vec3()
  var camAzim = 0f
  var camAlt = 0f

  lazy val mkl = {
    val mkl = new zhang.MultiKeyListener();
    addKeyListener(mkl)
    mkl
  }

  def handleMove() {
    def down(keyCode:Int) = mkl.isPressed(keyCode)

    def toWorld(v:Vec3) = {
//      pushMatrix()
//      resetMatrix();
//      camera();
//      doCamera()

//      val camMat = getMatrix(null:PMatrix3D) //we actually want the normal transformation; not the worldview transformation
//      camMat.invert()
//      camMat.transpose();
//      val t = P5Util.transformed(v, camMat)

      val t = Vec3(modelX(v.x, v.y, v.z), modelY(v.x, v.y, v.z), modelZ(v.x, v.y, v.z))
//      popMatrix()
      t

//      v //nope just kidding, we're not returning t
    }
    import java.awt.event.KeyEvent.{VK_W, VK_A, VK_S, VK_D, VK_Q, VK_Z}
    val map = Map(VK_W -> Vec3(0, -1, 0),
        VK_S -> Vec3(0, 1, 0),
        VK_D -> Vec3(-1, 0, 0),
        VK_A -> Vec3(1, 0, 0),
        VK_Q -> Vec3(0, 0, 1),
        VK_Z -> Vec3(0, 0, -1))
    camPos += map.filter(x => down(x._1)).map{x => toWorld(x._2)}.foldLeft(Vec3())(_ + _)
  }

  def handleLook() {
//    val (dx, dy) = (mouseX - pmouseX, mouseY - pmouseY)
//    camAzim = zhang.Methods.wrap(camAzim + radians(dx), TWO_PI)
//    camAlt = constrain(camAzim + radians(dy), -PI/2, PI/2)
    camAzim = map(mouseX, 0, width, -PI, PI)
    camAlt =  map(mouseY, 0, height, PI/2, -PI/2)
  }

  override def setup() {
    size(500, 500, P3D)
    (0 until 50) foreach {_ => hist.step()}
  }

  def doCamera() {
    val l = Vec3.fromSpherical(1, camAzim, camAlt)

    camera(camPos.x, camPos.y, camPos.z,
      camPos.x + l.x, camPos.y + l.y, camPos.z + l.z,
      0, 0, -1)
  }

  override def draw() {
    println(frameRate);
    noStroke();
    def cube(x:Int, y:Int, z:Int) {
      at(x*cubeSize, y*cubeSize, z*cubeSize) {
        box(cubeSize)
      }
    }


    doCamera()
    handleMove()
    handleLook()

    background(255);
//    fill(128);
//    rect(Vec2(-1e4f), 2e4f, 2e4f)

    ambientLight(64, 64, 64);
    directionalLight(128, 128, 128, Vec3.fromSpherical(1, sin(millis()/3000f), cos(millis()/4600f)));
//    pointLight(128, 128, 128, camPos)

    fill(255);
    for((grid, z) <- hist.grids.zipWithIndex) {
      for(x <- 0 until grid.length; y <- 0 until grid(x).length) {
        if(grid(x)(y)) cube(x, y, z)
      }
    }
    pollSave() //check if the screen should be saved
  }

  override def keyPressed() {
    super.keyPressed(); //pressing space toggles a boolean variable to save the screen
  }
}
abstract class CA2D(val width:Int, val height:Int) {
  type State = Boolean
  private var grid = Array.fill(width, height)(false)

  import zhang.Methods.wrap
  def get(x:Int, y:Int):State = grid(wrap(x, width).toInt)(wrap(y, height).toInt)

  def getGrid:Array[Array[State]] = grid map {_.clone()}

  def step() {
    val newGrid = getGrid
    for(x <- 0 until width; y <- 0 until height) {
      def numAdj = (for(i <- x-1 to x+1; j <- y-1 to y+1; if !(i == x && j == y)) yield get(i, j)).filter(x => x).length
      newGrid(x)(y) = nextState(grid(x)(y), numAdj)
    }
    grid = newGrid
  }

  def randomize() {
    for(x <- 0 until width; y <- 0 until height) {
      grid(x)(y) = randomState()
    }
  }

  override def toString = {
    (0 until height).map(y => (0 until width).map{x => if(grid(x)(y)) '*' else ' '}.mkString).mkString("\n")
  }

  protected def randomState():State
  protected def nextState(state:State, adj:Int):State
}
class ConwayLife(w:Int, h:Int) extends CA2D(w, h) {
  override type State = Boolean

  def nextState(state:Boolean, adj:Int) = state match {
    case true => adj == 2 || adj == 3
    case false => adj == 3
  }
  def randomState() = if(math.random < .5) true else false
}

class CAHistory(val ca2d:CA2D) {
  type Grid = Array[Array[ca2d.State]]
  var grids:Seq[Grid] = Seq()
  add(ca2d.getGrid)

  private def add(g:Grid) {
    grids :+= g
  }

  def step() {
    ca2d.step()
    add(ca2d.getGrid)
  }

}