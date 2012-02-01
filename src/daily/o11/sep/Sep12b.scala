package daily


/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 9/12/11
 * Time: 11:42 PM
 */
import processing.core._
import org.zhang.geom.Vec2
import org.zhang.lib.{HasMV, MyPApplet}
import collection.mutable.Buffer

class Sep12b extends MyPApplet with Savable with HasMV {
   import PConstants._;

  case class Vec2I(x:Int, y:Int) extends (Int, Int)(x, y)
  implicit def v2toInt(v:Vec2) = Vec2I(v.x.round, v.y.round)
  implicit def v2iToV2(v:Vec2I) = Vec2(v.x, v.y)

  sealed class State(val c:Int)
  case object None extends State(color(0))
  case object Block extends State(color(160))
  case object Wall extends State(color(180, 180, 0))

  lazy val BSIZE = width / 15
//  object grid extends BufferProxy[Buffer[State]] {
//    def self = Buffer.fill(width/BSIZE, height/BSIZE)(None:State)
//
//    def apply(x:Vec2I) = self(x.x)(x.y)
//    def update(x:Vec2I, value:State) = self(x.x)(x.y) = value;
//  }
  lazy val grid = Buffer.fill(width/BSIZE, height/BSIZE)(None:State)

  def drawGrid() {
    for(x <- grid.indices; y <- grid(x).indices) {
      stroke(240); fill(grid(x)(y).c);
      rect(x*BSIZE, y*BSIZE, BSIZE, BSIZE);
    }
  }

  def reset() {
    for (x <- grid.indices; y <- grid(x).indices) {
      grid(x)(y) = if((x + y) % 2 == 0) None else Block
    }
  }

  override def setup() {
    size(40*15, 40*15)
    reset
  }

  override def draw() {
    drawGrid()

//    println(frameRate)
//    pollSave("Sep12b-")
  }


  var clicked = (Vec2I(0, 0), 0);

  def toGrid(w:Vec2) = Vec2I(w.x.round/BSIZE, w.y.round/BSIZE);

  override def mousePressed() {
    clicked = (toGrid(mouseVec), mouseButton);
  }

  override def mouseReleased() {
    val here = toGrid(mouseVec)
    if(here == clicked._1) {
      val state = grid(here.x)(here.y)
      grid(here.x)(here.y) = {
        if(state != None) None
        else if(clicked._2 == LEFT) Block
        else Wall
      }
    }
    else {
      grid(here.x)(here.y) = grid(clicked._1.x)(clicked._1.y); //grid(clicked.x)(clicked.y)
      grid(clicked._1.x)(clicked._1.y) = None;
    }
  }
  
  override def keyPressed() {
    if(key == ' ')
      reset();
  }
}