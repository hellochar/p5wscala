package daily.o12.feb

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 2/14/12
 * Time: 12:12 AM
 */

import processing.core._
import org.zhang.lib.MyPApplet
import daily.Savable
import org.zhang.geom.Vec2

class Feb14 extends MyPApplet with Savable {

  import PApplet._;
  import PConstants._;

  trait Shape {
    def pos:Vec2
    def size:Vec2

    def gridX = (center.x / gridSize).toInt
    def gridY = (center.y / gridSize).toInt

    def center = along(.5f)
    def along(f:Float) = pos + size*f

    def render()
  }

  case class Circle(pos:Vec2, size:Vec2) extends Shape {
    def render() {
      ellipse(center, size)
    }
  }
  case class Square(pos:Vec2, size:Vec2) extends Shape {
    def render() {
      rect(pos, size.x, size.y)
    }
  }

  var shapes:Set[Shape] = Set()

  val gridSize = 50;
  def gridWidth = width / gridSize
  def gridHeight = height / gridSize

  override def setup() {
    size(500, 500)
    smooth()
  }

  override def draw() {
    background(0);
    strokeWeight(1)
    stroke(160, 160, 160); fill(color(200, 200, 0));
    shapes foreach {_.render()}

    stroke(color(255, 64));
    for(x <- (0 until width by gridSize); y <- 0 until height by gridSize) {
      vert(x)
      horiz(x)
    }

    shapes.toSeq.groupBy(_.gridX) foreach { case (gridX, shapes) => {
      val sorted = shapes.sortBy(_.pos.y)
      val end = sorted.head

      val bottom = Vec2((gridX+1)* gridSize, height - (gridWidth - gridX) * 5)
      stroke(255, 160, gridX * 10); strokeWeight(4);
      line(Vec2(bottom.x, end.along(.75f).y), bottom);
      line(bottom, Vec2(0, bottom.y))

      sorted foreach {s =>
        line(s.along(.75f), Vec2(bottom.x, s.along(.75f).y))
      }
    }}
    shapes.toSeq.groupBy(_.gridY) foreach { case (gridY, shapes) => {
      val sorted = shapes.sortBy(_.pos.x)
      val end = sorted.last

      val left = Vec2(gridY * 5 + 5, gridY * gridSize)
      val botCorner = Vec2(left.x, (height - 5 * gridWidth) - (gridHeight - gridY) * 5)
      stroke(gridY * 10, 160, 255); strokeWeight(4);
      line(Vec2(end.along(.25f).x, left.y), left);
      line(left, botCorner)
      line(botCorner, Vec2(0, botCorner.y))

      sorted foreach {s =>
        line(s.along(.25f), Vec2(s.along(.25f).x, left.y))
      }
    }}

    pollSave() //check if the screen should be saved
  }

  override def mousePressed() {
    val gs = Vec2(gridSize, gridSize)
    def randomShape = List(Circle(_, _), Square(_, _))(randi(1))(mouseVec - gs/2, gs)
    shapes += randomShape
  }

  override def keyPressed() {
    super.keyPressed(); //pressing space toggles a boolean variable to save the screen
  }
}