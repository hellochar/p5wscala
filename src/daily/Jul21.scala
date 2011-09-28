package daily

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 7/21/11
 * Time: 2:07 AM
 */
import processing.core._
import org.zhang.lib.misc.Vec2

class Jul21 extends NameApplet {
  import PApplet._; import PConstants._;

  //Fractal landscape as per the one http://en.wikipedia.org/wiki/File:Animated_fractal_mountain.gif

  var V = Seq[Vertex]()

  override def setup() {
    size(800, 600)
    V +:= Vertex(0, 50)
    V +:= Vertex(100, 0)
    V +:= Vertex(100, 100)
    V +:= Vertex(200, 50)
  }

  case class Vertex(xx:Float, yy:Float, val edges:Iterable[Vertex] = Seq()) extends Vec2(xx, yy) {
    def draw() {
      if(edges.isEmpty) point(x, y)
      else {
        edges.foreach(line(this, _))
      }
    }
  }

  override def draw() {
    V foreach(_.draw)
  }
}