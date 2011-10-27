package daily

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 8/9/11
 * Time: 8:02 PM
 */
import processing.core._
import peasy.PeasyCam
import org.zhang.lib.misc.TravList
import org.zhang.lib.Tree

class Aug09 extends PApplet {
  import PApplet._; import PConstants._;

  object textures extends TravList(dataFile("").listFiles().map(_.getName).filter(_.startsWith("leaf-")).map(s => (s, loadImage(s))).toSeq)

  val t = new Tree(300, 150, 40)

  override def setup() {
    size(500, 500, P3D)
    new PeasyCam(this, 500)
    textures
  }

  override def draw() {
    background(255);
    noStroke();
    fill(128);
    rect(-100, -100, 200, 200)
    zhang.Methods.drawAxes(g, 100)

    t.leafTexture = textures.item._2
    t.draw(this)

    textMode(SCREEN)
    textAlign(LEFT, TOP)
    fill(0)
    text(textures.item._1, 0, 0)
  }

  override def keyPressed() {
    textures.move(key, 'q', 'e')
  }
}
object Aug09 {
  def main(a:Array[String]) {
    PApplet.main(Array("daily.Aug09"))
  }
}