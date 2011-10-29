import java.awt.event.{MouseWheelEvent, MouseWheelListener}
import javax.swing.SwingUtilities
import org.zhang.lib.HasMV
import org.zhang.geom.Vec2
import processing.core.PApplet._
import processing.core.PConstants._
import processing.core.{PImage, PGraphics, PVector, PApplet}

//in file daily/package.scala
package object daily {
  val FONTS_DIR = "C:\\Windows\\Fonts\\"
  def findFont(name:String):String = FONTS_DIR+name
//    List(name, name.toUpperCase, name.toLowerCase, name.capitalize).foldLeft(null)((ret, name) =>
//      if(ret != null) ret else try {
//        FONTS_DIR+name+".ttf"
//  }catch {
//    case e: RuntimeException if e.getMessage.contains("Rename the file or change your code.") => {}
//  })


  trait NameApplet extends PApplet with HasMV with Savable {
    import geomerative._

    RG.init(this)

    def addScrollWheelListener(name: RShape) {
      addMouseWheelListener(new MouseWheelListener() {
        def mouseWheelMoved(e: MouseWheelEvent) {
          SwingUtilities.invokeLater(new Runnable() {
            override def run() {
              val scale = pow(1.06f, e.getWheelRotation)
              name.transform(name.getX, name.getY, name.getWidth * scale, name.getHeight * scale)
            }
          });
        }
      })
    }

    def setRShapeCenter(name:RShape, x:Float, y:Float) = name.transform(x-name.getWidth/2, y-name.getHeight/2, name.getWidth, name.getHeight);

    def getNameShape(size:Int, fontName:String = "RAGE.TTF", alignment:Int = CENTER, name:String = "Xiaohan Zhang") = RG.getText(name, findFont(fontName), size, alignment)

    @deprecated("use MyPApplet instead")
    def line(v1:Vec2, v2:Vec2) { line(v1.x, v1.y, v2.x, v2.y) }
    @deprecated("Use MyPApplet instead")
    def ellipse(v1:Vec2, dims:Vec2) { ellipse(v1.x, v1.y, dims.x, dims.y) }

  }

  trait Savable extends PApplet {
    var saving = false;
    private[this] lazy val trulyOnline = try { //PApplet.online doesn't catch appletviewer.
      System.getProperty("user.dir")
      false
    } catch {
      case e: SecurityException => true
    }

    def pollSave(prefix:String = "") {
      if(saving && !trulyOnline) saveFrame(prefix+this.getClass.getSimpleName.filter('$'!=)+"-####.png")
    }

    abstract override def keyPressed() {
      super.keyPressed()
      if(key == ' ') saving = !saving;
    }
  }


}