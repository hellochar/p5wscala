package daily
package dec

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 12/2/11
 * Time: 11:29 AM
 */
import processing.core._
import org.zhang.lib.MyPApplet
import zhang.Methods
import peasy.PeasyCam
import org.zhang.geom.Vec3

class Dec02 extends MyPApplet with Savable {
  import PApplet._; import PConstants._;

  lazy val cam = new PeasyCam(this, 100);
  override def setup() {
    size(500, 500, P3D)
  }

  override def draw() {
    Methods.drawAxes(g, 100)
    
    pollSave() //check if the screen should be saved
  }

  trait Node
  trait Branch extends Node {
    def end:Vec3
    def path:Float => Vec3
    def children:Seq[Node]
  } //the start is assumed to be at the origin.


  override def keyPressed() {
    super.keyPressed(); //pressing space toggles a boolean variable to save the screen 
  }
}