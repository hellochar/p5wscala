package daily


/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 12/2/11
 * Time: 11:29 AM
 */
import processing.core._
import zhang.Methods
import peasy.PeasyCam
import org.zhang.geom.Vec3
import org.zhang.lib.{P5Util, MyPApplet}

class Dec02 extends MyPApplet with Savable with SphereUtils {
   import PConstants._;

  lazy val cam = new PeasyCam(this, 100);
  override def setup() {
    size(500, 500, P3D)
  }

  override def draw() {
    Methods.drawAxes(g, 100)
    
    pollSave() //check if the screen should be saved
  }

  trait Node {
    def draw()
  }

  /**
   * A tree is either a trunk or a branch; it's one segment of a tree structure
   */
  abstract class Tree(val path:Float => Vec3, val branches:Seq[Branch]) extends Node {
    /**
     * The endpoint of this tree.
     */
    def end:Vec3 = path(1)

//    /**
//     * The path of this tree's trunk.
//     */
//    def path:Float => Vec3

//    /**
//     * A set of nodes that sprout immediately off this tree.
//     */
//    def children:Seq[Node]

    /**
     * Transform from this Tree's coordinate system to a system aligned with the path at amt.
     */
    def transformToPathMat(amt:Float) = {
      val m3d = new PMatrix3D();
      val p = path(amt)
      val epsilon = 1e-5f
      m3d.translate(p.x, p.y, p.z)
      m3d.apply(P5Util.rotateAtoBMat(Vec3.Z, (path(amt + epsilon) - path(amt)).normalize))
      m3d;
    }

    def draw() {
      //sample the path and just draw a line for now
      lines3((0 until 10).map(x => path(x/10f)))

      //now draw all of the children. Children could be other branches, or leaves, or flowers, etc.
      //We know that all children of a Tree have additional data associated with them - specifically,
      //how far along down the path the child is.
      branches foreach {c => pushMatrix(); applyMatrix(transformToPathMat(c.along)); c.draw(); popMatrix()}
    }
  }

  class Trunk(path:Float => Vec3, branches:Seq[Branch]) extends Tree(path, branches) {
  }

  class Branch(val along:Float, val parent:Tree, path:Float => Vec3, branches:Seq[Branch]) extends Tree(path, branches) {
  }


  override def keyPressed() {
    super.keyPressed(); //pressing space toggles a boolean variable to save the screen 
  }
}