package daily

import processing.core._
import org.zhang.lib.MyPApplet
import org.jbox2d.dynamics.World
import org.jbox2d.collision.AABB
import org.jbox2d.common.Vec2

class Feb16b extends MyPApplet with Savable {

  lazy val world = new World(new AABB(new Vec2(0, 0), new Vec2(width, height)), new Vec2(0, -9.8f), true);

  /**
   * Usage example: Create 5 boxes and a floor
   *
   */

  /**
   * Example: Create 100 circles at random positions and sizes, and a floor
   */

  /**
   * Example: Create 200 rects of various dimensions/positions, a floor with a hole in the center
   */

  import PApplet._;
  import PConstants._;

  override def setup() {
    size(500, 500)
  }

  override def draw() {


    pollSave() //check if the screen should be saved
  }

  override def keyPressed() {
    super.keyPressed(); //pressing space toggles a boolean variable to save the screen
  }
}