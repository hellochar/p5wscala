//package daily
//
//import processing.core._
//import org.zhang.lib.{WorldApplet, MyPApplet}
//import org.zhang.geom.Vec2
//import org.zhang.lib.world.{Location, Entity, Velocity, HashWorld}
//
//class April16 extends MyPApplet with Savable with WorldApplet {
//
//
//
//  import PApplet._;
//  import PConstants._;
//
//  //we have two creatures A and B; we have a rock-paper-scissors mechanic in there and a mutation
//  //that changes whether the creature is rock, paper, or scissors
//
//  val world = new HashWorld(this, .1f)
//
//  sealed abstract class Type(val color:Int)
//  case object A extends Type(color(190, 0, 0))
//  case object B extends Type(color(0, 190, 0))
//
//  class Critter(var loc:Vec2, val t:Type, val size:Float, var food:Float, var vel:Vec2 = Vec2()) extends Velocity {
//    val sightDist = 200;
//
//    override def run() {
//      super.run();
//      food = food - 1;
//      if(food < 0) {
//        remove();
//      }
//      val nearbyFood = world.inCircle(loc, sightDist, world.ofType(classOf[Food]))
//      vel = (nearbyFood.minBy(_.loc distTo this.loc).loc - loc) ofMag 5;
//    }
//
//    override def update() {
//      super.update();
//    }
//
//    override def draw(p: PApplet) {
//      matrix {
//        translate(loc)
//        rotate(angle)
//        scale(size);
//        strokeWeight(1/size);
//
//        stroke(0);
//        fill(t.color)
//        ellipse(0, 0, 1, 1);
//
//        stroke(255);
//        line(0, 0, 1.1f, 0);
//      }
//    }
//  }
//
//  class Food(val loc:Vec2, val size:Float, val food:Float) extends Location {
//
//  }
//
//  override def setup() {
//    size(500, 500)
//  }
//
//  override def draw() {
//
//
//    pollSave() //check if the screen should be saved
//  }
//
//  override def keyPressed() {
//    super.keyPressed(); //pressing space toggles a boolean variable to save the screen
//  }
//}