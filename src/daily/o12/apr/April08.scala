package daily

import processing.core._
import org.zhang.geom.Vec2
import org.zhang.lib.{P5Util, MyPApplet}
import scala.Double

class April08 extends MyPApplet with Savable { app =>

  import PApplet._;
  import PConstants._;

  def weightedRandom[T](m:Map[T, Float]):T = {
    var rand = random(m.values.sum)
    for((t, weight) <- m) if(rand < weight) return t else {rand -= weight }
    return null.asInstanceOf[T]
  }

  trait Instr {
    def apply(ant:Ant)
  }
  object Forward extends Instr {
    def apply(ant:Ant) {
      ant.loc += ant.face
    }
  }
  object Left extends Instr {
    def apply(ant:Ant) {
      ant.face = ant.face.rotate(-PI/2);
    }
  }
  object Right extends Instr {
    def apply(ant:Ant) {
      ant.face = ant.face.rotate(PI/2);
    }
  }
  object Reverse extends Instr {
    def apply(a:Ant) {
      a.face *= -1;
    }
  }
  object GiveBirth extends Instr {
    def apply(ant:Ant) {
      if(ant.food > 10) {
        ants += new Ant(ant);
        ant.food /= 2;
      }
    }
  }
  object Eat extends Instr {
    def apply(ant:Ant) {
      ant.food += 15;
    }
  }



  def randomInstr = weightedRandom(Map(
    Forward -> 1f,
    Left -> 1f,
    Right -> 1f,
    Reverse -> 1f,
    Eat -> .25f,
    GiveBirth -> .1f))

  def mutate(instrs:Seq[Instr]) = {
    instrs flatMap { i => randi(0, 8) match {
      case 0 | 1 | 2 | 3 | 4 => Seq(i)
      case 5 => Seq(i, randomInstr)
      case 6 => Seq(randomInstr, i)
      case 7 => Seq(i, randomInstr, randomInstr, randomInstr, randomInstr)
      case 8 => Seq()
    }}

//    if(nI.head == GiveBirth) nI.tail else nI
  }

  def inRadius(center:Vec2, rad:Float) = ants filter {x => (x.loc distTo center) < rad}

  class Ant(var loc:Vec2, var face:Vec2, val instrs:Seq[Instr], var food:Int) {
    var iStream = Stream.continually(instrs).flatMap{x => x}.drop(randi(0, instrs.length))

    def this(loc:Vec2, angle:Float, instrs:Seq[Instr], food:Int) = this(loc, Vec2.fromPolar(1, angle), instrs, food)
    def this(loc:Vec2, angle:Float, food:Int) = this(loc, angle, (0 until 10) map {_ => randomInstr}, food)
    def this() = this(Vec2(randi(-10, 10), randi(-10, 10)), randi(0, 3) * PI/2, randi(25, 125))
    def this(parent:Ant) = this(parent.loc + parent.face, parent.face, mutate(parent.instrs), parent.food / 2)
//    var variables:Map[String, Int] = Map()

    def run() {
      food -= (1 + ((inRadius(loc, 1.5f).size - 1) / 2.5f).toInt);
      val instr = iStream.head
      iStream = iStream.tail

      instr(this)
      if(food <= 0) {
        ants -= this
      }
    }

    def draw() {
      matrix {
        translate(loc);
        rotate(face.angle)

        //draw ant in local coordinate system
        strokeWeight(.05f); noStroke(); fill(2 * food, 0, 0)//fill(64, 30, 30)
//        ellipse(Vec2(), 2, 1);
        ellipse(Vec2(), 1, 1);
        stroke(0);
        line(Vec2(1, 0))
        implicit def d2f(d:Double) = d.toFloat

        stroke(0);
        def seg(a:Vec2, b:Vec2, c:Vec2) {lines2(List(a,b,c), false); lines2(List(a,b,c).map{x => x.copy(y = -x.y)}, false)}
//        seg(Vec2(.6, .4), Vec2(.7, .5), Vec2(.85, .4))
//        seg(Vec2(.2, .5), Vec2(.3, .55), Vec2(.5, .6))
//        seg(Vec2(-.2, .2), Vec2(-.3, .4), Vec2(-.5, .35))
//
//        seg(Vec2(.95, .1), Vec2(.96, .12), Vec2(.98, .11))
      }
    }
  }

  import collection.mutable.{SetProxy, Set}
  object ants extends SetProxy[Ant] {
    val self = Set[Ant]()
    val grid = Array.fill[Option[Ant]](100, 100)(None);

    override def +=(elem: Ant) = {
      super.+=(elem)
      this
    }

//    override def -=(elem: Ant) = {
//
//    }
  }
  lazy val cam = new zhang.Camera(this)
  override def setup() {
    size(500, 500)
    ants ++= (0 until 4) map {_ => new Ant()}

    cam.setCenter(0, 0)
    cam.setViewportWidth(20);
  }

  override def draw() {
    background(255);
    if(frameCount % 5 == 0) {
      ants foreach {_.run()}
    }
    ants foreach {_.draw()}

    strokeWeight(.02f);
    stroke(128, 128);
    (P5Util.drawGraphAxes(this, cam, 1, 999));
    pollSave() //check if the screen should be saved
  }

  override def keyPressed() {
    super.keyPressed(); //pressing space toggles a boolean variable to save the screen
  }
}