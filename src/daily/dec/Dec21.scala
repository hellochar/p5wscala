package daily
package dec

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 12/21/11
 * Time: 12:16 PM
 */

import processing.core._
import org.zhang.geom.Vec2
import org.zhang.lib.{P5Util, MyPApplet}

class Dec21 extends MyPApplet with Savable { app =>
  import PApplet._; import PConstants._;


  case class AnimalType(color:Int, id:Int) {
    override def toString = "AnimalType("+id+")";
  }
  class Ecosystem(numTypes:Int) {

    val animalTypes = {
      colorMode(HSB, numTypes, 255, 255);
      val t = (0 until numTypes) map { x => AnimalType(color(x, 255, 255), x)}
      colorMode(RGB, 255)
      t
    }
//    val animalTypes = {
//      colorMode(HSB, 3, 255, 255);
//      val t = (0 until 3) map { x => AnimalType(color(x, 255, 255), x)}
//      colorMode(RGB, 255)
//      t
//    }

    /**
     * Pred map is a mapping of [AnimalType t, set of types that t eats]
     */
//    val predMap = Map(animalTypes(0) -> Set(animalTypes(1)), animalTypes(1) -> Set(animalTypes(2)), animalTypes(2) -> Set(animalTypes(0)))
    val predMap = animalTypes.map{t:AnimalType =>
      val k = randomize(animalTypes.filter(t!=))           //aller types, in random order
      t -> k.take(randi(k.size)).toSet                      //map t -> a random number of types
    }.toMap
    def aEatsB(a:AnimalType, b:AnimalType):Boolean = predMap(a)(b)

    class Animal(var pos:Vec2, var angle:Float, val speed:Float, val tipe:AnimalType) {
      private var angleWant:Option[Float] = None
      var trail:Seq[Vec2] = Seq()
      def run() {
        angleWant = calculateAngle
      }

      override def toString = "Animal("+pos+", "+speed+", "+angle+", "+tipe+")";

      /**
       * Returns true iff this animal likes to eat the given animal
       */
      def eats(a:Animal) = aEatsB(tipe, a.tipe)

      def calculateAngle:Option[Float] = {
        //you have a set of angles S and you want to calculate a new angle A such that the distance from A to all of S is minimized.
        //The definition is intentionally left vague as to explore the whole space of distance minimizing algorithms

        //One definition of "minimized distance" is to calculate the distance from A to each angle S, sum those together, and then minimize.
        //How well might this choose an angle when there are only two members of S? Since distance is basically abs(A - B), there'd be two linear
        //functions but the sum would add up to the same value throughout the whole inner range between the two angles in S.

        //How to calculate the optimal angle?
        //Lets consider some use cases:
        //If S == (S1), optimal angle = S1
        //If S == (S1, S2), optimal angle = in the middle of S1 and S2

        //What is a good distance metric for angles?
        //What I really want is a "difference" operator for angles. Just as how you can subtract two vectors v1 from v2 and get
        //a vector V (with the property v1 + V = v2).
        //In fact even more than just a difference operator, I want to define a whole space of angular quantities and various operators
        //on this space (e.g. addition, subtraction, negation, scaling, and a distance metric)
        //We can imagines angles as 1 dimensional vectors - they have a real magnitude (0, TWO_PI) and a "direction" (either clockwise or counterclockwise).
        //Of course they can also be represented as one real number (e.g. 3, -2.4) but I have the feeling that a vector treatment might be better

        /**
         * An angle class - the magnitude is in (0, TWO_PI), and rhanded defines whether the angle lives in a righthanded (CCW) or lefthanded coordinate system (CW)
         */
  //      case class Angle private (mag:Float, rhanded:Boolean) {
  //
  //      }
  //      object Angle {
  //        def apply(num:Float) = Angle(abs(num) % TWO_PI, )
  //      }

        val vecs = (animals filter (this!=)) map { x => ((x.pos - this.pos) * (x match {
          case x if eats(x) && x.eats(this) => 0
          case x if eats(x) => 1
          case x if x.eats(this) => -1
          case _ => 0
        })).normalize / (x.pos distTo this.pos) }
        (vecs.fold(Vec2())(_ + _) / vecs.size) match {
          case Vec2(0, 0) => None
          case Vec2(x, y) if x.isNaN || y.isNaN => None
          case x => Some(x.angle)
        }
      }

      def update() {
        angleWant foreach { angle = _ }
        pos += Vec2.fromPolar(speed, angle)
        trail = (pos +: trail) take 300
      }

    }
    var animals = (0 until 25).map(_ => new Animal(middle + Vec2.random * random(width/2 - RADIUS), random(TWO_PI), 2, org.zhang.lib.random(animalTypes))).toSet

    def resolveHits() {
      //find all pairs of animals that are within 2*RADIUS where one eats the other, and add that to the list of animals to remove (cuz it got eaten)
//      val keepers = animals.toSeq.combinations(2).flatMap{x => Seq(x, x reverse)}.filter{x => x(0).eats(x(1))}.toSet
//      val toTrack = keepers.filter(x => (x(0).pos distTo x(1).pos) < 4*RADIUS).map{_(0)} //a set of Animals chasing others
//      val toRemove = keepers.filter(x => (x(0).pos distTo x(1).pos) < 4*RADIUS).map{_(1)}
//      bloods ++= toRemove map { x => (x.pos, x.tipe) }
//      bloods ++= animals map {x => (x.pos, x.tipe)}
//      bloods = bloods takeRight 4000
//      animals --= toRemove
    }

    def hitBoundaries() {
      animals filter {x => (x.pos - middle).mag > (width/2 - RADIUS) } foreach { x => x.pos = (x.pos - middle).ofMag(width/2 - RADIUS) + middle }
    }

    var bloods:Seq[(Vec2, AnimalType)] = Seq()

    def iterate() {
      animals foreach (_.run())
      animals foreach (_.update())
      resolveHits()
      hitBoundaries()
    }

  }

  var system:Ecosystem = _
  lazy val cam = new zhang.Camera(this)
  lazy val middle = Vec2(width / 2, height / 2)
  override def setup() {
    size(500, 500)
    cam
    reset()
    smooth()
  }

  def reset() {
    system = new Ecosystem(randi(3, 8))
    println(system.animalTypes)
    println(system.predMap)
  }

  /**
   *
   */
  val RADIUS = 5

  override def draw() {
//    println(animals)
//    println("------------------------------------")
//    colorMode(HSB)
//    background(0, 255, 127.5f)
//    colorMode(RGB)

//    background(255)
//    noFill(); stroke(0); rect(0, 0, width, height)

    background(0);
    noStroke(); fill(255); ellipse(middle, width, width) //yes width, width cuz width/2 is the radius of the circle
    def draw(a:Ecosystem#Animal) {
      pushMatrix()
      noStroke();
      fill(a.tipe.color, 255)
      translate(a.pos)
      rotate(a.angle)
      triangle(RADIUS*2f, 0, 0, RADIUS, 0, -RADIUS);
      ellipse(0, 0, 2*RADIUS, 2*RADIUS)
      popMatrix()
      stroke(a.tipe.color, 20); noFill();
      strokeWeight(5);
      beginShape()
      lines2(a.trail)
      endShape()
    }

    system.iterate()
    system.animals foreach (draw _)
//    noStroke();
//    system.bloods foreach {x => fill(x._2.color, 20); ellipse(x._1, 5, 5) }

    pollSave() //check if the screen should be saved
  }

  override def keyPressed() {
    super.keyPressed(); //pressing space toggles a boolean variable to save the screen
    if(key == 'r') reset();
  }


  def randomize[A](x:Seq[A]) = {
    var k = x;
    def pickRandom() = {val got = org.zhang.lib.random(k); k = k.filter(got!=); got }
    var newList = Seq[A]();
    while(!k.isEmpty) {
      newList :+= pickRandom()
    }
    newList
  }

}