package daily
package dec

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 12/1/11
 * Time: 10:34 PM
 */
import processing.core._
import org.zhang.lib.MyPApplet
import org.zhang.geom.Vec2
import traer.physics.{Particle, ParticleSystem}

class Dec01 extends MyPApplet with Savable {
  import PApplet._; import PConstants._;
  implicit def p2v2(p:Particle) = Vec2(p.position().x(), p.position().y())
  implicit def zhang2pv(z:Vec2) = new PVector(z.x, z.y, 0)
  implicit def pv2zhang(p:PVector) = Vec2(p.x, p.y)

  /*
   * We have a set of states. Each state S has a function that maps any given state X into a real number from
   * 0 to 1 denoting the probability of going from S to X. We calculate this function for every state by pushing a set of
   * training chains through it. A chain is simply an ordered list of states.
   *
   * For example, let us consider creating random English words. A chain will then be a whole word, and a state will be
   * an individual letter (or the None state).
   */

  abstract class State {
    var numHitsRaw = collection.mutable.Map[State, Int]() //A map of state -> the number of times this state goes to it
    def probabilities = numHitsRaw map { case (s, i) => (s, i.toFloat / numHitsRaw.values.sum)}

    var numHitsIncoming = collection.mutable.Map[State, Int]() //A map of state -> the number of times that state goes to me
    def probabilitiesInc = numHitsIncoming map { case (s, i) => (s, i.toFloat / numHitsIncoming.values.sum)}

    def train(to:State) = {
      numHitsRaw.put(to, numHitsRaw.getOrElse(to, 0) + 1)
      to.numHitsIncoming.put(this, to.numHitsIncoming.getOrElse(this, 0) + 1)
    }


    def choose = {
      val rand = random(1)
      (org.zhang.lib.partials(probabilities.toStream) map { s =>
        (s.last._1, s.map(_._2).sum)
      }).dropWhile(_._2 < rand).head._1
    }
  }

  abstract class MarkovChain[Cons] {
    case object NoState extends State {
      override def toString = nsString
    }
    abstract class MyState(val cons:Cons) extends State
    def nsString:String

    /**
     * The MyState that's returned should have a cons == c
     */
    protected def cons(c:Cons):MyState
    private var map = collection.mutable.Map[Cons, MyState]()
    def getState(c:Cons) = map.getOrElseUpdate(c, cons(c))
    def allStates = map.values ++ List(NoState)

    def train(from:State, to:State) {
      from.train(to)
    }

    def train(t:Seq[Cons]) {
      (NoState +: (t map (getState _)) :+ NoState).sliding(2) foreach (x => train(x(0), x(1)))
    }

    def generate() = {
      val iter = Stream.iterate(NoState:State)(_.choose).tail //.tail to remove the first NoState.
      iter.takeWhile(_ != NoState).collect{ case l:MyState => l.cons }
    }

  }

  class StringMarkovChain extends MarkovChain[Char] {
    class Letter(val char:Char) extends MyState(char) {
      override def toString = char.toUpper.toString
    }

    def cons(c:Char) = new Letter(c)

    def nsString = "Space"
  }

  val chain = new StringMarkovChain()
  lazy val states = chain.allStates
  lazy val particles:collection.mutable.Map[State, Particle] = collection.mutable.Map(states.map(_ -> ps.makeParticle(1, random(100, width-100), random(100, height-100), 0)).toSeq:_*)
  lazy val cam = new zhang.Camera(this)

  val ps = new ParticleSystem(0, .03f);

  val RAD = 20

  override def setup() {
    size(800, 600)
//    smooth()
    cam;

    println(dataPath("wlist_match10.txt"));
    {
      var i = 0;
//      loadStrings("wlist_match10.txt") foreach (s => {chain.train(s); i += 1; println("Trained "+i+"("+s+")")})
      loadStrings("wlist_match10.txt") foreach (chain.train(_))
    }

    word = generateRandomWord()
    particles foreach {
      case (state, particle) => state.probabilities foreach {
        case (s, pow) if s != state => ps.makeSpring(particle, particles(s), pow, .1f, 2*RAD + 15 / pow)
        case _ => ()
      }
    }
  }

  def generateRandomWord() = {
    chain.generate().mkString
  }

  def arrow(start:Vec2, end:Vec2) {
    line(start, end)
    pushMatrix();
    translate(end);
    rotate(atan2(start.x-end.x, end.y-start.y));
    line(0, 0, -10, -10);
    line(0, 0, 10, -10);
    popMatrix();
  }

  var word:String = _

  override def draw() {
    def hueFor(from:State, to:State) = 255 * ((PI + (p2v2(particles(from)) angleTo particles(to))) / TWO_PI)
    background(0);

    ps.tick(.05f)

    colorMode(HSB)
    def drawConnections(s:State, bright:Float) {
      s.probabilities foreach { //draw connections
            case (`s`, f) => { //self
              noFill(); stroke(bright); strokeWeight(f * 25)
              ellipse(Vec2(-RAD) + particles(s), 2*RAD, 2*RAD);
            }
            case (t, f) => { strokeWeight(f * 25)
      //        line(particles(s), particles(t))
              val start = p2v2(particles(s));
              val end = p2v2(particles(t));
              stroke(hueFor(s, t), 200, bright);
              arrow(start, start + ((end - start) ofMag ((start distTo end) - RAD)))
            }
          }
    }
    selected match {
      case Some(sel) => { states.filter(sel!=) foreach {drawConnections(_, 64)}; drawConnections(sel, 255); }
      case None => states foreach {drawConnections(_, 128)}
    }

    strokeWeight(1);
    textAlign(CENTER, CENTER); textSize(16);
    states foreach {s => //draw nodes
      def brightFor(s:State) = selected.map(_.probabilities.keysIterator.contains(s)).map(if(_) 255 else 64).getOrElse(128)
      stroke(255); fill(30 / 360f * 255, 255, brightFor(s));
      ellipse(particles(s), 2*RAD, 2*RAD);

      fill(255);
      text(s.toString, particles(s).x, particles(s).y);
    } //draw nodes

    dragged foreach {particles(_).position().set(cam.model(mouseVec).x, cam.model(mouseVec).y, 0)} //move selected

    colorMode(RGB);
    selected foreach { s => //draw probabilities for selected/moused over
      noFill(); stroke(255); ellipse(particles(s), 2*RAD + 10, 2*RAD + 10)
      resetMatrix(); textSize(12);
      textAlign(LEFT, TOP)
      text(s.toString+" goes to:", 0, 0);
      def dispVals(probs:collection.Map[State, Float], right:Boolean) {
        probs.toList.sortBy{-_._2}.zipWithIndex foreach { case ((s, f), idx) =>
          val y = (textAscent + textDescent + 2) * (idx + 1)
          noStroke(); fill(255, 160, 20);
          rect(if(right) width-(250*f) else 0, y+1, 250 * f, (textAscent + textDescent))
          fill(255);
          text(s+", "+nf(f, 0, 4), if(right) width else 0, y)
        }
      }
      dispVals(s.probabilities, false)

      textAlign(RIGHT, TOP)
      text(s.toString+" comes from:", width, 0)
      dispVals(s.probabilitiesInc, true)
    } //draw selected stats

    textAlign(CENTER, TOP);
    resetMatrix()
    textSize(24); fill(255);
    text(word, width/2, 0)

//    pollSave() //check if the screen should be saved
  }

  var dragged:Option[State] = None
  def selected = dragged.orElse(mousedOver)

  def mousedOver = states.find(x => (p2v2(particles(x)) distTo cam.model(mouseVec)) < RAD)

  override def mousePressed() {
    dragged = mousedOver
  }

  override def mouseReleased() {
    dragged = None
  }

  override def keyPressed() {
    if(key == ' ')
      word = generateRandomWord()
  }
}