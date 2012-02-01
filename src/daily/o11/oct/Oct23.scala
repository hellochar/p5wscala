package daily


/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 10/23/11
 * Time: 4:09 PM
 */
import processing.core._
import org.zhang.lib.MyPApplet;
import org.zhang.geom._
import peasy.PeasyCam
import controlP5._

class Oct23 extends MyPApplet with Savable { app =>
  import PApplet._; import PConstants._;
  implicit def t2l(t:Thing) = t.loc

  def REPELPOWER = cp5.rp.getValue

  def ATTRACTPOWER = cp5.ap.getValue

  def SPHERERAD = cp5.rad.getValue

  def DRAG = cp5.drag.getValue

  def CLOSE = cp5.close.getValue

  def CONSTRAIN = cp5.constrain.booleanValue

  def DT = cp5.dt.getValue

  private var repelMode:RepelMode = CloseThreshhold

  def DRAWLINES = cp5.drawLines.booleanValue

  def DRAWAXES = cp5.drawAxes.booleanValue

  private abstract sealed class RepelMode(val desc:String) {
    override def toString = desc
    def candidates(t:Thing):Traversable[Thing]
  }
  private case object CloseThreshhold extends RepelMode("Within Closeness Threshhold") {
    def candidates(me:Thing) = things.filter(t => (t.loc - me.loc).mag < CLOSE && t != me)
  }
  private case object NearestNeighbor extends RepelMode("Nearest Neighbor") {
    def candidates(me:Thing) = Set(things.filter(me != ).minBy(x => (x.loc - me.loc).mag2))
  }
  private case object Everyone extends RepelMode("Everyone") {
    def candidates(t:Thing) = things.filter(t !=)
  }
  private case object NoOne extends RepelMode("No one") {
    def candidates(t:Thing) = Set()
  }
  private val repelModes = List(CloseThreshhold, NearestNeighbor, Everyone, NoOne)

  private class Thing(var loc:Vec3, var vel:Vec3) {
    private var repellers:Traversable[Thing] = _
    private var force = Vec3()

    def act() {
      repellers = repelMode.candidates(this);

      force = repellers.map(k => ((o:Vec3) => o.scale(REPELPOWER / o.mag2))(loc - k.loc)).foldLeft(Vec3())(_ + _)
      force += loc ofMag -ATTRACTPOWER
    }

    def update() {
      vel = vel * (1 - DRAG) + force * DT;
      loc = (loc + (vel * DT))
      if(CONSTRAIN) {
        loc = loc ofMag SPHERERAD
        /* Project the velocity onto the plane that is tangent to the sphere at your current location.
         * A vector is uniquely determined by its component lying in a plane and its component
         * orthogonal to that plane - v = t+o => t = v - o. We can get the orthogonal component by projecting
         * the vector onto our location vector, which is the normal of the plane.
         * So, vel = tangent = vel - orthogonal = vel - (vel projected onto loc)
         */
        vel -= vel proj loc
      }
      force = Vec3();
    }

    def draw() {
      val c = 255 - vel.mag * 5
      noStroke(); fill(255, c, c)
      translate(loc)
      sphere(3);
      translate(-loc)
      stroke(0);
      if(DRAWLINES)
        repellers.map(x => line(loc, x))
    }
  }

  private object cp5 extends ControlP5(this) {

    setAutoDraw(false)

    val (width, height) = (300, 340)
    val myWindow = addControlWindow("window", 100, 100, width, height)
//    myWindow.hideCoordinates()
    myWindow.setBackground(64)
    val rp = addSlider("Repel Power", -100, 100); rp.setValue(5); rp.linebreak();
    val ap = addSlider("Attract Power", -5, 5); ap.setValue(1); ap.linebreak();
    val rad = addSlider("Sphere Radius", 1, 1000); rad.setValue(100); rad.linebreak;
    val drag = addSlider("Drag", 0, .1f); drag.setValue(.02f); drag.linebreak;
    val close = addSlider("Closeness Threshhold", 1, 500); close.setValue(50); close.linebreak;
    val dt = addSlider("Simulation Speed", 0, 1f); dt.setValue(.1f); dt.linebreak;
    val constrain = addButton("Constrained"); constrain.setOff(); colorful(constrain);
    val drawLines = addButton("Draw Lines"); drawLines.setOn(); colorful(drawLines);
    val drawAxes = addButton("Draw Axes"); drawAxes.setOn(); drawAxes.linebreak; colorful(drawAxes);
    def colorful(b:Button) = b.addListener(new ControlListener() {
      def controlEvent(theEvent: ControlEvent) {
        if(b.booleanValue()) //this event gets called before the switch
          b.setColorBackground(0xff003652); //taken from CColor.java
        else
          b.setColorBackground(b.getColor.getActive);
      }
    })


    def onBang[A](b:Bang, evt: => A) {
     b.addListener(new ControlListener() {
       def controlEvent(theEvent: ControlEvent) {
         evt
       }
     })
   }

    def makeResetter(b:Bang, t: => Traversable[Thing]) = {
      onBang(b, things.synchronized(things = t))
      b;
    }
    val makeRand = makeResetter(addBang("Random", 10, height - 140, 20, 20), randomThings)
    val makeSphere = makeResetter(addBang("Octahedron Refinement", 90, height-140, 20, 20), refinedOctahedron)
    val makeRect = makeResetter(addBang("Equirectangular", 220, height-140, 20, 20), equirectangular);
//    val jitter = addBang("Jitter", 10, height - 140 + 40, 20, 20); onBang(jitter, things.synchronized { things.foreach(_.loc += Vec3.fromSpherical(5, random(TWO_PI), random(-PI/2, PI/2)))})

    getControllerList.collect{ case k: Controller => k }.foreach{k =>
      k.moveTo(myWindow);
      k.setMoveable(false);
      if(k.isInstanceOf[Slider]) k.asInstanceOf[Slider].setSliderMode(Slider.FLEXIBLE)
    }

      val repelMode = addListBox("Repel Mode", 10, height - 75, 150, 75);
    repelMode.moveTo(myWindow)
    repelMode.setItemHeight(12); repelMode.setBarHeight(12)

    repelMode.captionLabel().toUpperCase(true);
//    repelMode.captionLabel().set("something else");
    repelMode.captionLabel().style().marginTop = 3;
    repelMode.valueLabel().style().marginTop = 3; // the +/- sign
    repelModes.zipWithIndex foreach { case (x, i) => repelMode.addItem(x.desc, i) }
    repelMode.setMoveable(false);

    repelMode.addListener(new ControlListener() {
      def controlEvent(e: ControlEvent) {
        println(e)
      }
    })

  }

  //This method gets called by ControlP5 (using reflection) whenever an event comes in from repelMode
  def controlEvent(theEvent: ControlEvent) {
    if (theEvent.isGroup()) {
      // an event from a group e.g. scrollList
      repelMode = repelModes(theEvent.group().value().toInt)
    }
  }

  private def thingify(t:Traversable[Vec3]) = t.map(x => new Thing(x, Vec3()))

  private def randomThings: Traversable[Thing] = thingify(
    (0 until 100).map(_ => Vec3.fromSpherical(SPHERERAD, random(TWO_PI), random(-PI / 2, PI / 2)))
  )

  private def refinedOctahedron = thingify(Mesh.sphere(SPHERERAD, 3).points)

  private def equirectangular = thingify(
    (for(angle <- scala.Range.Double(0, TWO_PI, TWO_PI / 12); angleZ <- scala.Range.Double(-PI/2 + PI/12, PI/2+.001, PI / 12)) //there's some weird FP rounding errors which motivate the +.001
      yield Vec3.fromSpherical(SPHERERAD, angle.toFloat, angleZ.toFloat)) :+ Vec3(0, 0, SPHERERAD) :+ Vec3(0, 0, -SPHERERAD)
  )

  private var things:Traversable[Thing] = _

  lazy val cam = new PeasyCam(this, SPHERERAD * 3f)

  override def setup() {
    size(500, 500, P3D)
    sphereDetail(3)
    cam; //force
    cp5; //force
    things = randomThings
  }

  override def draw() {
//    println("frame "+frameCount+"!")
    background(64);
    if(DRAWAXES)
      zhang.Methods.drawAxes(g, 25)
    noStroke();
    things.synchronized {
      things foreach (_.act())
      things foreach (_.update())
      things foreach (_.draw())
    }
    fill(0);
//    sphere(SPHERESIZE - 2);
//    things foreach (println _)
    pollSave("Oct23-")
    println(frameRate+", "+CONSTRAIN)

    cp5.draw()
  }
  
  override def keyPressed() {
    super.keyPressed();
  }
}