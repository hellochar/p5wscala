package daily

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 7/6/11
 * Time: 11:12 PM
 */
import processing.core._
import org.zhang.lib.world.particle.Particle
import org.zhang.lib.misc.Vec2
import org.zhang.lib.world.particle.Particle
import org.zhang.lib.world.{TimedLife, BoundedWorld, HashWorld}
import org.zhang.lib.{WorldApplet, P5Util}
import xml.Elem
import java.io.PrintWriter

class Jul06 extends NameApplet with WorldApplet {
  import PApplet._; import PConstants._;

  var world = new HashWorld(this) with BoundedWorld

  object SVGOut extends PGraphics() {

    var doc:StringBuilder = _ //Holds the XML data

    def begin() { //Call this method right before/after you call beginRecord(SVGOut)
      doc = new StringBuilder("<svg width=\"100%\" height=\"100%\" version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">")
    }

    private def docAdd(e:Elem) = doc ++= appendStyle(e).toString //Add the element to the document

    override def line(p1: Float, p2: Float, p3: Float, p4: Float) = docAdd(<line x1={""+p1} y1={""+p2} x2={""+p3} y2={""+p4}/>)
    override def ellipse(p1: Float, p2: Float, p3: Float, p4: Float) = docAdd(<ellipse cx={""+p1} cy={""+p2} rx={""+p3} ry={""+p4}/>)

    //I've only implemented line and ellipse because those are the only two drawing methods I use in this sketch

    def lpad(s:String, c:Char, wantLen:Int) = makeStr(c, wantLen - s.length()) + s;
    def makeStr(c:Char, len:Int) {
        val b = new StringBuilder(len);
        for (i <- 0 until len) {
            b.append(c);
        }
        b.toString();
    }

    private def appendStyle(elem:Elem) = elem % styleAttributes //append the style attribute to the element.
    private def styleAttributes = {
      def strokeAttr = lpad(strokeColor.toHexString, '0', 8).drop(2) //drop 2 to get rid of alpha
      def strokeOpacityAttr = 1 //todo: map the alpha to go from 0 to 1
      def fillAttr = lpad(fillColor.toHexString, '0', 8).drop(2)
      def fillOpacityAttr = 1
      <dummy style={"stroke:#"+strokeAttr+";stroke-opacity:"+strokeOpacityAttr+";fill:#"+fillAttr+";fill-opacity:"+fillOpacityAttr}/>.attributes
      //I couldn't figure out how to create specific attribute objects so I just hacked it
    }

    def end = { //Call this method ribht before/after you call endRecord()
      doc ++= "</svg>";
      doc.toString
    }
  }

  override def setup() {
    size(500, 500)
    world ++= ((0 until 5) map(i => new MyParticle(null)))
    smooth()
  }

  override def mousePressed() {
    world += new MyParticle(null, 6000, 20, mouseVec)
  }

  def magThresh = map(mouseX, 0, width, 1, 100)

  var numAdded = 0;
  var numRemoved = 0;
  class MyParticle(parent:MyParticle = null,
                   val millisAlive:Float = 6000,
                   sizeInit:Float = 20,
                   locc:Vec2 = P5Util.randomVector(Jul06.this),
                   vell:Vec2 = Vec2.fromPolar(5, random(TWO_PI))) extends Particle(world, locc, vell) with TimedLife {
    numAdded += 1
    def size = map(lifeTime, 0, millisAlive, sizeInit, 0)

    var alive = true

    override def draw(p: PApplet) {
      p.noStroke(); p.fill(color(map(vel.mag, 0, magThresh, 0, 255), 0, 0))
      p.ellipse(x, y, size, size)
      p.stroke(0)
      if (parent != null && parent.alive) P5Util.line(Jul06.this, loc, parent.loc)
    }

    override def remove = {
      alive = false
      numRemoved += 1;
      super.remove
    }

    override def update() {
      super.update()
      if(vel.mag2 > magThresh*magThresh) {
        world += new MyParticle(this, millisAlive + random(-100, 100), sizeInit + random(-5, 5), loc, vel/2)
//        vel /= 2;
      }
      else vel += Vec2.fromPolar(2f, random(0, TWO_PI));
    }

    };

  var recordFrame = false
  override def keyPressed() {
    if(key == 'q')
      recordFrame = !recordFrame
  }

  val ps = System.getProperty("file.separator")
  val myDir = "jul06-out"+ps
  override def draw() {
    background(255)
    var shape:PShape = null
    if(recordFrame) {
      beginRecord(SVGOut)
      SVGOut.begin()
    }
    super.draw()
    pollSave(myDir)
    if(recordFrame) {
      endRecord()
      val out = SVGOut.end
      val stream = new PrintWriter(myDir+getClass.getSimpleName.dropRight(1)+"-"+frameCount+".svg")
      stream.write(out)
      stream.flush()
      stream.close()
    }
    noStroke();
    fill(100, 180, 60)
    rect(0, 0, numAdded * 5, 10)
    fill(210, 50, 79)
    rect(0, 10, numRemoved * 5, 10)
    numAdded = 0
    numRemoved = 0
  }
  
}