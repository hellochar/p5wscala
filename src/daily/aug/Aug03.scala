package daily.aug

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 8/3/11
 * Time: 12:28 AM
 */

import processing.core._
import toxi.color.RGBDistanceProxy
import zhang.Methods
import org.zhang.geom.Vec2
import org.zhang.lib._
import controlP5.{ControlEvent, ControlListener, ControlP5}

class Aug03 extends PApplet {
  import PApplet._; import PConstants._; import CP5._;

  class Runner(var myColor:Int, var loc:Vec2 = Vec2(width/2, height/2)) {
    implicit def f2i(f:Float) = math.round(f)
    implicit def color2toxicolor(c:Int) = toxi.color.TColor.newARGB(c)

    var vel = Vec2.fromPolar(1, random(TWO_PI))
    def inBounds = zhang.Methods.isInWindow(loc.x, loc.y, width, height)

    private def compareFunc(a:Float, b:Float) = if(gt) a > b else a < b

    val distanceProxy = new toxi.color.RGBDistanceProxy()
    def canMove = if (!inBounds) false
                  else compareFunc(distanceProxy.distanceBetween(myColor, buffer.get(loc.x, loc.y)), distanceThreshold)

    def move() {
      val lastLoc = loc.clone
      while (canMove) loc += vel;
      if(!lastLoc.equals(loc)) { //if lastLoc isn't equal to the new location
        buffer.stroke(myColor);
        buffer.line(lastLoc.x, lastLoc.y, loc.x, loc.y)
      }

      vel = Vec2.fromPolar(1, random(TWO_PI));
      var counter = 0;
      while(!canMove) {
        loc += vel;
        if (!inBounds) loc = Vec2(random(width), random(height));
        if({counter += 1; counter} > 100) return; //prevent infinite looping
      }
    }
  }

  class Group(val runners:Runner*) {
    def draw() {
      for(i <- 0 until numRuns)
        runners.foreach(_.move)
    }
  }
  object BGroup extends Group(new Runner(color(0), Vec2(width / 2, height / 2)))
  object BWGroup extends Group(new Runner(color(0)), new Runner(color(255)))
  object HSBGroup extends Group((0 until 5).map(i => new Runner(color(i*50, 255, 255))):_*)

  object CP5 {
    lazy val cp5 = new ControlP5(Aug03.this)

    //a float slider for distance threshold
    //an int slider for posterize levels
    //an int slider for blur radius
    //an int slider for #moves per blur/posterize

    //a toggle for blur and posterize
    //a toggle for pausing

    private[this] val mkSlider = {
     var sNum = 0;
     val sWidth = width/2;
     val sHeight = 20;
     val sInset = 15;
     (name:String, low:Float, high:Float, init:Float) => {
       val s = cp5.addSlider(name, low, high, init, 0, sNum*(sHeight+sInset), sWidth, sHeight)
       sNum += 1
       s;
     } }

    val dtBar = mkSlider("Distance Threshold", 0, 1.41f, .8f)
    val posterizeBar = mkSlider("Posterize Levels", 2, 24, 6); posterizeBar.setNumberOfTickMarks((24-2)+1)
    val blurBar = mkSlider("Blur radius", 1, 8, 3)
    val mpbBar = mkSlider("# runs between B&P", 1, 100, 1);    mpbBar.setNumberOfTickMarks(((100-1)+1)/10)

    def mkToggle(name:String, x:Int, y:Int = height/3) = {
        val t = cp5.addToggle(name, x, y, 40, 25)
//      t.addListener(new ControlListener() { def controlEvent(theEvent: ControlEvent) = set(t.getState) })
        (t, () => t.getState(), t.setState(_:Boolean))
    }

//    var blur = true
//    var posterize = true

    private[this] val gtT = mkToggle("gt/lt (g)", width/2 + 100, 0); gtT._1.setMode(controlP5.ControlP5Constants.SWITCH); gtT._1.setState(true)
    def gt = gtT._2();         def gt_=(bool:Boolean) = gtT._3(bool)
    
    private[this] val b = mkToggle("Blur (b)", 0);        def blur = b._2();         def blur_=(bool:Boolean) = b._3(bool)
    private[this] val p = mkToggle("Posterize (p)", 60);   def posterize = p._2();    def posterize_=(bool:Boolean) = p._3(bool)
    private[this] val ` ` = mkToggle("Pause (space)", 120); def isPaused = ` `._2();   def isPaused_=(bool:Boolean) = ` `._3(bool)
    private[this] val resetBang = cp5.addBang("Reset (r)", 200, height/3, 25, 25); resetBang.addListener(new ControlListener() {
      def controlEvent(theEvent: ControlEvent) {
        reset()
      }
    })

    private[this] val nextGroup = cp5.addBang("Next Group (q)", 250, height/3, 25, 25); nextGroup.addListener(new ControlListener() {
      def controlEvent(theEvent: ControlEvent) {
        groups++;
      }
    })

    def distanceThreshold = dtBar.getValue//map(mouseX, 0, width, 0, 1.41f)
    def blurRadius = blurBar.getValue
    def posterizeLevels = posterizeBar.getValue
    def numRuns = mpbBar.getValue.toInt
  }

  object groups extends TravList(Seq(BGroup, BWGroup, HSBGroup))

  lazy val buffer = createGraphics(width, height, JAVA2D) //the image that the groups run on

  override def setup() {
    size(500, 500)
    colorMode(HSB)
    reset();
  }
  def reset() {
    buffer.beginDraw()
    buffer.background(128)
    buffer.endDraw()
  }

  override def draw() {
    if(!isPaused) { //isPaused is defined in TogglePauseOnKey
      buffer.beginDraw()
      groups.item.draw()

      if(blur) buffer.filter(BLUR, blurRadius);
      if(posterize) buffer.filter(POSTERIZE, posterizeLevels);
  //    filter(THRESHOLD)
  //    stop()
      buffer.endDraw()
    }
    image(buffer, 0, 0)
    textAlign(LEFT, BOTTOM); fill(color(255))
    text(groups.item.getClass.getSimpleName/*+
         "\nblur: "+blur+
         "\nposterize: "+posterize+
         "\npaused: "+isPaused+
         "\ndistanceThreshold: "+distanceThreshold*/, 0, height)
  }

  val actionMap:Map[Int, () => Any] = {
    import java.awt.event.KeyEvent._;
    Map(VK_B -> (() => blur = !blur),
        VK_P -> (() => posterize = !posterize),
        VK_SPACE -> (() => isPaused = !isPaused),
        VK_G -> (() => gt = !gt),
        VK_R -> (() => reset()),
        VK_Q -> (() => {reset(); groups++}))
  }

  override def keyPressed() {
    super.keyPressed()
    actionMap.find(keyCode == _._1) match {
      case Some((x, action)) => action()
      case None => {}
    }
  }

}