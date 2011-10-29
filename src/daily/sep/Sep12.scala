package daily
package sep

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 9/12/11
 * Time: 10:16 PM
 */
import processing.core._
import org.zhang.lib.MyPApplet
import controlP5.ControlP5
import org.zhang.parse.float.{FloatParser, VariableFloatParser}

class Sep12 extends MyPApplet with Savable { app =>
  import PApplet._; import PConstants._;
  import CP5._

  object CP5 {
    val cp5 = new ControlP5(app); cp5.setAutoDraw(false)
    private val inputControl = cp5.addTextfield("Function", 0, 0, 200, 20);

    def inputString = inputControl.getText
  }

  object cam extends zhang.Camera(this) {
    override def uiHandle() {
      if(!cp5HasFocus(cp5)) super.uiHandle();
    }
  }

  override def setup() {
    size(500, 500)
    cam.setCenter(0, 0);
    cam.setViewportWidth(5);
    frameRate(30);
  }

  object Parser {
    val vars: collection.mutable.Map[String, Float] = collection.mutable.Map("x" -> 0)
    val mp = new FloatParser(vars);

    val (memFunc, memCache) = org.zhang.lib.memoize(evalStr _)

    private def evalStr(args: (Float, String)) = {
      vars("x") = args._1; mp.parse(args._2);
    }

    def eval(x:Float) = {
      if(memCache.size > 1024) memCache.clear() //keep it from eating too much memory
      memFunc(x, inputString)
    }
  }

  override def draw() {
    strokeWeight(0);
    background(188); stroke(64, 64, 240); noFill()
    
    beginShape
    val rect = cam.getRect
    for(x <- Range.Double(rect.x, rect.x + rect.width, rect.width/width)) {
      Parser.eval(x.toFloat) match {
        case Right(y) => vertex(x.toFloat, -y) //give the illusion of +y going up by rendering the point negatively.
        case _ => Unit
        //case Left(s) => println("Fail: "+s)
      }
    }
    endShape

    stroke(0);
    line(rect.x, 0, rect.x+rect.width, 0); //draw horizontal line
    line(0, rect.y, 0, rect.y+rect.height); //draw vertical line

    {
      val (modelMin, modelMax) = (cam.model(cam.getCorner).x, cam.model(new PVector(width, height)).x)
      val (min, max) = (modelMin.floor.toInt, modelMax.ceil.toInt);
      for(x <- min until max) {
        line(x, -.1f, x, .1f)
      }
    }

    {
      val (modelMin, modelMax) = (cam.model(cam.getCorner).y, cam.model(new PVector(width, height)).y) //since y downwards is positive, the minimum y is at the topleft corner.
      val (min, max) = (modelMin.floor.toInt, modelMax.ceil.toInt);
      for(y <- min until max) {
        line(-.1f, y, .1f, y)
      }
    }

    resetMatrix();
    cp5.draw()

    println(frameRate)
    pollSave("")
  }
  
  
  override def keyPressed() {
    if(!cp5HasFocus(cp5))
      super.keyPressed();
  }
}