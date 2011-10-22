package daily

import java.applet.Applet
import processing.core.{PConstants, PApplet}
import org.zhang.lib.misc.Vec2
import org.zhang.lib.{P5Util, MyPApplet}
import controlP5._
import parse.expr._
import java.awt.event.{MouseWheelEvent, MouseWheelListener}
import javax.swing.{BorderFactory, JPanel}
import javax.swing.border.BevelBorder
import java.awt.{Dimension, GridLayout}

//Processing's positive y-axis is downwards so we need to negate certain things to make it look like we're
// going up instead of down. Those that need to be negated are marked with a //negate
class Sep25 extends Applet {
  import PApplet._, PConstants._;
  import CP5.{ampExpr, freqExpr, phaseExpr}

  var varMap = collection.mutable.Map("n" -> 0f)
  val Parser = new ExprParser(varMap)

  private var timeMillis = 0f;
  def time = timeMillis / 1000f;

  def cosColor = 0xFFFF9A00 //orange
  def sinColor = 0xFFFF0000 //red
  def vecColor = 0xFF0D56A6 //blueish

  def bgColor = CP5.color(253);


  def darken(col:Int) = { import CP5._; color(red(col)*.8f, green(col)*.8f, blue(col)*.8f); }
  def brighten(col:Int) = { import CP5._; color(red(col)/.8f, green(col)/.8f, blue(col)/.8f); }

  def panelize(x:PApplet) = {
    val k = new JPanel();
    k.add(x);
    k.setBorder(BorderFactory.createBevelBorder(BevelBorder.RAISED))
    k;
  }

  lazy val apps = List(CP5, CosGraph, SinGraph, Argand)

  override def init() {
    setLayout(new GridLayout(2, 2));
    CP5.init();
    Argand.init(); //argand must be initted before others because the other two might otherwise force the cam instance before it should be forced.
    CosGraph.init();
    SinGraph.init();
    apps.foreach(x => add(panelize(x)))
  }


  override def start() { apps.foreach(_.start()) }

  override def stop() { apps.foreach(_.stop()) }

  override def destroy() { apps.foreach(_.destroy()) }

  def phasor(amp: Float, theta: Float) = Vec2.fromPolar(amp, theta);

  /*
    a) make phasors only call eval on some already parsed Expr; we will need to have three Expr vars for amp, freq, and phase.
      i) think about threading issues.
    b) In CP5, reparse the strings and update the Expr vars every loop. Use CP5 as a mutex to remove threading issues
   */
  //amplitude, frequency, phase; y(t) = Ae^(i*2*pi*f*t + phase)
  def phasors(time:Float = time):Seq[Vec2] = CP5.synchronized{ //Since each PApplet lives on its own thread we're not actually sure when a draw will occur.
    (ampExpr, freqExpr, phaseExpr) match {
      case (Right(amp), Right(freq), Right(phase)) => (1 to CP5.N).map(n => {
        varMap("n") = n;
        phasor(amp.eval, 2*PI*freq.eval*time + phase.eval)
      })
      case _ => List()
    }
  }

  def totalPhasor(time:Float = time) = (phasors(time) foldLeft Vec2())(_ + _)

  object CP5 extends PApplet {

    type E = Either[String, Expr]
    var (ampExpr, freqExpr, phaseExpr) = (null:E, null:E, null:E)

    def setColors(tc: ControllerInterface) {
//      tc.setColorActive(color(8, 0x8a, 0xcf)) //This affects what the button and bang look like when it's moused over. default 0x 08 a2 cf
      tc.setColorLabel(color(0, 0, 0)) //make the label text show up in black
//      tc.setColorValue(color(0, 0, 0));
    }

    object cp5 extends ControlP5(this) { app =>
      val (xOffset, yOffset) = (10, 10);
      override def register(tc: ControllerInterface) {
        tc.getPosition.sub(-xOffset, -yOffset, 0);
        setColors(tc)
        super.register(tc)
      }

      private def withHook[T, A](thing: => T)(hook:T => A) = {
        val t = thing
        hook(t)
        t;
      }

      private def mkTextfield(name: String, y: Int, initText:String) = {
        val s = addTextfield(name, 0, y, width / 2, 15); s.linebreak();
        s.setText(initText);
        s;
      }
      val ampControl = mkTextfield("Amplitude", 0, "1/10");
      val freqControl = mkTextfield("Frequency / Hz", 40, "n");
      val phaseControl = mkTextfield("Phase / rad", 80, "0");

      val numberControl = mkTextfield("How many", 120, "10");

      val timeLabel = withHook(addTextlabel("time", "", 0, 170))(_.setColorValueLabel(CP5.color(0))) //make the time label show up in black.
      val reset = withHook(addBang("set t = 0", width / 2, 170-12, 25, 25))(x => {
        x.addListener(new ControlListener {def controlEvent(theEvent: ControlEvent) {
            timeMillis = 0; //reset the time to t = 0
          }});
//        x.setColorForeground(0xFF00698c)}) //This affects how the bang looks untouched. default 0x 00 69 8c
      })

      val pausedButton = withHook(addButton("Paused", 0, width / 2 + 40, 170-12, 25, 25))(x => {
        x.setColorBackground(times(4, brighten _)(CP5.color(0x00, 0x36, 0x52))); //This affects how the button looks untouched. default 0x 00 36 52
        x.setColorForeground(times(2, brighten _)(CP5.color(0xc8, 0x8a, 0xcf)))         //this affects what the button looks like when it's clicked.
      })
      val speedControl = mkTextfield("Speed", 210, ".01");

      //times(0, x => x+2) = x => x
      //times(1, x => x+2) = x => x + 2
      //times(2, x => x+2) = x => (x + 2) + 2
      //times(3, x => x+2) = x => ((x + 2) + 2) + 2
      private def times[A](num:Int, f:A => A):A => A = num match {
        case 0 => identity;
        case k => x => f(times(num-1, f)(x))
      }

      //the problem is that the button text is unreadable since the text goes over both
    }

    def N = try { cp5.numberControl.getText.toInt } catch { case _ => 0 }
    def isPaused = cp5.pausedButton.booleanValue();
    def speed = try { cp5.speedControl.getText.toFloat } catch { case _ => 0f }

    override def setup() {
      size(250, 250)
      cp5; //force controlP5.
      lastMillis = millis();
      Thread.currentThread().setName("CP5 Thread")
    }

    private var lastMillis = 0;

    override def draw() {
      CP5.synchronized {
        ampExpr = Parser.parse(cp5.ampControl.getText)
        freqExpr = Parser.parse(cp5.freqControl.getText)
        phaseExpr = Parser.parse(cp5.phaseControl.getText)
      }
      background(bgColor)
      val now = millis();
      if (!isPaused) {
        timeMillis += speed * (now - lastMillis);
      }
      lastMillis = now;
      cp5.timeLabel.setValue("t = " + time + " sec");

      List(Argand, CosGraph, SinGraph).foreach(_.redraw)
    }
  }

  private trait Grapher extends MyPApplet {

    var scaleRange = 1f;
    override def setup() {
      size(250, 250);
      Thread.currentThread.setName(getClass.getSimpleName+" Thread")
      smooth();
      noLoop();
      addMouseWheelListener(new MouseWheelListener {
        def mouseWheelMoved(e: MouseWheelEvent) {
          scaleRange *= pow(1.03f, e.getWheelRotation)
        }
      })
    }
  }

  private object CosGraph extends MyPApplet with Grapher {

    override def draw() {
      import Argand.cam
      background(bgColor);

      /*
       * Here's how things will connect:
       * a) CosGraph's drawing methods interpret all numbers as screen coordinates.
       * b) To draw something as if it were living in model space, we provide a method to convert from
       *    model coordinates to screen coordinates - this is the toScreen method.
       * c) For all drawing invocations, we use toScreen(model) as necessary
       *
       * The model space has an
       *
       * x-range defined as [cam.modelX(0), cam.modelX(width)]    (scale = cam.getScale)
       * y-range defined as [-scaleRange, scaleRange]             (scale = height / (2*scaleRange))
       */

      def toScreen(model:(Float, Float)) = Vec2(map(model._1, cam.modelX(0), cam.modelX(width), 0, width),
                                                map(model._2, -scaleRange, scaleRange, 0, height));


      //draw coordinate lines
      strokeWeight(1);
      stroke(0);
      line(toScreen(0, -scaleRange), toScreen(0, scaleRange)) //draw a vertical line down the middle of the screen
      line(0, height/2, width, height/2) //draw a horizontal line through the middle of the screen

      //draw the big current line
      strokeWeight(6);
      stroke(cosColor);
      line(toScreen(0, 0), toScreen(totalPhasor().x, 0f));

      //draw the graph. We want height/2 to call totalPhasor(time) exactly. We want the graph to extend up and down by scaleRange.
      //We should iterate through each y coordinate, transform it to the corresponding time
      //(y = 0 -> t = time - tRange, y = height/2 -> t = time, y = height -> t = time + tRange).
      stroke(darken(cosColor)); noFill();
      strokeWeight(1);
      beginShape();
      for(y <- 0 until height) {
        val timeOffset = (y - height / 2) * scaleRange / height;
        val curTime = time + timeOffset;
        val x = totalPhasor(curTime).x
        vertex(toScreen(x, 0).x, y); //we only need the X coordinate in the model space
      }
      endShape();
    }
  }

  private object SinGraph extends MyPApplet with Grapher {

    override def draw() {
      import Argand.cam
      background(bgColor);

      /*
       * Here's how things will connect:
       * a) SinGraph's drawing methods interpret all numbers as screen coordinates.
       * b) To draw something as if it were living in model space, we provide a method to convert from
       *    model coordinates to screen coordinates - this is the toScreen method.
       * c) For all drawing invocations, we use toScreen(model) as necessary
       *
       * The model space has an
       *
       * x-range defined as [-scaleRange, scaleRange]             (scale = width / (2 * scaleRange))
       * y-range defined as [cam.modelY(0), cam.modelY(height)]   (scale = cam.getScale)
       */

      def toScreen(model:(Float, Float)) = Vec2(map(model._1, -scaleRange, scaleRange, 0, width),
                                                map(model._2, cam.modelY(0), cam.modelY(height), 0, height));


//draw coordinate lines
      strokeWeight(1);
      stroke(0);
      //draw a vertical line down the center of the screen
      line(width/2, 0, width/2, height)
      //draw a horizontal line through model-y zero
      line(toScreen(-scaleRange, 0), toScreen(scaleRange, 0))

      //draw the big current line
      strokeWeight(6)
      stroke(sinColor)
      line(toScreen(0, 0), toScreen(0f, -totalPhasor().y)) //negate

      //draw the graph. Go from x [0, width], transforming it to the corresponding time.
      stroke(darken(sinColor)); noFill();
      strokeWeight(1);
      beginShape();
      for(x <- 0 until width) {
        val timeOffset = (x - width/2) * (scaleRange / width);
        val curTime = time + timeOffset;
        val y = totalPhasor(curTime).y;
        vertex(x, toScreen(0, -y).y); //we only need the Y coordinate in the model space; //negate
      }
      endShape();
    }
  }

  object Argand extends MyPApplet {

    import PConstants._;

    lazy val cam = new zhang.Camera(this);

    override def setup() {
      size(250, 250)
      cam.setCenter(0, 0);
      cam.setViewportWidth(4);
      smooth();
      noLoop();
      Thread.currentThread().setName("Argand Thread")
    }

    override def draw() {
      background(bgColor);
      zhang.Methods.strokeWeightScreen(this, 1, cam);
      stroke(0);
      noFill();
      ellipse(0, 0, 2, 2); //unit circle of diameter 2 = radius 1
      P5Util.drawGraphAxes(this, cam);

      zhang.Methods.strokeWeightScreen(this, 6f, cam);
      val tp = totalPhasor()
      stroke(cosColor);
      line(0, 0, tp.x, 0)
      stroke(sinColor);
      line(0, 0, 0, -tp.y) //negate

      zhang.Methods.strokeWeightModel(this, 2f, cam);
      stroke(vecColor);
      (phasors() foldLeft Vec2()) {
        (accum: Vec2, enum: Vec2) => {
          def drawPhasor(start: Vec2, p: Vec2) {
            def line(a:Vec2, b:Vec2) { Argand.line(a.x, -a.y, b.x, -b.y); } //negate
            line(start, p);
            val offset = p - start;
            line(p, p - offset.rotate(PI / 8).scale(.2f))
            line(p, p - offset.rotate(-PI / 8).scale(.2f))
          }
          drawPhasor(accum, accum + enum);
          accum + enum;
        }
      }
    }

    override def mouseDragged() {
      cam.translate(pmouseX - mouseX, pmouseY - mouseY)
    }
  }

}
  
