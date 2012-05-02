package daily

import processing.core._
import org.zhang.lib.MyPApplet

class Mar01 extends MyPApplet with Savable {



  import PApplet._;
  import PConstants._;

  val DOT_NUM = 10000;
  val dots = List.fill(DOT_NUM)(new Dot())
  var as = List[Attractor]()

  val DT = .2f;
  var drag:Float = 0

  val cosCache = (0 until 360).map{i => cos(map(i, 0, 360, -PI, PI))}
  val sinCache = (0 until 360).map{i => sin(map(i, 0, 360, -PI, PI))}

  var goNum:Int = 0;

  def cosf(rad:Float) = {
    var idx = (360 * ((rad+PI)/TWO_PI)).toInt % 360;
    if (idx < 0) idx += 360;
    cosCache(idx);
  }

  def sinf(rad:Float) = {
    var idx = (360 * ((rad+PI)/TWO_PI)).toInt % 360;
    if (idx < 0) idx += 360;
    sinCache(idx);
  }

  def reset() {
    as = List()
    background(0);
    dots.foreach(_.reset())
    drag = pow(2, randi(0, 4))/100 - .01f;//0, .01, .03, .07, .15
    goNum = randi(1, 6);
    as :+= new Attractor();
  }

  var magMin, magMax = 0f;

  override def setup() {
    //do not change, this is fixed for LED screens.
    size(512, 160, P2D);
    background(0);
    reset();
  }

  override def draw() {
    println(frameRate);
    //  background(0); //keep background black (LEDs are brightness based)
    fill(0, 64);
    rect(0, 0, width, height);
    stroke(255, 0, 0); //LEDs are on the scale of red
    fill(255, 0, 0);   //LEDs are on the scale of red

    for (_ <- 0 until 5) {
      magMin = 999999;
      magMax = -1;
      dots foreach (_.run())
      dots foreach (_.update())
    }

    if (millis() % 8000 < 180) {
      if (as.length > goNum) {
        reset();
      } else {
        as :+= new Attractor();
      }
    }

  /*
    fill(255);
    for (Attractor a : as) {
      text(a.f.getClass().getSimpleName(), a.x, a.y);
    }*/
    pollSave() //check if the screen should be saved
  }


  class Dot(var x:Float, var y:Float, var dx:Float, var dy:Float, var ax:Float, var ay:Float) {
    var mag2:Float = _
    var mag:Float = _

    def this() = this(random(width), random(height), 0, 0, 0, 0);

    def reset() {
      x = random(width);
      y = random(height);
      dx = 0;
      dy = 0;
      ax = 0;
      ay = 0;
      computeMags();
    }

    def computeMags() {
      mag2 = dx*dx+dy*dy;
      mag = sqrt(mag2);
    }

    def run() {
      computeMags();
      val d = mag;
      if (d > magMax) magMax = d;
      if (d < magMin) magMin = d;
      as.foreach(_(this))
    }

    def update() {
      val ox = x;
      val oy = y;
      stroke(255*(mag-magMin)/(magMax - magMin), 0, 0);
      dx += ax * DT;
      dy += ay * DT;
      dx *= 1 - drag;
      dy *= 1 - drag;
      x += dx * DT;
      y += dy * DT;
      ax = 0;
      ay = 0;
      //    stroke(255, 0, 0);
      //    stroke(64 + mag * 10, 0, 0);
      line(ox, oy, x, y);

      if (x < 0 || x > width || y < 0 || y > height) {
        reset();
      }
    }
  }

  class Attractor(val x:Float = random(width), val y:Float = random(height), val f:Force = randomForce()) {

    def apply(d:Dot) {
      val ff = f.force(this, d);
      d.ax += ff.x;
      d.ay += ff.y;
    }
  }

  abstract class Force {
    def force(a:Attractor, d:Dot):PVector
  }
  case class PushForce(power:Float, exponent:Float = 1) extends Force {

    def force(a:Attractor, d:Dot) = {
      val dx = a.x - d.x;
      val dy = a.y - d.y;
      val mag = dist(0, 0, dx, dy);
      new PVector(power * dx / pow(mag, exponent), power * dy / pow(mag, exponent));
    }
  }

  case class TwirlForce(power:Float) extends Force {

    def force(a:Attractor, d:Dot) = {
      val dx = a.x - d.x;
      val dy = a.y - d.y;
      val dist = sqrt(dx*dx+dy*dy);
      val ang = atan2(dy, dx);
//      if(d.mag2 < 1)
        new PVector(1, 1);
//      else
//      new PVector(power * cosf(ang+PI/2), power * sinf(ang+PI/2));
//        new PVector(d.mag2 / dist * cosf(ang+PI/2), d.mag2 / dist * sinf(ang + PI/2))
    }
  }

  case class TanForce(power:Float, offset:Float = random(TWO_PI)) extends Force {

    def force(a:Attractor, d:Dot) = {
      val dx = a.x - d.x;
      val dy = a.y - d.y;
      val ang = atan2(dx, dy) + offset;
      new PVector(power * cosf(ang), power * sinf(ang));
    }
  }

  case class BarForce(power:Float, scale:Float = pow(2, randi(0, 3)), o:Float = random(-PI, PI)) extends Force {

    def force(a:Attractor, d:Dot) = {
      def powHere = power*sinf((d.x*cosf(o) + d.y*sinf(o)) / scale);

      new PVector(powHere*cosf(o), powHere*sinf(o));
    }
  }


  case class RadialBarForce(power:Float, scale:Float = pow(2, randi(0, 3))) extends Force {

    def force(a:Attractor, d:Dot) = {
      val dx = a.x - d.x;
      val dy = a.y - d.y;
      val dist = sqrt(dx*dx+dy*dy);
      val powHere = power*sinf(dist / scale);

      new PVector(powHere * dx / dist, powHere * dy / dist);
    }
  }



  case class TwistForce(power:Float, scale:Float = randi(2, 6)) extends Force {

    def force(a:Attractor, d:Dot) = {
      val dx = a.x - d.x;
      val dy = a.y - d.y;
      val ang = atan2(dy, dx);
      new PVector(power * cosf(ang*scale), power * sinf(ang*scale));
    }
  }


  def randomForce() = {
    val pow = 2*(.2f + randStep(5))*(if(random(1)<.5) -1 else 1);
    randi(0, 5) match {
    case 0 =>
      new PushForce(pow, 1);
    case 1 =>
      new TwirlForce(pow);
    case 2 =>
      new TanForce(pow);
    case 3 =>
      new BarForce(pow);
    case 4 =>
      new RadialBarForce(pow);
    case 5 =>
      new TwistForce(pow);
    case _ => null
    }
  }

  def randStep(num:Int) = randi(0, num-1) / num.toFloat;

  override def keyPressed() {
    if(key == 'r') reset();
  }
}