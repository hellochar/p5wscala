package daily

import processing.core._
import org.zhang.lib.MyPApplet
import themidibus.MidiBus

class May11 extends MyPApplet with Savable {

  import PApplet._;
  import PConstants._;
  implicit def d2f(d:Double) = d.toFloat

  lazy val midibus = new MidiBus(this, -1, 1)

  val tempo = 160.0f //in units beats / minute
  val beatsPerMeasure = 4 //in units beats / measure

  case class Time(t:Float) { //t is the percentage of one measure that this Time represents, in units of measures
    def millis = {
      val measuresPerMinute = tempo / beatsPerMeasure //in units measure / minute
      val minutesPerMeasure = 1 / measuresPerMinute // in units minute / measure
      val minutes = minutesPerMeasure * t //the units are (minute / measure) * measure = minute
      val millis = minutes * 60 * 1000 //each minute has 60 seconds, and each second has 1000 ms
      millis
    }

    def +(o:Time) = Time(t + o.t)
    def -(o:Time) = Time(t - o.t)
  }
  implicit def f2t(f:Float) = Time(f)
  implicit def t2f(t:Time) = t.millis

  case class Note(pitch:Int, loc:Time, volume:Int = 63, duration:Time = .25f)

  override def setup() {
    size(500, 500)
  }

  def tryDelay(amt:Int) {
    if(amt != 0) delay(amt)
  }

  def play(notes:Set[Note]) {
    val sorted = notes.toSeq.sortBy(_.loc.millis)
    tryDelay(sorted.head.loc.toInt)
    for(Seq(now, next) <- sorted sliding 2) {
      midibus.sendNoteOn(1, now.pitch, now.volume)
      tryDelay((next.loc - now.loc).toInt)
    }
    midibus.sendNoteOn(1, sorted.last.pitch, sorted.last.volume)
    tryDelay(sorted.last.duration.toInt)
  }

  override def draw() {
//    midibus.sendNoteOn(2, 65, 64);
//    delay(200);
//    midibus.sendNoteOff(2, 65, 64);
    play(Set(Note(65, 0), Note(67, .25f), Note(69, .5f), Note(70, .75f)))
    pollSave() //check if the screen should be saved
  }

  override def keyPressed() {
    super.keyPressed(); //pressing space toggles a boolean variable to save the screen
  }
}