package daily

import processing.core._
import org.zhang.lib.MyPApplet
import themidibus.MidiBus
import util.parsing.combinator.RegexParsers

class May11 extends MyPApplet with Savable {

  import PApplet._;
  import PConstants._;
  implicit def d2f(d:Double) = d.toFloat

  var midibus:MidiBus = null

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

  /**
   * The Pitch class represents a musical tone as a MIDI scale number where 60 is the C3 and each semitone changes the index by 1.
   * Each octave
   */
  case class Pitch(i:Int)
  object Pitch {
    private val noteMap = Map('c' -> 0, 'd' -> 2, 'e' -> 4, 'f' -> 5, 'g' -> 7, 'a' -> 9, 'b' -> 11)
    //A3 = 69
    //C3 = 60
    //
    def toIndex(pc:String, octave:Int = 3):Int =
      24 +                                        //offset to match MIDI standard
      octave * 12 +                               //12 semitones per octave
      noteMap(pc.head.toLower) +                  //base pitch modifier
      Map("" -> 0, "b" -> -1, "#" -> 1)(pc.tail)  //sharp/flat modifier

    def apply(pc:String, octave:Int = 3):Pitch = apply(toIndex(pc, octave))
    def apply(pString:String):Pitch = apply(PitchParser.toIndex(pString))
  }

  private object PitchParser extends RegexParsers {
    private def pc:Parser[String] = """[a-gA-G](#|b)?""".r
    private def octave:Parser[String] = "\\d+".r
    def pitch:Parser[Int] = (pc ~ opt(octave)) ^^ {
      case pc ~ Some(octave) => Pitch.toIndex(pc, octave.toInt)
      case pc ~ None => Pitch.toIndex(pc, 3)
    }

    def pitchSet: Parser[List[Int]] = rep(pitch)

    def toIndex(pString:String) = parse(pitch, pString).get
  }

  implicit def i2p(i:Int) = Pitch(i)
  implicit def s2p(s:String) = Pitch(s)
  implicit def t2p(t:(String, Int)) = Pitch(t._1, t._2)
  implicit def p2i(p:Pitch) = p.i

  case class Note(pitch:Pitch, volume:Int = 64, duration:Time = .25f)
  implicit def s2n(s:String) = Note(s)
  implicit def tst2n(t:(String, Time)) = Note(pitch = Pitch(t._1), duration = t._2)
  implicit def tsf2n(t:(String, Float)) = Note(pitch = Pitch(t._1), duration = t._2)


  case class TimedNote(note:Note, loc:Time)
  implicit def tn2n(tn:TimedNote) = tn.note

  //  object Note {
//
//    def apply(pitchString:String, loc:Time, volume:Int = 63, duration:Time = .125f):Note = apply(Pitch(pitchString), loc, volume, duration)
//    def apply(pc:String, octave:Int, loc:Time, volume:Int = 63, duration:Time = .125f):Note = apply(Pitch(pc, octave), loc, volume, duration)
//  }
  case class Notes(notes:Set[TimedNote], length:Time) {
    def play() {
//    val sorted = notes.toSeq.sortBy(_.loc.millis)
//    tryDelay(sorted.head.loc.toInt)
//    for(Seq(now, next) <- sorted sliding 2) {
//      midibus.sendNoteOn(1, now.pitch, now.volume)
//      tryDelay((next.loc - now.loc).toInt)
//    }
//    midibus.sendNoteOn(1, sorted.last.pitch, sorted.last.volume)
//    tryDelay(sorted.last.duration.toInt)
      val start = millis()
      def process(now:Int, last:Int) {
        def inRange(x:Int, low:Int, high:Int) = (if(high < length.millis.toInt) x < high else x <= high) && x >= low
        //find all notes that should start in this step, start them
        notes.filter(n => inRange(n.loc.toInt, last, now)) foreach {n => midibus.sendNoteOn(1, n.pitch, n.volume)}
        //find all notes that should stop in this step, stop them
        notes.filter(n => inRange((n.loc + n.duration).toInt, last, now)) foreach {n => midibus.sendNoteOff(1, n.pitch, n.volume)}

        //if there are notes left in the future, keep processing
  //      if(notes.exists(n => (n.loc + n.duration).millis > now))
        if(length.millis > now)
          process(millis() - start, now)
      }
      process(0, 0)
    }

    def delay(t:Time) = Notes(notes map {x => x.copy(loc = x.loc + t)}, length + t)
    def overlay(n:Notes) = Notes(notes ++ n.notes, max(length.t, n.length.t))
    def repeat(num:Int) = {
      def func(accum:Notes, times:Int):Notes = if(times <= 1) accum else {
        func(this / (accum > length), times-1)
      }
      func(this, num)
    }

    def >(t:Time) = delay(t)
    def /(n:Notes) = overlay(n)
    def *(n:Int) = repeat(n)

  }
  object Notes {
  //todo: figure out how to incorporate rests; they should not appear as notes but rather just move the timing along
  //todo: add support for chord literals
    def apply(notes:Note*):Notes = {
      val (set, time) = notes.foldLeft((Set[TimedNote](), Time(0))){ case ((set, time), note) => ((set + TimedNote(note, time), time + note.duration))}
      apply(set, time)
    }
  }

  def tryDelay(amt:Int) {
    if(amt != 0) delay(amt)
  }


  override def setup() {
    size(500, 500)
    midibus = new MidiBus(this, -1, 1)
  }

  override def draw() {
//    play(Set(Note(65, 0), Note(67, .25f), Note(69, .5f), Note(70, .75f)), 1)
//    val note = Note("C", 0, 63, .25f)
    (Notes("C", "D", "E", ("F", .1f))*2).play();
    delay(1000)
    println(frameCount)
    pollSave() //check if the screen should be saved
  }

  override def keyPressed() {
    super.keyPressed(); //pressing space toggles a boolean variable to save the screen
  }
}