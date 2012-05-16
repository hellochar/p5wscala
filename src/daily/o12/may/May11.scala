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

  val tempo = 110.0f //in units beats / minute
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

    override def toString = {
      if(zhang.Methods.distance(t, 0, 1/16f) < (1/16f)*(1e-2f)) {//close enough within a 1/16th note to quantize
        val a = (zhang.Methods.roundToNearest(t, 1/16f)*16).toInt
        if(a == 0) "0" else a+"/16"
      }
      else t.toString
    }
  }
  implicit def f2t(f:Float) = Time(f)
  implicit def t2f(t:Time) = t.millis

  /**
   * The Pitch class represents a musical tone as a MIDI scale number where 60 is the C3 and each semitone changes the index by 1.
   * Each octave
   */
  case class Pitch(i:Int) {
    override def toString = Pitch.toString(i)

    def shift(semi:Int) = Pitch(i + semi)
    def ^(semi:Int) = shift(semi)
  }
  object Pitch {
    val noteMap = Map('c' -> 0, 'd' -> 2, 'e' -> 4, 'f' -> 5, 'g' -> 7, 'a' -> 9, 'b' -> 11)
    //A3 = 69
    //C3 = 60
    //
    def toIndex(pc:String, octave:Int = 3):Int =
      24 +                                        //offset to match MIDI standard
      octave * 12 +                               //12 semitones per octave
      noteMap(pc.head.toLower) +                  //base pitch modifier
      Map("" -> 0, "b" -> -1, "#" -> 1)(pc.tail)  //sharp/flat modifier

    def toString(idx:Int) = {
      val indexClass = idx % 12
      val noteInv = noteMap.map{t => t._2 -> t._1.toString}.toMap
      val pc = noteInv.getOrElse(indexClass, noteInv(zhang.Methods.wrap(indexClass - 1, 12).toInt) + "#")

      val octave = idx / 12 - 2

      pc + octave
    }

    def apply(pc:String, octave:Int = 3):Pitch = apply(toIndex(pc, octave))
    def apply(pString:String):Pitch = apply(PitchParser.toIndex(pString))
  }

  private object PitchParser extends RegexParsers {
    private def pc:Parser[String] = """[a-gA-G](#|b)?""".r
    private def octave:Parser[String] = "\\d+".r
    private def pitch:Parser[Int] = (pc ~ opt(octave)) ^^ {
      case pc ~ Some(octave) => Pitch.toIndex(pc, octave.toInt)
      case pc ~ None => Pitch.toIndex(pc, 3)
    }

    private def pitchSet: Parser[List[Int]] = rep(pitch)
    def toSet(s:String) = parse(pitchSet, s).get

    def toIndex(pString:String) = parse(pitch, pString).get
  }

  implicit def i2p(i:Int) = Pitch(i)
  implicit def s2p(s:String) = Pitch(s)
  implicit def t2p(t:(String, Int)) = Pitch(t._1, t._2)
  implicit def p2i(p:Pitch) = p.i

  case class Note(pitch:Pitch, volume:Int = 64, duration:Time = .25f) {

    override def toString = {
      var list:List[Any] = List(pitch)
      if(volume != 64) list :+= volume
      if(duration.t != .25f) list :+= duration
      if(list.length == 1) pitch.toString
      else list.mkString("(", ", ", ")")
    }

    def shift(semi:Int) = copy(pitch = pitch.shift(semi))
    def ^(semi:Int) = shift(semi)
  }
  implicit def s2ln(s:String) = PitchParser.toSet(s).map{Note(_)}
  implicit def tst2ln(t:(String, Time)) = PitchParser.toSet(t._1).map{Note(_, duration = t._2)}
  implicit def tsf2ln(t:(String, Float)) = PitchParser.toSet(t._1).map{Note(_, duration = t._2)}


  case class TimedNote(note:Note, loc:Time) {
    override def toString = "("+note+", "+loc+")"

    def shift(semi:Int) = copy(note = note.shift(semi))
    def ^(semi:Int) = shift(semi)
  }

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
        implicit def tn2n(tn:TimedNote) = tn.note
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
    override def toString = notes.toList.sortBy{_.loc.t}.mkString("[", ", ", "]")

    def delay(t:Time) = Notes(notes map {x => x.copy(loc = x.loc + t)}, length + t)
    def overlay(n:Notes) = Notes(notes ++ n.notes, max(length.t, n.length.t))
    def repeat(num:Int) = {
      def func(accum:Notes, times:Int):Notes = if(times <= 1) accum else {
        func(this / (accum > length), times-1)
      }
      func(this, num)
    }
    def append(n:Notes) = this / (n > length)//Notes(notes ++ n.notes map {t => t.copy(loc = t.loc + length)}, length + n.length)
    def durScale(amount:Float) = Notes(notes map {t => t.copy(note = t.note.copy(duration = Time(t.note.duration.t * amount)))}, length)
    def shift(semi:Int) = Notes(notes map {x => x.copy(note = x.note.shift(semi))}, length)

    def >(t:Time) = delay(t)
    def /(n:Notes) = overlay(n)
    def *(n:Int) = repeat(n)
    def +(n:Notes) = append(n)
    def >>(amount:Float) = durScale(amount)
    def ^(semi:Int) = shift(semi)

  }
  object Notes {
  //todo: figure out how to incorporate rests; they should not appear as notes but rather just move the timing along
    def apply(notes:List[Note]*):Notes = {
      val (set, time) = notes.foldLeft((Set[TimedNote](), Time(0))){ case ((set, time), noteList) => ((set ++ noteList.map{note => TimedNote(note, time)}, time + noteList.map{_.duration.t}.max))}
      apply(set, time)
    }
  }
  //shorthand for notes
  type N = Notes
  val N = Notes

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
//    (N("CEG", "DFA", "EGB", ("FAC4", 1/8f), ("GBD4", 1/8f))*2).play();

    val first = (N(("CE", 3/16f)) >> 2/3f)*4
    val last = (N(("EG", 2/16f)) >> 1/2f)*2
    ((first + last)*3 + (first + (last ^ -2))).play()

//    (N(("CE", 3/16f))*4 + N(("EG", 2/16f))*2)*4 play()

    delay(1000)
    println(frameCount)
    pollSave() //check if the screen should be saved
  }

  override def keyPressed() {
    super.keyPressed(); //pressing space toggles a boolean variable to save the screen
  }
}