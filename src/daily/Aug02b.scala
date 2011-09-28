//package daily
//
///**
//* Created by IntelliJ IDEA.
//* User: hellochar
//* Date: 8/2/11
//* Time: 3:26 PM
//*/
//import processing.core._
//import actors.Actor
//import ddf.minim.signals.SineWave
//import util.parsing.combinator.JavaTokenParsers
//import collection.mutable.Buffer
//import collection.SeqProxy
//import util.matching.Regex
//
///**
//* So there's the continuum of frequencies, and within this continuum we give names to specific values - a pitch represents both the
//* name and the frequency. Theory requires a reference pitch from which it works (usually it's that A4 maps to 440 Hz). We're splitting
//* the 1D continuum into a two-dimensional plane, with one dimension specified by the octave, and the second dimension specified by the
//* pitch class.
//*
//* ===========Pitch Classes==============
//* There are twelve pitch classes, which correspond to the twelve tones in the equal temperament scale. Pitch Classes (PC) may be specified
//* using an integer 0-11, or through a tonal spelling ("A", "C#", "Gb").
//* ===========Names===========
//* Names are specified with a pitch class and an octave. The pitch class is represented as the regex [A-G](#|b)?. That is, one of A,B,C,D,E,F, or G, optionally
//* followed by a sharp (#) or flat (b) symbol. This means that names are not one-to-one; both A# and Bb represent the same frequency. Pitch classes define a
//*
//*
//*/
//class Aug02b extends PApplet {
//  import PApplet._; import PConstants._;
//  import ddf.minim._;
//  import Synchronizer.notes
//
////  implicit def f2p(f:Float) = Pitch(f)
//  implicit def p2f(p:Pitch) = p.pitch
//  implicit def s2p(s:String) = Pitch(s)
//
//  val SAMPLE_RATE = 44100;
//  lazy val minim = new Minim(this)
//  lazy val out = minim.getLineOut(Minim.STEREO, 2048, SAMPLE_RATE);
//
//  override def setup() {
//    size(500, 500)
//    Synchronizer.start()
//    playNote(Note("A4", .25f, 1000))
//    playNote(Note("C4", .25f, 1000))
//    playNote(Note("E4", .25f, 1000))
//    //playChord(Chord("1", .25, 1000))
//    //playChord(Chord(One, .25, 1000))
//    //playChord(Chord(One.interval(4), .25, 1000))
//    //playChord(Chord(
//  }
//
//  def playNote(n:Note) = Synchronizer.play(n)
//  def playChord(n:Chord) = n.notes.foreach(playNote _)
//
//  object Synchronizer extends Actor {
//
//    private case class Play(n:Note)
//    private case class Stop(n:Note)
//    val notes = Buffer[Note]()
//
//    def act() {
//      while(true) {
//        receive {
//          case Play(note) => {
//            notes += note
//            out.addSignal(note.sine)
//            Waiter(note.duration, () => this ! Stop(note)).start()
//          }
//          case Stop(note) => {
//            notes -= note
//            out.removeSignal(note.sine)
//          }
//        }
//      }
//    }
//
//    case class Waiter[A](time:Long, action:() => A) extends Actor {
//        def act() {
//            this.synchronized {
//                wait(time);
//                action()
//            }
//        }
//    }
//
//    def play(n:Note) = this ! Play(n)
//  }
//
//  case class Note(pitch:Pitch, amplitude:Float, duration:Duration) {
//    lazy val sine = new SineWave(pitch, amplitude, SAMPLE_RATE)
//  }
//
//  type Duration = Long //duration in milliseconds, for now
//  type Octave = Int
//
//  case class Pitch(pc:PitchClass, h:Octave) {
//    def pitch:Float = //calculate pitch here
//    def sharp = interval(1)
//    def flat = interval(-1)
//    def interval(i:Int) = Pitch(pitch*pow(2, i/12f))
////    def interval(s:String) = interval(parse(s)) //m2, M2, P3, P4, P5, m6/M6, m/M7
////    private def parse(s:String):Int = {
////      object Parser extends JavaTokenParsers {
////        def
////      }
////    }
//  }
//  object Pitch {
//    def apply(s:String) = parse(s)
//    def parse(value:String) = Grammar.parse(value)
//    private object Grammar extends JavaTokenParsers {
//      def parse(s:String) = parseAll(Freq, s).get
//
//      /**
//      * This map gives you the frequency ratios between a given class and the pitch class 'A'.
//      */
//      private val SEMITONE_MAP = Map("A" -> 0, "B" -> 2, "C" -> 3, "D" -> 5, "E" -> 7, "F" -> 8, "G" -> 10).map{ case(p, i) => (p, pow(2, i/12f)) }
//
//      /*
//        Freq ::= PitchClass Octave
//        PitchClass ::= A-G [# | b]
//        Octave ::= any integer
//       */
//      def Freq:Parser[Pitch] = (PitchClass ~ opt("#" | "b") ~ Octave) ^^ { case pClass~opt~octave => {
//        //A4 is 440, A3 is 220, A2 is 110, A1 is 55, A0 is 27.5
//        //first find the A pitch of the given octave, then use the semitone map to convert to the given pitch class, then account for sharps and flats
//        val aPitch = pow(2, octave-4)*440 //find the A pitch
//        val pcPitch = Pitch(aPitch*SEMITONE_MAP(pClass))
//        opt match {
//          case Some("#") => pcPitch.sharp
//          case Some("b") => pcPitch.flat
//          case None => pcPitch
//        }
//      }}
//      def PitchClass:Parser[String] = "[A-G]".r
//      def Octave:Parser[Int] = wholeNumber ^^ (_.toInt)
//    }
//  }
//
//  /**
//  * What makes up a pitch class? What is their defining feature?
//  * a) They are required to specify a pitch (along with an octave)
//  * b) Combinations of notes that have pitch classes relating to each other "sound good"
//  */
//  sealed class PitchClass(val index:Int) {
//  }
//
//  class Chord(val notes:Note*) extends SeqProxy[Note] {
//    def self = notes
//  }
//
//  /**
//  * root, third, and fifth
//  */
//  case class Triad(val root:Pitch)
//
////  I wanna be able to say "Play 1, 6, 4, 5" or "4.progressions"; I'm assuming the chord is A minor. In theory, a triad is three notes.
////
//
//  override def draw() {
//    background(204);
//    def draw(n:Note) = {
//      val c = color(map(n.amplitude, 0, 1, 0, 255), 0, 0)
//      val y = log(n.pitch) / log(2)
//      val w = map(n.duration, 0, 1000, 0, 100)
//      fill(c); stroke(0);
//      rect(0, y, w, 10)
//    }
//    notes.foreach(draw _)
//  }
//}