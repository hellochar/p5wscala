package daily


/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 8/12/11
 * Time: 10:48 PM
 */
import processing.core._
import toxi.geom.Rect
import org.zhang.lib.{P5Util, MyPApplet}
import ddf.minim.analysis.FFT
import ddf.minim.{AudioSignal, Minim}

class Aug12 extends MyPApplet with Savable { app =>
  import PApplet._; import PConstants._;

  object Audio {
    val BUFFERSIZE = 1024;
    val SAMPLERATE = 44100;

    val minim = new Minim(app)
    var fft = new FFT(BUFFERSIZE, SAMPLERATE)
    val out = minim.getLineOut(Minim.MONO, BUFFERSIZE, SAMPLERATE);
    val buffer = Array.fill(BUFFERSIZE)(0f)
    val signal = new AudioSignal {
      def generate(p1: Array[Float]) {
        java.lang.System.arraycopy(buffer, 0, p1, 0, p1.length)
      }

      def generate(p1: Array[Float], p2: Array[Float]) {
        java.lang.System.arraycopy(buffer, 0, p1, 0, p1.length)
        java.lang.System.arraycopy(buffer, 0, p2, 0, p2.length)
      }
    }
    out.addSignal(signal);

    def addFreq(f:Float, amp:Float) {
      fft.setFreq(f, fft.getFreq(f) + amp);
    }

    /**
    * The seq of chords I, ii, .. etc; each chord is represented a Seq of length 3 whose members are the pitch classes in that chord.
    * e.g. chords(0) contains the Seq(0, 4, 7), which corresponds to the I chord, represented as semitones above the tonic
    */
    val chords = {
      val scale = Seq(0, 2, 4, 5, 7, 9, 11) //scale degree -> semitones. since seq's are 0 indexed, the tonic is 0, supertonic is 1, ... dominant is 4, ... and subtonic is 6
      def get(i:Int) = i%scale.length //7 -> 0, 8 -> 1, 9 -> 2, etc.
      //generate the chords (specified in scale degrees): 024 (I), 135 (ii), 246(iii), 357(IV), 460(V), 571(vi), 602(vii*)
      (0 until scale.length).map(i => Seq(i, get(i+2), get(i+4)).map(scale(_)))
    }

    /**
    * A map of "acceptable" chord progressions. The map's keys are scale degrees, and values are sets of scale degrees to which the given key will progress
    * nicely to.
    */
    val progressions = Map( //borrowed from http://www.musictheory.net/lessons/57
      1 -> ((1 to 7) toSet),
      2 -> Set(5, 7),
      3 -> Set(6),
      4 -> Set(5, 7),
      5 -> Set(1, 6),
      6 -> Set(2, 4),
      7 -> Set(1, 3)
      ).map{ case(a, b) => (a-1, b.map(_-1))} //I wrote the Map in scale degrees that are 1-indexed, but since everything else is 0-indexed i'm converting all of the scale degrees down one.

//    def curChord = chords((millis() / 2500).toInt % chords.length) //go up the chords I, ii, iii, etc.
    val chordStream = Stream.iterate(0)(k => random(progressions(k)))
    def curChord = chords(chordStream((millis / 2500).toInt))
    
    def random[A](e:Iterable[A]) = e.view.drop((math.random*e.size).toInt).first

    def contribute(seq:Seq[Int], wantFreq:Float, baseAmp:Float) {
      val p = (69 + 12*log(wantFreq / 440f) / log(2)).toInt //from http://en.wikipedia.org/wiki/Pitch_class
      val pm12 = p % 12

      //"round" a frequency to its nearest diatonic neighbor (using the C major scale)
//        if(List(1,3,6,8,10).contains(pm12)) p += 1//non-chord tones are 1, 3, 6, 8, 10. if pm12 is one of these, move it up one semitone.


        //only do C major
      if(curChord.contains(pm12)) {

      /* p = 69+12log2(f/440)
         (p - 69)/12 = log2(f/440)
         2^((p-69)/12) = f/440
         440 * 2^((p-69)/12) = f
       */

        val baseFreq = 440f * pow(2, (p-69)/12f) + detune
//        addFreq(baseFreq, baseAmp)
        seq.zipWithIndex.foreach{ case(i, idx) => addFreq(baseFreq*i, baseAmp*pow(2, -idx)) }
      }
    }

    def detune = map(mouseX, 0, width, -10, 10)

    def resetBands() {
      fft = new FFT(BUFFERSIZE, SAMPLERATE)
//      for (i <- 0 until fft.specSize()) fft.setBand(i, 0)
    }

    def populateBands() {
    /*
       * Interpret the current window as some sort of description of sound. Each pixel's hue and brightness dually determines
       *
       * maybe interpret the hue as some sort of generating pattern for octaves and enharmonics, interpret the height as the base
       * note (and maybe round to a scale or a note or something), interpret the x value as ????
       */
      val x = width/2;
      for(y <- 0 until height) {

        val c = get(x, y)
        val amp = brightness(c) / 255f;
        if(amp != 0) {
          val int = {
            if(red(c) == green(c) && red(c) == blue(c)) 1
            else if(green(c) == 0 && blue(c) == 0) 1010101010
            else java.lang.Float.floatToIntBits(hue(c))
          }
          //if it's gray, do '1'. if it's red, do '1010101010'
          contribute(int.toString.map(_ - '0'), map(y, 0, height, 100, 3000), amp)
        }
      }
    }

    def draw() {
      resetBands()
      populateBands()
      fft.inverse(buffer)
//      println("BF "+frameCount+" at "+millis+"!")
    }

//    def dipose() {
//      minim.stop()
//    }
  }

//  object CP5 {
//    val p5 = new ControlP5(app)
//
//  }

//  lazy val buffer = createGraphics(500, 500, JAVA2D)

  //block size - length of the side of a square.
  def bSize = width / (sq(map(constrain(mouseX, 0, width), 0, width, sqrt(1), sqrt(25))).toInt)
  def extent = if(mouseY < 25) 0 else sq(map(mouseY, 25, height, 0, sqrt(10)))

  def morphers = (Range.Double(0, width, bSize)).flatMap(x => (Range.Double(0, height, bSize)).map(y => {
    implicit def d2f(d:Double) = d.toFloat
    Morpher(new Rect(x, y, bSize, bSize), new Rect(x-extent, y-extent, bSize+extent*2, bSize+extent*2))
  }))

  override def setup() {
    size(500, 500)
    background(0)
    colorMode(HSB)
    Audio
  }

  override def mousePressed() { if(mouseButton == RIGHT) background(0); else {fill(255); ellipse(mouseX, mouseY, 25, 25); } }
  override def keyPressed() {
    super.keyPressed();
//    morphers.foreach(m => {stroke(0, 255, 0); noFill(); rect(m.from); stroke(255, 0, 0); rect(m.to) })
    noFill();
    stroke(map((key - ' ').toInt, 0, 0x7F-0x20, 0, 255), 255, 255);
    morphers.foreach(m => {rect(m.from); rect(m.to) })
  }

  def rect(m:Rect) = super.rect(m.x, m.y, m.width, m.height)

  override def draw() {
    if(!((mousePressed:Boolean) && mouseButton == LEFT)) {
      noStroke();
      fill(0, 40); rect(0, 0, width, height)
      fill(255);
      ellipse(P5Util.randomVector(this), 25, 25)

      morphers.foreach(_.morph())
    }
//    background(0)
    Audio.draw()
    textAlign(LEFT, BOTTOM);
    text("frameRate: "+frameRate+"\nBlock size: " + bSize + "\nMorpher extent: " + extent, 0, height)
    pollSave()
  }

  def get(rect: Rect): PImage = try{implicit def f2i(f:Float) = f.toInt; super.get(rect.x, rect.y, rect.width, rect.height) } catch { case _ => {println("get "+rect); null;} }

  trait Morpher {
    def from:Rect
    def to:Rect

    def morph() {
      image(get(from), to.x, to.y, to.width, to.height)
    }
  }
  object Morpher {
    def apply(fromm:Rect, too:Rect) = new Morpher{ def from = fromm; def to = too; }
  }

}