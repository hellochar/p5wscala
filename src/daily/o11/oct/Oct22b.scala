package daily


import org.zhang.lib.MyPApplet
import JMyron.JMyron
import java.lang.StringBuffer
import java.awt.datatransfer.StringSelection

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 10/22/11
 * Time: 1:10 AM
 */

class Oct22b extends MyPApplet with Savable {
  import processing.core._, PApplet._, PConstants._;
  
  lazy val jmyron = new JMyron
  lazy val myFont = createFont("Tahoma", 8.5f)

  override def setup() {
    size(320, 240)
    textFont(myFont);
    jmyron.start(width, height)
//    jmyron.adapt()
//    frameRate(1)
    textAlign(LEFT, TOP);
  }

  override def stop() {
    jmyron.stop
  }

  class Asciier(val things:Seq[Char], yStride:Float) {

    def mapper(i:Int) = brightness(i)
    def xStride(c:Char) = textWidth(c)
    def doWithChar(c:Char, x:Float, y:Float) {}

    def charAt(pixel:Int) = things(map(mapper(pixel), 0, 256, 0, things.length).toInt)

    def ascii(img:Array[Int], width:Int) = {
      val height = img.length / width;
//    val (min, max) = ((k:Array[Float]) => (k.head, k.last))(img.map(brightness _).sorted)
      val buf = new StringBuffer(width * height / 10);
      var y = 0f;
      while(y < height) {
        var x = 0f;
        while(x < width) {
          val c = charAt(img(y.toInt * width + x.toInt))
          buf append c
          doWithChar(c, x, y)

          x += xStride(c);
        }
        buf append '\n'
        y += yStride;
      }
      buf.toString
    }
  }

  object myAsciier extends Asciier(
//    List('.', ',', '-', ':', '*', '@').reverse,
//    List(' ', '*', '.'),
    List('@', '*', ':', '-', '.', ' '),
//    (0x20 to 0x7f).map(_.toChar),
    14) {
    val map = Map(' ' -> 3, '.' -> 4, '-' -> 4, ':' -> 4, '*' -> 6, '@' -> 10)
    override def xStride(c:Char) = map(c)

    override def doWithChar(c: Char, x:Float, y:Float) {
      text(c, x, y)
    }
  }

  lazy val pic = loadImage("050.JPG");

  override def draw() {
    background(0);
//    jmyron.sensitivity(10)
//    jmyron.adaptivity(2)
//    jmyron.update()
//    val img = jmyron.image();
//    loadPixels();
//    arrayCopy(img, pixels)
//    updatePixels()

    copy(pic, 0, 0, pic.width, pic.height, 0, 0, width, height)
    loadPixels();

    updatePixels();
    val s = myAsciier.ascii(pixels, width)
    java.awt.Toolkit.getDefaultToolkit.getSystemClipboard.setContents(new StringSelection(s), null)
    for(x <- 0 until 10) println()
    println(s)
  }

  override def keyPressed() {
    super.keyPressed()
  }
}