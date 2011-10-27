package daily.jun

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 6/27/11
 * Time: 5:22 PM
 */
import processing.core._
import JMyron.JMyron
import processing.core.PApplet._
import zhang.Methods
import org.zhang.lib.misc.TravList

class Jun27 extends PApplet {
  import PApplet._; import PConstants._;
  var myron:JMyron = new JMyron()
  setBackground(loadImage("background.jpg"))
  //Copies the contents of the webcam to the given image
  def copyCamera(i:PImage) {
    i.loadPixels
    arrayCopy(myron.image(), i.pixels)
    i.updatePixels
  }

  //Gets the current camera as a PImage
  def cameraImage = {val i = createImage(myron.getForcedWidth, myron.getForcedHeight, RGB); copyCamera(i); i; }

  //Set the background to the given image
  def setBackground(i:PImage) { bg = i }


  /**
  * Holds some sort of "background" image.
  */
  var bg:PImage = _
  /**
  * Holds the camera image.
  */
  var cam:PImage = _
  /**
  * Holds the last image that the mangler returned
  */
  var lastImage:PImage = _



  val MODES = Array(BLEND, ADD, SUBTRACT, LIGHTEST, DARKEST, DIFFERENCE, EXCLUSION, MULTIPLY, SCREEN, OVERLAY, HARD_LIGHT, SOFT_LIGHT, DODGE, BURN)
  val MODESTRING = Array("BLEND", "ADD", "SUBTRACT", "LIGHTEST", "DARKEST", "DIFFERENCE", "EXCLUSION", "MULTIPLY", "SCREEN", "OVERLAY", "HARD_LIGHT", "SOFT_LIGHT", "DODGE", "BURN")
  val modeList = new TravList[(Int, String)](MODES.zip(MODESTRING), 5)
  val manglers = new TravList[ImageMangler](Array(Mode1, Mode2))
  def BMODE = modeList.item._1

  override def setup() {
    size(640, 480)
    cam = createImage(width, height, RGB)
    myron.start(width, height)
  }

  var analyze = false
  override def draw() {
//  val pic1 = loadImage("050.JPG")
//  val pic2 = loadImage("051.JPG")
    myron.update
    copyCamera(cam)
//    cam = pic1
//    bg = pic2
    lastImage = manglers.item.mangle
    if(analyze) {
      val newImage = analyze(lastImage)
      image(newImage, 0, 0)
    }
    else {
      image(lastImage, 0, 0)
    }

    textAlign(LEFT, TOP)
    text((modeList.index+1)+"/"+MODES.length+"("+modeList.item._2+")", 0, 0)
  }

  override def keyPressed() {
    if(key == '.') setBackground(cameraImage)
    if(key == ',') setBackground(lastImage)
    modeList.move(keyCode, LEFT, RIGHT)
    manglers.move(key, 'q', 'w')
    if(key == 'a') analyze = !analyze
  }

  def analyze(i:PImage) = {
    val ret = createImage(i.width, i.height, RGB);
    ret.loadPixels
    i.loadPixels
    ret.pixels = i.pixels.map(i => if(brightness(i) > map(mouseX, 0, width, 0, 255)) color(255) else color(0))
    ret.updatePixels
    ret
  }

  class ImageMangler(func: () => PImage) {
    def this() = this(() => null)
    def mangle:PImage = func()
  }
  object Mode1 extends ImageMangler(() => {
      cam.blend(bg, 0, 0, width, height, 0, 0, width, height, BMODE)
      cam;
  })
  object Mode2 extends ImageMangler {
    val bgClone = createImage(width, height, RGB)
    override def mangle = {
    bgClone.copy(bg, 0, 0, width, height, 0, 0, width, height)
    bgClone.blend(cam, 0, 0, width, height, 0, 0, width, height, BMODE)
    bgClone
    }
  }

  override def stop() {
    myron.stop
    super.stop()
  }

}