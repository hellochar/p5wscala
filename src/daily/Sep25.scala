package daily

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 9/25/11
 * Time: 8:39 PM
 */
import processing.core._
import org.zhang.lib.MyPApplet

class Sep25 extends MyPApplet with Savable {
  import PApplet._; import PConstants._; 

  object Parser {
    val vars: collection.mutable.Map[String, Float] = collection.mutable.Map("n" -> 0)
    val mp = new MathParser(vars);

    val (memFunc, memCache) = org.zhang.lib.memoize(evalStr _)

    private def evalStr(args: (Float, String)) = {
      vars("n") = args._1; mp.parse(args._2);
    }

    def eval(n:Float) = {
      if(memCache.size > 1024) memCache.clear() //keep it from eating too much memory
      memFunc(x, inputString)
    }
  }

  def N = 100;

  override def setup() {
    size(500, 500)
    val cam = new zhang.Camera(this);
    //cam.setWidth(5);
  }

  override def draw() {
    
    
    pollSave("Sep25-")
  }
  
  override def keyPressed() {
    super.keyPressed();
  }
}