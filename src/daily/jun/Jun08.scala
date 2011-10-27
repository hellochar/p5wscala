package daily.jun

import processing.core.{PVector, PApplet}

/**
* Created by IntelliJ IDEA.
* User: hellochar
* Date: 6/8/11
* Time: 7:17 PM
* To change this template use File | Settings | File Templates.
*/

class Jun08 extends PApplet {

  override def setup() {
    size(500, 500)
  }

  override def draw() {
    ellipse(random(width), random(height), 20, 20)
  }

  trait Liner {
//    def location:Vec2
//    def
  }
  
}