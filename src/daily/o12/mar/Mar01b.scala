package daily

import processing.core._
import org.zhang.lib.MyPApplet

class Mar01b extends MyPApplet with Savable {

  import PApplet._;
  import PConstants._;

  val GRID_SIZE = 100
  val SCALAR = 6

  case class Loc(x:Int, y:Int) {
    def +(l:Loc) = Loc(x+l.x, y+l.y)
    def adj = (Seq(Loc(x-1, y), Loc(x+1, y), Loc(x, y-1), Loc(x, y+1))).filter{l => l.x >= 0 && l.y >= 0 && l.x < GRID_SIZE && l.y < GRID_SIZE}
    def adjDiags = (for(i <- x-1 to x+1; j <- y-1 to y+1) yield Loc(i, j)).filter{l => l.x >= 0 && l.y >= 0 && l.x < GRID_SIZE && l.y < GRID_SIZE}
  }


  case class Thing(l:Loc, w:Int, h:Int) {

    val Loc(x, y) = l

    def draw(g:PGraphics) {
      g.rect(x, y, w, h);
    }

    def locs = for(i <- x until x+w; j <- y until y+h) yield Loc(i, j)

    def contains(where:Loc) =
      where.x >= x &&
        where.y >= y &&
      where.x < x+w &&
        where.y < y+h

    def adjDiags = (for(l <- locs; x <- l.adjDiags) yield x).filter(!contains(_))
    def adj = (for(l <- locs; x <- l.adj) yield x).filter(!contains(_))
  }

  class Grid {
    val arr = Array.fill(GRID_SIZE, GRID_SIZE)(true)

    def update(l:Loc, t:Boolean) { update(l.x, l.y, t) }
    def update(x:Int, y:Int, t:Boolean) { arr(x)(y) = t }

    def apply(l:Loc):Boolean = apply(l.x, l.y)
    def apply(x:Int, y:Int) = arr(x)(y)

    def falsify(s:Traversable[Loc]) { s.foreach(this(_) = false)}

    /**
     * Using wave-propagation, this method computes how "far away" the end location is from the set of start locations.
     * This method returns a Map between Locs and Ints; each Int represents how "far away" the corresponding Loc is from
     * the closest start location, taking into account untraversable paths in the grid.
     * @param start
     * @param end
     * @return
     */
    def flood(start:Seq[Loc], end:Loc) = {
      var flood = start.map{_ -> 0}.toMap
      var i = 0;

      //wave expansion
      var sizePrev = 0
      while(!flood.contains(end) && sizePrev != flood.size) {
        sizePrev = flood.size

        flood ++=
          flood.filter(_._2 == i).              //find all nodes at the wavefront
          flatMap{ case (loc, _) => {
            loc.adj.filter(x => this(x) && !flood.contains(x)). //for each node, find all adjacent nodes that live on this grid, and which haven't been marked yet
            map(_ -> (i+1))
        }}
        i += 1;
      }
      flood
    }

    def tryPath(end:Loc, flood:Map[Loc, Int]):Option[Seq[Loc]] = {
      def func(list:Seq[Loc]):Option[Seq[Loc]] = {
        if(!flood.contains(list.head)) None
        else {
          val (loc, i) = (list.head, flood(list.head));
  //        println("found "+loc+", "+i)
          loc.adj.sortBy(-flood.getOrElse(_, 99999)).filter(flood.getOrElse(_, 999999) < i).headOption match {
            case Some(l) => func(l +: list)
            case None =>    Some(list)
          }
        }
      }
      func(List(end))
    }
  }

  var things = Seq[Thing]()

  lazy val pg = createGraphics(GRID_SIZE, GRID_SIZE, P2D);
  override def setup() {
    size(GRID_SIZE * SCALAR, GRID_SIZE * SCALAR)
    (0 until 5) foreach {_ => things :+= Thing(Loc(randi(GRID_SIZE-20), randi(GRID_SIZE-20)), randi(5, 20), randi(5, 20))}
  }

  override def draw() {
    val grid = new Grid()
    things.foreach(x => grid.falsify(x.locs.toSet.flatMap{l:Loc => l.adjDiags})) //falsify the location of all buttons and their adjacent locations

    pg.beginDraw();
    pg.background(0);
    pg.noStroke(); pg.fill(255);
    things.foreach(_.draw(pg))

    pg.loadPixels()
    things.zipWithIndex.foreach{ case (thing, idx) =>
      val t = org.zhang.lib.time {
        val target = mouseLoc + Loc(0, idx*10)
        grid.tryPath(target, grid.flood(thing.adj, target)) foreach { _.foreach { loc => //for every location in the path:
          pg.pixels(loc.y*GRID_SIZE+loc.x) = pg.color(255, idx*50, 0); //color it
          grid.falsify(loc.adjDiags) //falsify it and nearby ones
        } }
      }._2
      println("Finished "+idx+"! Took "+(t/1e6).toInt+" ms")
    }
    for(x <- 0 until GRID_SIZE; y <- 0 until GRID_SIZE) if(!grid.arr(x)(y) && pg.get(x, y) == color(0)) pg.pixels(y*GRID_SIZE+x) = color(0, 0, 255);
    pg.updatePixels()

    pg.endDraw();
    image(pg, 0, 0, width, height)
  }

  def mouseLoc = Loc(mouseX / SCALAR, mouseY / SCALAR)

  override def mousePressed() {
    things :+= Thing(mouseLoc, 10, 10)
  }
}