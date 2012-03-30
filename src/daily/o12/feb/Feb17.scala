package daily

import processing.core._
import org.zhang.lib.MyPApplet
import org.zhang.geom.Vec2

class Feb17 extends MyPApplet with Savable {

  import PApplet._;
  import PConstants._;

  class Map {

    class Vertex private[Vertex] (pos:Vec2) extends Vec2(pos.x, pos.y) {
      /**
       * All of the edges that this vertex is connected to
       * @return
       */
      def edges = Edge.all filter {_(this).isDefined}
      def faces = Face.all filter {_(this)}
    }
    object Vertex {
      private val stashThresh = 1e-4f
      private var stash = Set[Vertex]()
      def apply(pos:Vec2) = stash.find(x => (x distTo pos) < stashThresh).getOrElse{
        val v = new Vertex(pos)
        stash += v
        v
      }

      def all = stash
    }

    class Face private[Face] (val center:Vec2) {
      val vertices:Seq[Vertex] = (0 until 6) map {i => Vertex(center + Vec2.fromPolar(1, i * TWO_PI / 6))}
      val edges:Seq[Edge] = (vertices :+ vertices.head).sliding(2).toSeq.map{ case Seq(a, b) => Edge(a, b) }
      println(## +" - " + edges.length)

      def apply(v:Vertex) = touching(v)
      def touching(v:Vertex) = vertices contains v
    }
    object Face {
      private val stashThresh = 1e-4f
      private var stash = Set[Face]()
      def apply(center:Vec2) = stash.find(f => (f.center distTo center) < stashThresh).getOrElse{
        val v = new Face(center)
        stash += v
        v
      }

      def all = stash
    }

    class Edge private[Edge] (val v1:Vertex, val v2:Vertex) {
      def apply(v:Vertex) = other(v)
      def other(v:Vertex) = if(v == v1) Some(v2) else if(v == v2) Some(v1) else None
    }
    object Edge {
      private var stash = Set[Edge]()
      def apply(v1:Vertex, v2:Vertex) = stash.find(e => e(v1).isDefined && e(v2).isDefined).getOrElse{
        val v = new Edge(v1, v2)
        stash += v
        v
      }

      def all = stash
    }

    //Initialization of objects
    {
      val start = Face(Vec2())
      val tileDist = (Vec2.fromPolar(1, 0) + Vec2.fromPolar(1, 1 * TWO_PI / 6)).mag
      Seq.iterate(Set(start), 3){ set =>
        set.flatMap(f => (0 until 6) map {i => Face(f.center + Vec2.fromPolar(tileDist, i * TWO_PI / 6 + (TWO_PI / 12)))})
      }
    }

    def faces = Face.all
    def edges = Edge.all
    def vertices = Vertex.all
  }

  val map = new Map()

  override def setup() {
    size(800, 600)
    new zhang.Camera(this)
  }

  override def draw() {
    noFill(); stroke(255); rect(0, 0, width, height)
    background(0)
    colorMode(HSB)
    def drawFace(f:Map#Face) {
      noStroke(); fill(f.hashCode() % 255, 255, 255, 64)
      lines2(f.vertices, true)
    }
    def drawEdge(e:Map#Edge) {
      noFill(); stroke(e.hashCode() % 255, 255, 255, 64)
      line(e.v1, e.v2)
    }
    def drawVertex(v:Map#Vertex) {
      noStroke(); fill(v.hashCode() % 255, 255, 255, 64)
      ellipse(v, 1/10f, 1/10f)
    }

    translate(width/2, height/2)
    scale(width/2, width/2)
    scale(.2f)
//    map.faces foreach (drawFace _)


    strokeWeight(15/(width/2f))
//    map.faces.head.edges foreach (drawEdge _)
    map.edges foreach (drawEdge _)

//    map.vertices foreach (drawVertex _)

    pollSave() //check if the screen should be saved
  }

  override def keyPressed() {
    super.keyPressed(); //pressing space toggles a boolean variable to save the screen
  }
}