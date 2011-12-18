package daily
package nov

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 11/12/11
 * Time: 7:01 PM
 */
import processing.core._
import org.zhang.lib.MyPApplet
import org.zhang.geom.Vec2
import collection.SeqProxy

class Nov12 extends MyPApplet with Savable { app =>
  import PApplet._; import PConstants._;

  /*Categories I see:
   * Nothing but grass (4, 0) -> (7, 3), (8, 0) -> (12, 0), (8, 3) -> (9, 3), (0, 14)
   * Dirt 1, 14
   *
   * Fence 1 (5, 5) -> (6, 6)
   *
   * Fence 2
   *    N 6, 7
   *    E 7, 8
   *    W 5, 8
   *    S 6, 8
   *    gate 6, 9
   *    NW 5, 7
   *    NE 7, 7
   *    SW 5, 9
   *    SE 7, 9
   *
   * Grassy cliff
   *    N (1, 16), (2, 16)
   *    E (3, 15), (3, 16)
   *    W (0, 15), (0, 16)
   *    S (1, 17), (2, 17)
   *    SW (0, 17)
   *    SE 3, 17
   *    NW 1, 15
   *    NE 2, 15
   */
  private object Tiles {

    object images extends SeqProxy[IndexedSeq[PImage]] {
      val self = for(x <- 0 until rawImage.width by 32) yield for(y <- 0 until rawImage.height by 32) yield rawImage.get(x, y, 32, 32)

      def apply(pt:(Int, Int)):PImage = apply(pt._1)(pt._2)
    }

    case class Direction(var r:Direction, dx:Int, dy:Int) { def reverse = r; }
    case object North extends Direction(South, 0, -1)
    case object South extends Direction(null, 0, 1); South.r = North;
    case object East extends Direction(West, 1, 0)
    case object West extends Direction(null, -1, 0); West.r = East;
    val Up = North; val Down = South; val Left = West; val Right = East;
    val directions = List(North, South, East, West)


    trait Face

    class GrassEdgeFace(val grassEdge:Direction) extends Face

    /**
     * Grass in north, dirt in south.
     * A face that has grass extruding from the north and ending in dirt in the south (e.g. (5, 16)).
     * Should only be placed in east/west directions.
     */
    case object GNDS extends GrassEdgeFace(North)

    /**
     * A face that has grass extruding from the south and ending in dirt in the north (e.g. 5, 17)
     * Should only be placed in east/west directions.
     */
    case object GSDN extends GrassEdgeFace(South)

    /**
     * A face that has grass extruding from the east and ending in dirt in the west (e.g. 7, 16).
     * Should only be placed in north/south directions.
     */
    case object GEDW extends GrassEdgeFace(East)

    /**
     * A face that has grass extruding from the west and ending in dirt in the east (e.g. 4, 17).
     * Should only be placed in north/south directions.
     */
    case object GWDE extends GrassEdgeFace(West)

    /**
     * A presentation is a mapping between directions and faces. It might look something like this:
     * [North -> GrassFace]
     * [South -> GrassFace]
     * [East -> DirtFace]
     * [West -> GEDW]
     *
     * This means that for a tile T that has this presentation, T presents grass in the north and south, dirt in the east, and GEDW in the west.
     */
    case class Presentation(map:Map[Direction, Face]) {
      assert(map.size == 4)

      //this asserts that none of the GrassEdgeFaces are placed in an invalid direction.
//      assert(map.collect{ case (dir, f:GrassEdgeFace) => f.grassEdge == dir || f.grassEdge == dir.r }.fold(true)(_  && _))

      /**
       * This method returns true if the given tile, placed the given direction relative to this presentation, is a successful match.
       * <code>tile</code> will be a match iff this presents, in the <code>adjacency</code> direction, the same face as the one tile presents
       * in adjacency's reverse.
       */
      def accept(tile:TileType, adjacency:Direction) = map(adjacency) == tile.presentation.map(adjacency.r)

      def face(dir:Direction) = map(dir)

      def flipped = Presentation(map.map{case (d, f) => (d.r, f)})
    }
    object Presentation {
      def apply(north:Face, south:Face, east:Face, west:Face):Presentation = Presentation(North -> north, South -> south, East -> east, West -> west)
      def apply(f:Face):Presentation = apply(f, f, f, f)
      def apply(t1:(Direction, Face), t2:(Direction, Face), t3:(Direction, Face), t4:(Direction, Face)):Presentation = Presentation(Map(t1, t2, t3, t4))
    }

    /*
     * Each tile has a set of requirements for the tiles placed adjacent to it. For instance, the plain grass tile
     * requires that its north block have grass as a south block, its
     *
     * What is the general requirement for two blocks to look good adjacent to each other? Both blocks have to agree
     * that the block adjacent to it is a successful match. How does one block say the other is successful?
     *
     * Resolution 1) Each block has a set of acceptable faces for a given direction; the other block must provide that face in that direction's reverse.
     *    Also, each block has a face (or a set of faces) that it provides for each direction.
     *
     * Resolution 2) Each block simply has one face (or set of faces) that it provides for each direction; the other block must have a matching face
     *    in that direction's reverse.
     *
     * 2) {
     * Take the GrassDirtNW tile (4, 15). It presents:
     *    grass from west to east (GWDE)          north
     *    grass from north to south (GNDS)        east
     *    dirt                                    south and west
     *
     * The grass tile presents:
     *    grass in all directions
     *
     * The dirt tile presents:
     *    dirt in all directions
     *
     * The GrassDirtW tile presents:
     *    grass from west to east (GWDE)            north and south
     *    dirt                                      east
     *    grass                                     west
     *
     *
     * }
     *
     */
    abstract sealed class TileType(imgLocX:Int, imgLocY:Int) {
      def this(pt:(Int, Int)) = this(pt._1, pt._2)
      val image = images(imgLocX, imgLocY)
      assert(image.width == 32 && image.height == 32)

      def draw(pt:(Int, Int)) { draw(pt._1, pt._2) }
      def draw(x:Int, y:Int) { app.image(image, x*32, y*32) }

      val presentation:Presentation
    }

    object Grass extends TileType(4, 0) with Face {
      val presentation = Presentation(this)
    }
    object Dirt extends TileType(1, 14) with Face {
      val presentation = Presentation(this)
    }

    /**
     * Grass-dirt interface where grass is on the northwest corner (4, 15). It presents:
     *    grass from west to east (GWDE)          north
     *    grass from north to south (GNDS)        west
     *    dirt                                    south and east
     */
    object GrassDirtNW extends TileType(4, 15) {
      val presentation = Presentation(North -> GWDE, West -> GNDS, South -> Dirt, East -> Dirt)
    }
    object GrassDirtNE extends TileType(7, 15) {
      val presentation = Presentation(North -> GEDW, East -> GNDS, South -> Dirt, West -> Dirt)
    }
    object GrassDirtSW extends TileType(6, 15) {
      val presentation = Presentation(South -> GWDE, West -> GSDN, North -> Dirt, East -> Dirt)
    }
    object GrassDirtSE extends TileType(5, 15) {
      val presentation = Presentation(South -> GEDW, East -> GSDN, North -> Dirt, West -> Dirt)
    }

    /**
     * Grass-dirt West: grass-dirt interface where grass is on the west (4, 16). It presents:
     *    grass from west to east                     north and south
     *    dirt                                        east
     *    grass                                       west
     */
    class GrassDirtW(x:Int, y:Int) extends TileType(x, y) {
      val presentation = Presentation(North -> GWDE, South -> GWDE, East -> Dirt, West -> Grass)
    }
    object GrassDirtW1 extends GrassDirtW(4, 16)
    object GrassDirtW2 extends GrassDirtW(4, 17)

    class GrassDirtE(x:Int, y:Int) extends TileType(x, y) {
      val presentation = Presentation(North -> GEDW, South -> GEDW, East -> Grass, West -> Dirt)
    }
    object GrassDirtE1 extends GrassDirtE(7, 16)
    object GrassDirtE2 extends GrassDirtE(7, 17)

    class GrassDirtN(x:Int, y:Int) extends TileType(x, y) {
      val presentation = Presentation(North -> Grass, South -> Dirt, East -> GNDS, West -> GNDS)
    }
    object GrassDirtN1 extends GrassDirtN(5, 16)
    object GrassDirtN2 extends GrassDirtN(6, 16)

    class GrassDirtS(x:Int, y:Int) extends TileType(x, y) {
      val presentation = Presentation(South -> Grass, North -> Dirt, East -> GSDN, West -> GSDN)
    }
    object GrassDirtS1 extends GrassDirtS(5, 17)
    object GrassDirtS2 extends GrassDirtS(6, 17)

    val types = List(Grass, Dirt,
      GrassDirtNW, GrassDirtNE, GrassDirtSW, GrassDirtSE,
      GrassDirtE1, GrassDirtE2,
      GrassDirtW1, GrassDirtW2,
      GrassDirtN1, GrassDirtN2,
      GrassDirtS1, GrassDirtS2
    )

    def randomType = org.zhang.lib.random(types)

    case class Tile(x:Int, y:Int, tipe:TileType) {
      def accept(dir:Direction, tipe:TileType) = tipe.presentation.accept(tipe, dir)
      def draw() { tipe.draw(x, y) }
    }
  }
  import Tiles._

  lazy val rawImage = loadImage("artwork\\tilesets\\Exterior_Farm.png")
  private lazy val landscape = Array.fill[Option[Tile]](25, 20)(None)
  override def setup() {
    size(landscape.length * 32, landscape(0).length*32)
    rawImage; //force it
    images; //force it
  }

  override def draw() {
    background(0);
    landscape.foreach(_.foreach(_.foreach(_.draw())))

    val i = 1+random(23).toInt
    val j = 1+random(18).toInt
    val tipe = randomType
    def accept(dir:Direction, tile:Option[Tile]):Boolean = tile match {
      case Some(t) => t.accept(dir, tipe)
      case None => true
    }
    //check acceptability of all adjacent tiles
    if(directions.forall(d => accept(d, landscape(i+d.dx)(j+d.dy))))
    {
      landscape(i)(j) = Some(new Tile(i, j, tipe))
    }

//    image(rawImage, 0, 0);
//    import Vec2._
//    if(clicked != null)
//      image(clicked, ints._1*32, ints._2*32)
//    noFill();
//    rect(ints*32, 32, 32);
//    text(ints.toString, mouseX, mouseY)
//    pollSave() //check if the screen should be saved
  }

  def ints = (mouseX/32, mouseY/32);

  override def mousePressed() {
  }

  override def keyPressed() {
    super.keyPressed(); //pressing space toggles a boolean variable to save the screen 
  }
}