package daily

import processing.core._
import peasy.PeasyCam
import org.zhang.lib.MyPApplet
import zhang.Methods
import toxi.color.{ColorList, ColorGradient, ReadonlyTColor, TColor}
import org.zhang.lib.misc.{Vec3, Vec2, TravList}

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 8/6/11
 * Time: 1:44 PM
 */
class Aug06 extends MyPApplet with Savable {

  import PApplet._
  import PConstants._;
  import K.Orient
  implicit def tcolor2Int(t:ReadonlyTColor) = t.toARGB
  implicit def int2TColor(t:Int) = TColor.newARGB(t)
  private implicit def tupleOfInts2Orient(t:(Int, Int)) = Orient.fromIJ(t._1, t._2)

//  @inline
//  def mapf(value:Float, istart:Float, istop:Float, ostart:Float, ostop:Float) = ostart + (ostop - ostart) * ((value - istart) / (istop - istart));


  def index(o:Orient) = o.j*resolution+o.i//y*width+x

  //Wrapper object for Orients so that no one uses the Orient constructor
  private object K {
  /**
  * Represents an orientation in the world, represented either by rectangular i-j coordinates or by azimuth-elevation coordinates.
  * Create Orient objects by calling Orient.fromIJ or Orient.fromAE.
  * i ranges from [0, resolution - 1] while j ranges from [0, resolution] (that is, i is exclusive of the resolution endpoint but j is inclusive)
  * az ranges from [0, 2*PI*(resolution-1)/resolution], while el ranges from [-PI/2, PI/2]
  * azimuth (az) is synonymous with longitiude, while elevation (el) is exactly the same measurement as latitude (using radians)
  */
    case class Orient protected[K] (i:Int, j:Int) extends (Int, Int)(i, j) {

  //    private def this(aa:Float, ee:Float) = this()
  //    val i = _1
  //    val j = _2
      lazy val i1 = Orient.fromIJ(i+1, j)
      lazy val j1 = Orient.fromIJ(i, j+1)
      lazy val ij1 = Orient.fromIJ(i+1, j+1)

      lazy val az = map(i, 0, resolution, 0, TWO_PI)
      lazy val el = map(j, 0, resolution, -PI/2, PI/2)

      def +(d:Orient) = Orient.fromIJ(i + d.i, j + d.j)

      def offset(azEl:(Float, Float)) = Orient.fromAE(az+azEl._1, el + azEl._2)

      def unary_- = Orient.fromIJ(-i, -j)

      override def toString = "(i, j = "+i+", "+j+") (az, el = "+az+", "+el+")"
    }
    object Orient {
      private val cache = Array.ofDim[Orient](resolution*(resolution+1))
      (0 to resolution).foreach( j => (0 until resolution).foreach( i => cache(j*resolution+i) = Orient(i, j)))

      def fromIJ(ii:Int, jj:Int):Orient = cache(
        { val j = jj % (resolution + 1); if(j < 0) j + (resolution + 1) else j } * resolution +
        { val i = ii % resolution; if(i < 0) i + resolution else i })
      
      def fromAE(az:Float, el:Float):Orient = fromIJ(map(az, 0, TWO_PI, 0, resolution).toInt, map(el, -PI/2, PI/2, 0, resolution).toInt)

      def all =
        (0 to resolution).flatMap( j =>
          (0 until resolution).map( i =>
            fromIJ(i, j)))

      def rand = fromIJ(random(resolution).toInt, random(resolution+1).toInt)
    }
  }


/** Grid holds the actual underlying arrays representing the planet's heights and colors. The underlying array is set up
* in the same way that Processing's pixel array is. You may imagine the planet as being a 2D image of width = resolution and
* height = resolution + 1, where the i and j values in an Orient represent the x and y coordinates of the image. Except that
* Grid holds both height and color values for each pixel.
            i (az)
      +------------------+   ^
      |                  |   |
      |                  |   |height
      |                  |   |
  j   |                  |   |   =
      |                  |   |
 (ele)|                  |   |resolution+1
      |                  |   |
      |                  |   |
      |                  |   |
      +------------------+   v

      |------------------|
       width = resolution

*/
  private object Grid {
    private val grid = Array.ofDim[Float](resolution*(resolution+1))
    private val colors = Array.ofDim[Int](resolution*(resolution+1))

    def reset() {
      (0 until grid.length).foreach(i => {grid(i) = 0f; colors(i) = 0; minCache = 0;})
    }

    def update(o: Orient, x:Float) { grid.update(index(o), x); updateCache(x); }
    def update(o: Orient, c:Int) { colors.update(index(o), c) }

    def heightAt(o: Orient) = grid(index(o))
    def colorAt(o: Orient) = colors(index(o))

    /**
    *  Calculate the magnitude of the gradient at the given orient. This is calculated by calculating the partial
    *  derivative with respect to the i and j directions (averaging the derivatives of both plus and minus in each direction)
    *  and then using the pythagorean theorem with the two average-partials.
    */
    def slopeAt(o:Orient) = {
      val myHeight = heightAt(o)
      def slope(di:Int, dj:Int) = heightAt(o + (di, dj)) - myHeight // /dist(0, 0, di, dj) -- commented out because it's always 1
      def avgSlope(di:Int, dj:Int) = (slope(di, dj) + slope(-di, -dj)) / 2
      val (partialI, partialJ) = (avgSlope(1, 0), avgSlope(0, 1))
      dist(0, 0, partialI, partialJ)
    }

    private var minCache = Float.MaxValue
    private var maxCache = 0f
    private def updateCache(newVal:Float) {
//      mhCache = grid.reduceLeft(_ min _) //todo: put back in!
      minCache = minCache min newVal
      maxCache = maxCache max newVal
    }

    def minHeight = minCache
    def maxHeight = maxCache
  }


  private object Entities extends collection.mutable.SetProxy[Entity] {
    val self = collection.mutable.Set[Entity]()

    def reset() { self.clear }
  }//collection.mutable.Set[Entity]

  /**
  * A colorizer "paints" the landscape.
  * The colorAt method should return the color for the given orient.
  * The colorizerDefinedAt method should return a boolean indicating whether the colorizer will make any noticable changes
  * to the given Orient.
  *
  * When you have a colorizer "c", call "c.spray()" to automatically apply the colorizer to every orient it is defined at.
  * This mutates the grid.
  */
  private sealed trait Colorizer {
    def colorAt(h: Orient):Int

    /**
    * Apply the colorizer to all orients that it is defined at.
    */
    def colorize() { Orient.all.filter(colorizerDefinedAt _).foreach(orient => Grid(orient) = colorAt(orient)) }

    def colorizerDefinedAt(h: Orient):Boolean
  }
  private class MonoColorizer(c:Int) extends Colorizer {
    def colorizerDefinedAt(h: Orient) = true
    def colorAt(h: Orient) = c
  }
  //A HeightColorizer only looks at the height of the grid at the given orient.
  private trait HeightColorizer extends Colorizer {
    final def colorAt(h: Orient) = colorAt(Grid.heightAt(h))
    final def colorizerDefinedAt(h: Orient) = colorizerDefinedAt(Grid.heightAt(h))

    def colorAt(h: Float):Int
    def colorizerDefinedAt(h: Float):Boolean
  }

  /**A ColorRangeColorizer has a map from heights to colors. ColorRangeColorizer's map holds one color
  * per integer height, so this colorizer will only affect heights below the size of the map. The map is gauranteed
  * to have values in the height range [0, map.size). The map is implemented as a ColorList.
  */
  private trait ColorRangeColorizer extends HeightColorizer {
    val map:ColorList

    def colorAt(h:Float) = map.get(h.toInt)
    def colorizerDefinedAt(h: Float) = h < map.size()
  }
  //A very basic colorizer that makes everything below 300 look green.
  private class GreenColorizer(max:Float) extends ColorRangeColorizer {
    val map = {
      val colorRange = new ColorGradient()

      def addColorAt(lvl: Float, hex: String) {
        colorRange.addColorAt(lvl*max, TColor.newHex(hex))
      }

      addColorAt(0,   "279C1C") //greens
      addColorAt(1/2f, "279C1C") //greens
      addColorAt(1f, "0E5707") //dark greens
      colorRange.calcGradient()
    }
  }
  //Colors heights above the snowLevel various shades of white
  private class SnowColorizer(val snowLevel:Float) extends HeightColorizer {
    def colorAt(h: Float) = color(200 + random(h - snowLevel))

    def colorizerDefinedAt(h: Float) = h > snowLevel
  }

  /**
  * Colors orients that have a sharp change in height. cliffThresh determines how steep the change has to be.
  */
  private class CliffColorizer(val cliffThresh:Float) extends Colorizer {
    def colorizerDefinedAt(h: Orient) = Grid.slopeAt(h) > cliffThresh
    def colorAt(h: Orient) = {
      val s = Grid.slopeAt(h)
      lerpColor(Grid.colorAt(h), color(0, 0, 0), (s - cliffThresh) / 5)
    }
  }

  /**
  * A Feature changes the landscape in some way; they are mountains, hills, craters, and also include features generated
  * through noise.
  * The apply method should denote how much height the Feature has at the given Orient.
  * The isDefinedAt method should denote whether the feature exists or makes any noticable difference at the given Orient.
  */
  sealed private abstract class Feature extends PartialFunction[Orient, Float] {
    def addGrid() { updateGrid(_ + _) }
    def overwriteGrid() { updateGrid((a, b) => b) }
    def updateGrid(combiner: (Float, Float) => Float) {//combiner specifies "how" to add this feature to the Grid
      Orient.all.filter(isDefinedAt _).foreach(orient => Grid(orient) = combiner(Grid.heightAt(orient), apply(orient)))
    }

    def +(f:Feature) = new FeatureCombiner(this, f, _ + _)
  }
  private class FeatureCombiner(f1:Feature, f2:Feature, func:(Float, Float) => Float) extends Feature {
    def apply(v1: Orient) = func(f1(v1), f2(v1))
    def isDefinedAt(x: Orient) = f1.isDefinedAt(x) && f2.isDefinedAt(x)
  }

  private class FlatFeature(val level:Float) extends Feature {
    def apply(v1: Orient) = level

    def isDefinedAt(x: Orient) = true
  }
  //A continuous noise feature that ranges from 0 to amp.
  private abstract class NoiseFeature(val scl:Float, val amp:Float) extends Feature {
    protected val dim3 = random(100) //this ensures that different NoiseFeature instances will actually produce different heights.
    def apply(v1: Orient) = amp*noiseFunc(v1.az*scl, v1.el*scl, dim3)

    def noiseFunc(x:Float, y:Float, z:Float):Float
    def isDefinedAt(x: Orient) = true
  }
  private class PerlinFeature(scll:Float, ampp:Float) extends NoiseFeature(scll, ampp) {
    private val offsetEl = PI/2 //since el ranges from -PI/2 to PI/2 and perlin noise is an even function, add an offset to prevent the symmetry
    override def apply(v1: Orient) = amp*noiseFunc(v1.az*scl, (v1.el + offsetEl)*scl, dim3)

    def noiseFunc(x:Float, y:Float, z:Float) = noise(x, y, z)
  }
  private class SimplexFeature(scll:Float, ampp:Float) extends NoiseFeature(scll, ampp) {
    implicit def d2f(d:Double) = d.toFloat
    def noiseFunc(x:Float, y:Float, z:Float) = (toxi.math.noise.SimplexNoise.noise(x, y, z)+1)/2 //make sure the nosie ranges from [0, 1]
  }

  private class ImageFeature(img:PImage, conversion: Int => Float) extends Feature with Colorizer {
    def toPixel(o: Orient) =
             (round(map(o.i, 0, resolution, 0, img.width)),
              round(map(o.j, 0, resolution+1, 0, img.height)))

    //=========Feature stuff=========
    def apply(v1: Orient) = { val (x, y) = toPixel(v1); conversion(img.get(x, y)) }
    def isDefinedAt(x: Orient) = true

    //========Colorizer stuff========
    def colorizerDefinedAt(h: Orient) = true
    def colorAt(h: Orient) = { val (x, y) = toPixel(h); img.get(x, y) }
  }
  private class BrightnessFeature(img:PImage, maxHeight:Float = 255) extends ImageFeature(img, brightness(_)*maxHeight / 255) // divide by 255 because brightness [0, 255]
  /**
  * A CenteredFeature is a feature that has some sort of focal point; e.g. the tip of a mountain, the bottom of a crater.
  * As of now, CenteredFeature also has a <radius> field to denote maximum outward extent from the <center> of the feature;
  * in this way, the feature is only defined within a circle of center <center> and radius <radius>. The <radius> is defined
  * in AE coordinates (so as to keep resolution-independence).
  */
  private abstract class CenteredFeature(center:Orient, radius:Float) extends Feature {

    /**
    * Return the height of the CenteredFeature at the given polar coordinate away from the center. "rad" is a normalized
    * metric in the range [0, 1] that maps to [0, radius] away from "center"
    */
    def apply(rad:Float, ang:Float):Float

    /**
    * Returns the *real* offset in AE coordinates from the center to the given Orient. This method
    * returns small negative numbers to denote that it's easier to go "backwards". Originally there was
    * a.-(b), where a and b were orients, but since orients are always positive, it'd say things like
    * (1, 1) - (5, 5) = (96, 97), which is technically true but which gives you junk values when used for
    * distance.
    * This method returns (change in Azimuth, change in Elevation).
    */
    def getOffset(x: Orient): (Float, Float) = {
      import Methods.distance
      val dAz = distance(center.az, x.az, TWO_PI);
      val dEl = distance(center.el + PI / 2, x.el + PI / 2, PI + .001f);//add an epsilon value to the wrap to make +PI inclusive
      (dAz, dEl)
    }

    /**
    * Return the height of the CenteredFeature at the given Orient. This method should just convert orients to polar
    * coordinates centered around "center".
    */
    final def apply(x: Orient) = {
      val offset = getOffset(x)
      val angle = atan2(offset._2, offset._1) //atan2(y, x)
      val rad = map(dist(0, 0, offset._2, offset._1), 0, radius, 0, 1)
      apply(rad, angle)
    }

    def isDefinedAt(x: Orient) = {
      val offset = getOffset(x)
      dist(0, 0, offset._1, offset._2) < radius
    }
  }
  private class RadialFeature(c:Orient, r:Float, func:Float => Float) extends CenteredFeature(c, r) {
    def apply(rad: Float, ang: Float) = func(rad)
  }
  private class PointyMountainFeature(center:Orient, radius:Float, height:Float) extends RadialFeature(center, radius, (f:Float) => sq(f-1)*height)
  private class CraterFeature(center:Orient, radius:Float, depth:Float) extends RadialFeature(center, radius, (f:Float) => -sqrt(1 - f*f) * depth)
  private object PlateauFeature {
    private var offsetStart = 0f

    def findGoodOffset(range:Float = TWO_PI, epsilon:Float = 1e-5f, start:Float = offsetStart) = {
      val sampleInterval = .001f; //maybe calculate this with respect to epsilon
      var offset = start;
      while(abs(noise(offset)-noise(offset+range)) > epsilon) offset += sampleInterval
      offset
    }

    //Returns a Float => Float that takes an angle as a parameter and returns the outer radius of the plateau at that angle.
    //The returned radius should be in the range [0, 1]
    def nextFunction() = {
      val offset = findGoodOffset()
      offsetStart = offset //this ensures that no two plateaus will be the same.
      (angle:Float) => noise(angle + offset)
    }

  }
  private class PlateauFeature(center:Orient, radius:Float, height:Float) extends CenteredFeature(center, radius) {
    //func takes an angle as a parameter and returns the outer radius of the plateau at that angle.
    val func = PlateauFeature.nextFunction()

    def apply(rad: Float, ang: Float) = if(func(ang) > rad) height else 0
  }
  private abstract class CenteredFeatureRange(start:Orient, cons: (Orient, Float) => CenteredFeature) extends Feature {
    val rangeList:Seq[(Orient, Float)]
  }

  private object Sea extends FlatFeature(-1) with ColorRangeColorizer {
    val seaLevel:Float = 260
    override val level = seaLevel

    //stuff for Feature:
    override def isDefinedAt(o: Orient) = Grid.heightAt(o) <= seaLevel

    //stuff for ColorRangeColorizer
    /**
    * range is a mapping between the grid's height and its color. calling range.get(seaLevel) gives you the color of
    * land of height seaLevel (aka depth zero). range.get(0) gives you the surface color for land at a height of 0 (aka depth
    * = seaLevel)
    */
    val map = {
      val grad = new ColorGradient()
      /**
      * Add a hex color to the given depth. depthNorm ranges from [0, 1] and is a normalized metric for the depth of the water.
      * For instance addColorAt(0, "FAF7A5") means that at a depth of 0, the color will be FAF7A5 (light yellow).
      * addColorAt(.5f, "101B82") means that at a depth of seaLevel/2, the color will be 101B82 (darkish blue)
      * addColorAt(1, "02184F") means that at a depth of seaLevel, the color will be 02184F (dark blue)
      */
      def addColorAt(depthNorm:Float, hex:String) = grad.addColorAt(depthNorm*seaLevel, TColor.newHex(hex).setAlpha(1))
      addColorAt(1, "FAF7A5")
      addColorAt(.95f, "3242D1")
      addColorAt(.8f, "222E9C")
      addColorAt(.5f, "101B82")
      addColorAt(0, "02184F")

      grad.calcGradient()
    }
    assert(map.size() == seaLevel)
  }

  private sealed abstract class Entity {
    Entities.+=(this)

    def orient:Orient
    def height:Float

    def act()
    def draw()

    def die() {onDeath(); Entities.-=(this) }
    def onDeath() {}
  }
  private trait SurfaceEntity extends Entity {
    def height = Grid.heightAt(orient)
  }
  private trait NoAct extends Entity { def act() {} }
  private class Tree(val orient:Orient) extends SurfaceEntity with NoAct {
    private val myTree = {
      val height = random(12, 60);
      new util.Tree(
        height = height,
        extent = random(12, height/2),
        baseRadius = random(5, height/5),
        trunkColor = 0xFF7D320A + color(random(25), random(25), random(25)),
        greenMin = 32, greenMax = 128)
    }
    def draw() {
      myTree.draw(Aug06.this)
    }


  }
  private object Tree {
    def prefers(o:Orient) = TColor.newHex("279C1C").distanceToRGB(Grid.colorAt(o)) < .5f //!Sea.isDefinedAt(o) //&& Grid.heightAt(o) < Grid.minHeight + 100

    def makeForest(o:Orient) = matching(o.offset(randOffset(45, 20)), prefers).take(40)
  }

  def randOffset(azDeg:Float, elDeg:Float) = (radians(azDeg*random(-1, 1)), radians(elDeg*random(-1, 1)))

  private class Cloud(var orient:Orient, var height:Float) extends Entity {

    def act() {
      orient = orient.offset(randOffset(5, 4))
    }

    def draw() {
      
    }
  }

  def matching[A](s: => A, pred:A => Boolean) = Stream.continually(s).filter(pred)

  val resolution = 100
  lazy val cam = new PeasyCam(this, 600)

  /**
  * TODO FOR THE FUTURE
  * <DONE>A feature that takes an image => heightMap/colorMap</DONE>
  *
  * <DONE>Refactor DS =>
  *     1) VertexMode {plotPoint} (with Wireframe and Mesh (and maybe Voxel))
  *     2) CoordinateSystem{fromCartesian, normalAt defined}
  *     3) Renderer that takes a vtxmode and CoordinateSystem {draw() { goes through all Orients and plotPoints the fromCartesian}}
  * </DONE>
  *
  * Make superclass of colorizer and feature that has an "isDefinedAt", a "<thing>At", and an "updateGrid" that updates the Grid
  *
  * Modify Orient so that the elevation doesn't wrap around (it either throws an error or 
  * 
  * Moons
  *
  * Make sun show directionalLight
  *
  * *maybe*, in Space, transform the planet instead of the sun
  */
  override def setup() {
    size(600, 600, P3D)
    cam //this makes cam un-lazy
    newGrid()
  }

  var drawAxes = true

  def newGrid2() {
    new FlatFeature(300).overwriteGrid()
    val img = loadImage("mtp-0058.png");
    val b = new BrightnessFeature(img, 125);
    b.addGrid(); b.colorize()

//    new GreenColorizer(425).colorize()
//    new CliffColorizer(5).colorize()
//    new SnowColorizer(420).colorize()
    //smooth out the landscape
    //    for(i <- 0 until 2) smoothGrid()

//    Sea.colorize()
//    Sea.overwriteGrid()
  }

  def newGrid() {
    reset()

    new FlatFeature(190).overwriteGrid()
    new SimplexFeature(1, 40).addGrid()
    new PerlinFeature(10, 10).addGrid()
//    new SimplexFeature(4, 5).updateGrid(_ - _)
    new PerlinFeature(.5f, 100).addGrid()

    for(i <- 0 until 3)  new CraterFeature(Orient.rand, .5f, 25).addGrid()
    if(random(1) < .25f) new CraterFeature(Orient.rand, 1, 50).addGrid()
    for(i <- 0 until 10) new PointyMountainFeature(Orient.rand, .2f, 50).addGrid()
    if(random(1) < .8f)  for(i <- 0 until 4)  new PlateauFeature(Orient.rand, random(.25f, 2f), random(25, 100)).addGrid()
    new PlateauFeature(Orient.rand, random(.5f, 1.25f), 200 + random(20, 150)).updateGrid(_ max _)

    new GreenColorizer(Grid.maxHeight).colorize()
    new CliffColorizer(6).colorize()
    new SnowColorizer(400).colorize()
    //smooth out the landscape
    //    for(i <- 0 until 2) smoothGrid()


    Sea.colorize()
    Sea.overwriteGrid()

    //make 4 forests of 40 trees each
    matching(Orient.rand, Tree.prefers).take(4).flatMap(Tree.makeForest _).foreach(o => new Tree(o))
  }

  def reset() {
    Grid.reset()
    Entities.reset();
  }

  private class VertexMode(val prepare: Int => Unit, val shapeMode:Int, val vtxOrder:Seq[Orient => Orient])
  private object MeshVM extends VertexMode(fill _, QUADS, Seq(identity, _.i1, _.ij1, _.j1))
  private object WireframeVM extends VertexMode(stroke _, LINES, Seq(identity, _.i1, identity, _.j1))
//  object BothVM extends VertexMode((i:Int) => {stroke(i); fill(i)}, QUAD, Seq(identity, _.i1, _.ij1, _.j1))

  private trait CanOcclude {
    def occluded(o: Orient):Boolean
  }
  private abstract class CoordinateSystem {
    val to = resolution

    def convert(az:Float, el:Float, rad:Float):(Float, Float, Float)
    def convert(o:Orient, z:Float):(Float, Float, Float) = convert(o.az, o.el, z)
    def convert(o:Orient):(Float, Float, Float) = convert(o, Grid.heightAt(o))

    def normal(o:Orient) = convert(o, 1)
  }
  private object PlanarCS extends CoordinateSystem {
    override val to = resolution - 1
    var scl = 600 / TWO_PI
    def convert(az: Float, el: Float, rad: Float) = (az*scl - scl/2, el*scl, rad)
  }
  private object SphericalCS extends CoordinateSystem with CanOcclude {
    import zhang.Methods.{cosf => cos, sinf => sin}
    def convert(az: Float, el: Float, rad: Float) = (rad * cos(az) * cos(el), rad * sin(az) * cos(el), rad * sin(el))

    def occluded(o:Orient) = {
      /* To test if a point is occluded (assuming the radial center of the planet is situated at the center of the screen)
       * 1) If the orient is on the side facing the camera, then it's not occluded
       * 2) Get the voxel that the orient points to
       * 3) Transform that voxel into its accompanying screen pixel coordinates
       * 4) Convert Grid.minHeight into a pixel distance (call it pDist)
       * 5) If that pixel is within a circle centered at the center of the screen with radius pDist, then it's occluded
       * 6) otherwise it isn't.
       *
       * -1-) To test if the Orient is facing the camera
          * 1) Get the camera's azimuth and elevation with respect to the planet (call it cAz and cEl)
          * 2)
          * //todo: finish
       */

      false
    }
  }
  private object TorodialCS extends CoordinateSystem {
    import zhang.Methods.{cosf => cos, sinf => sin}
    def fromSpherical(r:Float, t:Float, angleZ:Float) = Vec3(r*cos(t)*cos(angleZ), r*sin(t)*cos(angleZ), r*sin(angleZ))

    def convert(az: Float, el: Float, rad: Float) = {
      //todo: optimize
      val bigCircle = fromSpherical(400, az, 0)
      val littleCircle = fromSpherical(rad, az, el*2) //el [-PI/2, PI/2] => el*2 [-PI, PI]
      bigCircle + littleCircle
    }
  }

  private abstract class View {
    def before()
    def after()
  }
  private object DefaultView extends View {
    def before() {
      lights();
    }
    def after() {}
  }
  private object InSpaceView extends View {
    lazy val bg = {
          val bg = createGraphics(width, height, JAVA2D)
          bg.beginDraw()
          bg.background(0);
          bg.smooth(); bg.noStroke(); bg.fill(255);
          for(i <- 0 until 100) {
            val rad = random(1, 5)
            bg.ellipse(random(width), random(height), rad, rad)
          }
          bg.filter(BLUR, .4f)
          bg.endDraw()
          bg;
        }

    def before() {
      background(bg)
      val sunPoint = SphericalCS.convert(map(millis(), 0, 20000, 0, TWO_PI), 0, 800)
      pushMatrix()
      translate(sunPoint)
      fill(255, 255, 0); noStroke();
      sphere(300) //sun
      popMatrix()
      val v = Vec3(sunPoint).ofMag(500)
//      pointLight(255, 255, 255, v)
      directionalLight(255, 255, 255, -v.x, -v.y, -v.z)
  }
    def after {}
  }

  private object Renderer {
    val vmList = new TravList(Seq(MeshVM, WireframeVM))
    val csList = new TravList(Seq(SphericalCS, PlanarCS, TorodialCS))
    val viewList = new TravList(Seq(DefaultView, InSpaceView))
//    var vm:VertexMode = MeshVM
//    var cs:CoordinateSystem = PlanarCS
//    var addon:View = DefaultView

    def vm = vmList.item;
    def cs = csList.item;
    def view = viewList.item;

    def render() {
      view.before()
      noFill(); noStroke();
      beginShape(vm.shapeMode)
      (0 until cs.to).foreach(i => {
        (0 to cs.to).foreach(j => {
          val o = Orient.fromIJ(i, j)
          vm.vtxOrder.map(_(o)).foreach(vtx _)
//          vtx(o); vtx(o.j1); vtx(o.ij1); vtx(o.i1);
        })
      })
      endShape()

      //todo: render entities
      Entities.foreach(e => {
        //transform to the entity's location, rotate the positive Z axes to the normal, and then draw
        pushMatrix();
        translate(cs.convert(e.orient, e.height))
        val norm = cs.normal(e.orient);
        //rotate the +Z to the normal
        rotateAtoB(Vec3(0, 0, 1), Vec3(norm))
        e.draw()
        popMatrix();
      })
      view.after();
    }

    private def vtx(o:Orient) {
      vm.prepare(Grid.colorAt(o))
      val j = cs.convert(o)
      vertex(j)
    }
  }

  override def draw() {
    background(255);
    Renderer.render()
    if(drawAxes) Methods.drawAxes(g, 100)
    fill(0)
    textMode(SCREEN)
    textAlign(LEFT, TOP)
    val dispString = {
      import Renderer._
      def name[A <: AnyRef](o:TravList[A]) = o.item.getClass.getSimpleName.drop("Aug06$".length).dropRight(1);
      "CoordinateSystem: "+name(csList)+"\nVertexMode: "+name(vmList)+"\nView: "+name(viewList)
    }
    text(dispString+"\n"+frameRate, 0, 0)
    pollSave("aug06-")
  }

  def smoothGrid() {
    Orient.all.foreach(o => Grid(o) = List(o, o + (0, 1), o + (1, 0), o + -(0, 1), o + -(1, 0)).map(Grid.heightAt _).sum / 5f)
  }

  override def keyPressed() {
    super.keyPressed()
    Renderer.vmList.move(key, 'q', 'w')
    Renderer.csList.move(key, 'a', 's')
    Renderer.viewList.move(key, 'z', 'x')
//    if(key == 's') smoothGrid()
    if(key == 'n') newGrid()
    if(key == 'g') drawAxes = !drawAxes;
  }
  
}