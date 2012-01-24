package daily
package dec

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 12/18/11
 * Time: 12:36 PM
 */

import processing.core._
import org.zhang.lib.MyPApplet
import collection.SeqProxy

/**
 * Generative 2D cellular automata. Steps to generate:
 * 1) Create a random number of arbitrary states + the empty state
 * 2) Create a random set of rules for each state
 *
 * What is a rule? examples:
 *  a) if you are adjacent to at least 3 of you, place yourself on your own state
 *  b) if there is state X above you, turn into state Y
 * The general pattern of rules is
 *  <predicate> => <new self state>
 * What if multiple predicates are true? how to decide which one goes first? The easiest is to just do a list of predicates and
 *  choose the first true one in the list. What if none of them are true? That itself is a generative property.
 * So the pattern list is <list of generated rules> + <default rule>, where each rule is a tuple of [predicate -> state]
 *  
 * We have a grid of cells; each cell has an x/y position and exactly one state at any given time. 
 * We have an iteration function of type grid -> state; this determines the next state of the given cell
 * as it evolves "through time". 
 */
class Dec18 extends MyPApplet with Savable {
  import PApplet._; import PConstants._;

  trait Grid extends SeqProxy[Seq[Cell]] {
    def width:Int
    def height:Int
//    val self = Array.fill(width, height)(NoState:State) //to begin with
    
    def iterate = Grid(for(xArr <- self) yield for(elem <- xArr) yield elem.iterate(this))
  }
  object Grid {
    def apply(x:Int, y:Int) = new Grid {
      val width = x
      val height = y
      val self = Seq.tabulate(width, height){case (x, y) => Cell(x, y, NoState)}
    }
    def apply(states:Seq[Seq[Cell]]) = new Grid {
      val width = states.length
      val height = states(0).length
      val self = states
    }
  }

  case class Cell(x:Int, y:Int, state:State) {
    def iterate(grid:Grid):Cell = this.copy(state = state.transform(grid, this))
  }

  class State(val color:Int) {
    /**
     * glue(this) should return a (Grid, Cell) => State
     */
    def transform(grid:Grid, cell:Cell) = glue(this)(grid, cell)
  }
  /**
   * Set of randomly generated states
   */
  val genStatePool = (0 until randi(3, 6)).map(_ => new State(randomColor)).toSet
  /**
   * All states: this is the set of randomly generated states along with the NoState
   */
  val statePool:Set[State] = genStatePool + NoState
  object State {
    /**
     * Choose a random state from the state pool
     */
    def random:State = org.zhang.lib.random(statePool)
  }
  object NoState extends State(color(0))

  /**
   * State -> (Grid, Cell) => State (RuleState)
   */
  val glue:Map[State, RuleList] = statePool.map(_ -> RuleList.random).toMap

  case class Rule(pred:(Grid, Cell) => Boolean, state:State) {
    def get(grid:Grid, cell:Cell) = if(pred(grid, cell)) Some(state) else None
  }
  object Rule {
    //basically, glue a random predicate with a random rule
    def random = Rule(randomPred, State.random)

    private def randomPred = {
      //todo: finish!
      null
    }
  }

  /**
   * Guaranteed to return a state, given a (Grid, Cell)
   */
  class RuleList(val rules:Seq[Rule]) extends ((Grid, Cell) => State) {
    def apply(grid:Grid, cell:Cell) = rules.find(_.pred(grid, cell)).map(_.state).getOrElse(cell.state)
  }
  object RuleList {
    def random = new RuleList((0 until randi(3, 6)).map(_ => Rule.random))//todo:make random
  }

  trait Predicate extends ((Grid, Cell) => Boolean) //function2
  trait PredArgTrans[ArgL, ArgR] extends Predicate { //transforms (grid, cell) => boolean into a (ArgL, ArgR) => boolean
    def left:(Grid, Cell) => ArgL
    def right:(Grid, Cell) => ArgR
    def apply(left:ArgL, right:ArgR):Boolean
    def apply(g:Grid, c:Cell) = apply(left(g, c), right(g, c))
  }
  //int, int => boolean (comparators)
  trait IntIntPred extends PredArgTrans[Int, Int]
  object IntIntPred {

  }

  trait PredRetTrans[Ret] extends (((Grid, Cell) => Boolean) => ((Grid, Cell) => Ret)) { //transforms (grid, cell) => boolean into a (grid, cell) => ret

  }


  override def setup() {
    size(500, 500)
  }

  override def draw() {


    pollSave() //check if the screen should be saved
  }

  override def keyPressed() {
    super.keyPressed(); //pressing space toggles a boolean variable to save the screen
  }
}