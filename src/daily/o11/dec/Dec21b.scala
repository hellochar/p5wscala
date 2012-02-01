package daily


import org.zhang.lib.MyPApplet
import java.util.Date

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 12/21/11
 * Time: 9:56 PM
 */

class Dec21b extends MyPApplet with Savable {

  case class Entry(transType:String, date:Date, amt:Float, fitid:String, name:String, memo:String)
  object Entry {
    def apply(t:String, dateMillis:String, amt:String, fitid:String, name:String, memo:String):Entry = apply(
      t,
      new Date(dateMillis.toLong),
      amt.toFloat,
      fitid,
      name,
      memo
    )
  }

  override def setup() {
    size(500, 500)
    val Regex = """<STMTTRN><TRNTYPE>(\w*)<DTPOSTED>(\w*)<TRNAMT>([-+]?[0-9]*\.?[0-9]+)<FITID>(\w*)<NAME>([\w\s-]*)<MEMO>([\w\s]*)</STMTTRN>""".r
    loadStrings("l90ofxdl.ofx") foreach { _ match {
        case Regex(transType, date, amt, fitid, name, memo) => Some(Entry(transType, date, amt, fitid, name, memo))
        case k => {
          println("Couldn't match "+k+"!"); None
        }
      }
    }
  }

  override def draw() {

  }
}