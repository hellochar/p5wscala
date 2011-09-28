package daily

import scala.util.continuations._

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 7/28/11
 * Time: 1:50 AM
 */

object Continuations extends Application {

//  reset {
//    // A
//    shift { cf: (Int=>Int) =>
//    // B
//      val eleven = cf(10)
//      // E
//      println(eleven)
//      val oneHundredOne = cf(100)
//      // H
//      println(oneHundredOne);
//      34
//    }
//    // C execution continues here with the 10 as the context
//    // F execution continues here with 100
//    + 1
//    // D 10.+(1) has been executed - 11 is returned from cf which gets assigned to eleven
//    // G 100.+(1) has been executed and 101 is returned and assigned to oneHundredOne
//  }

//  val v = reset {
//    shift { k:(Int => Int) =>
//      k(7)
//    } + 1
//  }

//  val v = reset {
//    //A
//    println("A")
//    val s = shift { k:(Int => Int) =>
//      //B
//      println("B")
//      println(k(7))
//      println(k(23))
//      println(k(90))
//      //D
//      println("D")
//      "HELLO" //RETURN VALUE OF RESET
//    }
//
//    //C
//    println(s+": C")
//    s + 6 //RETURN VALUE OF K
//  }
//  println("E")

  val v = reset {
    println("A")
    def inner(i:Int) = shift {k:(Int => Int) =>
      println("C")
      println("i: "+i)
      val list = (0 until 5).map(x => k(i*x))
      println(list.mkString)
      list
    }
    println("B")
    val k = inner(7) * 2
    println("D")
    k
//    val l = inner(4) * 5
//    l
  }
  println(v)

}