package calculator

import scala.concurrent._
import scala.util._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._

object Future_prac extends App {
  val fut: Future[Int] =Future{20}
  val result: Future[Int] = fut.map(x=>x+1)
  println("hello")
  val fut2 = Future { Thread.sleep(10000); 21 + 21 }
  val result2: Future[Int] = fut2.map(x=>x+1)
  val valid=fut.filter{a=>a>0}
  val valid2=fut.collect{case a if a>0 =>a+6}
  //blocking
  Await.result(result.map(a => println(a)),Duration.Inf )
  Await.result(valid.map(a=>println(a)),Duration.Inf)
  Await.result(valid2.map(a=>println(a)),Duration.Inf)

  val fail=Future{41/0}
  val expectfail=fail.failed
  val fallback=fail.fallbackTo(Future { val res = 42; require(res < 0); res })
  val recover=fallback recover {case ex:ArithmeticException=> -1}
  val recoverWith=fallback recoverWith {case ex:ArithmeticException=> Future{60}}
  Await.result(expectfail.map(a=>println(a)),Duration.Inf)
  Await.result(recoverWith.map(a=>println(a)),1 second)







  //callback-result type: Unit-executed eventually
  fut.onComplete {
    case Success(value) => println(value)
    case Failure(e) => e.getMessage
  }

  val l1=List(6,-1,2)
  val l2=List(8,-3,-4,6,2)
  val lis=List()
  val ch =l1.flatMap{ t=>l2.filter(x=>x==t).map(t2=>t2::lis)}.flatten
  println(ch)
}
