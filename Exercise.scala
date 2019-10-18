package calculator

import scala.collection.mutable

object Exercise  extends App {
  def handleoper(str: String,stack: mutable.Stack[Int]): Boolean=str match{
    case "+" =>
      val r=stack.pop()
      val l=stack.pop()
      stack.push(l+r)
      true
    case "-" =>
      val r=stack.pop()
      val l=stack.pop()
      stack.push(l-r)
      true
    case "*" =>
      val r=stack.pop()
      val l=stack.pop()
      stack.push(l*r)
      true
    case "/" =>
      val r=stack.pop()
      val l=stack.pop()
      stack.push(l/r)
      true
    case _ => false

  }

  def handlenum(str: String, stack: mutable.Stack[Int]): Boolean = try {
    stack.push(str.toInt)
    true
  } catch {
    case _: NumberFormatException =>false
  }

  def cal(str: String): Int ={
    val stack = new mutable.Stack[Int]
    for(token<-str.split(" ")
      if(!handleoper(token,stack)&& !handlenum(token,stack)))
        throw new IllegalArgumentException("Invalid token")
    stack.pop()
  }
  /*def main(args: Array[String]): Unit =
//    if(args.length!=1)
//      throw new IllegalArgumentException("invalid arg 1111")
//    else
      println(cal("1 1 + 2 3 * +"))*/
}
