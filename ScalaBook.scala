package calculator

import scala.collection.mutable
class ScalaBook{
  private val sum=0
  //A method that is executed only for its side effects is known as a procedure.
  def add(n:Int)={
//    n=1 //method parameters are val
    val k=sum //we can access private in same class
    val s=6;println(s) //scala splits into multiple lines
  }
}

// Scala’s two namespaces are:
// •  values (fields, methods, packages, and singleton objects)
// •  types (class and trait names)

// same name for method and field (both are in same namespace) so you can override a parameterless method with a val
//class WontCompile{
//  def f()=println("hi")
//  val f="hi"
//}

abstract class Element{
  def demo=println("element")

}
class ArrayElement extends Element{
  override def demo=println("Array element")
}
class LineElement extends Element{
  override def demo: Unit = println("Line element")
}
class UniformElement extends Element

object ScalaBook {
  val hashSet: mutable.Set[String] =mutable.HashSet("scala","language")
  println(hashSet+("program"))
  println(hashSet("scala"))
  def main(args: Array[String]): Unit = {
    val acc=new ScalaBook
    acc.add(4)
    println(acc.sum) //we cant access private but we have access because class and object names are same(companion object)-opposite(standalone object)
    //A class and its companion object can access each other’s private members.
    //One difference between classes and singleton objects is that singleton objects cannot take parameters, whereas classes can.
    //raw string
    println( """hello good morning.
               welcome to scala""")
    println("""hello good morning.
      |welcome to scala""".stripMargin)
    //Symbol literal- symbols are interned
    val s: Symbol ='aSymbol
    val k='aSymbol
    println(s==k)
    println(s.name)
    //string interpolator
    val name="Reader"
    println(s"hello $name!")
    println(s"the sum is ${6+4}.")
    println(raw"No\\\\escape") //same like s but raw doesn't identifies character literals(\\)
    println("\\")
    println("\'\"")
    println(f"${10000/3}%.3f")  //like printf style

    //Int contains several overloaded+methods
    val plus: Long =1+2L
    println(plus)
    val str="Hello world"
    //What makes a method an operator is how you use it
    println(str.indexOf('o')) //method
    println(str indexOf 'o') //here indexOf is operator
    println(str.indexOf('o',5))

    val lower: String =str.toLowerCase() //parentheses is kept if method has side effects(println()), if not no need of it.
    //postfix operators don't take arguments
    println(lower)
    //unary prefix operators
    val neg=5+ -8
    val neg2=5+ (8).unary_-
    println(neg2)

    def salt()={println("salt");false}
    def pepper()={println("pepper");true}
    println(salt()&pepper()) //& checks both sides
    println(salt()&&pepper()) // && checks left side and gives res

    //partially applied function
    def sum(a:Int,b:Int,c:Int)= a+b+c
    val n=sum _
    println(n(1,2,3))

    val n2=sum(1, _:Int,3)
    println(n2(5))

    //currying
    def sumCurry(x:Int)(y:Int)=x+y
    println(sumCurry(2)(3))

    abstract class fruit{
      val v:String
      def m:String
    }
    abstract  class Apple extends fruit {
      val v:String
      val m:String //can override def with val
    }
    abstract class BadApple extends fruit {
//      def v:String // can't override def with val
      def m:String
    }
    def removeDuplicates(list: List[String]):List[String]={
      if(list.isEmpty) list
      else{
        list.head:: removeDuplicates(list.tail.filter(x=>x!=list.head))
      }
    }

    case class Person(name: String,isMale: Boolean,children: Person*)
    val lara = Person("Lara", false)
    val bob = Person("Bob", true)
    val julie = Person("Julie", false, lara, bob)
    val persons = List(lara, bob, julie)
    println(persons.filter{p=> !p.isMale}.flatMap{x=>(x.children.map{c=>(x.name,c.name)})})

    case class Book(title:String,author:String*)
    val books=List(Book("The java language Program","Gosling","Joy Bill"),Book("Elements of ML","Ullman","Joy Bill"),Book("Programming in Modula-2","Wirth, Niklaus"))
    println(for(b<-books if(b.title.indexOf("Program")>=0)) yield b.title)

    val lis = for (b1 <- books; b2 <- books if b1 != b2;a1 <- b1.author; a2 <- b2.author if a1 == a2)yield a1
    println(lis)
    println(removeDuplicates(lis))


  }

}
