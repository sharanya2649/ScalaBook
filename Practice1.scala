package calculator

import java.io.File

import scala.annotation.tailrec
import scala.collection.mutable

class Square {
  def sq(n: Int): Int = {
    var k = n * n
    k
  }
}


object Practice1 {
  def show(num: Int, l: List[Int]): List[Int] = l.flatMap(List.fill(num)(_))

  def reverse(x: Array[Int]): Array[Int] = x.reverse

  def sumf(i: Option[Int], j: Option[Int]): Option[Int] = {

    val op = Some(i.getOrElse(0) + j.getOrElse(0))
    op

  }
  def msort(list:List[Int]):List[Int]={
    def merge(l1:List[Int],l2:List[Int]):List[Int]=(l1,l2) match{
      case (Nil,_)=>l2
      case (_,Nil)=>l1
      case (x::x1,y::y1)=>
        if(x<y) x::merge(x1,l2)
        else y::merge(l1,y1)
    }
    val n=list.length/2
    if(n==0) list
    else{
      val (s1,s2)= list splitAt n
      merge(msort(s1),msort(s2))
    }

  }
  def segregate(arr:Array[Int]):Array[Int]=arr match{

  }





//  def msort[T](less: (T, T) => Boolean)(xs: List[T]): List[T] =
//  {
//    def merge(xs: List[T], ys: List[T]): List[T] =(xs, ys) match {
//      case (Nil, _) => ys
//      case (_, Nil) => xs
//      case (x :: xs1, y :: ys1) =>
//        if (less(x, y)) x :: merge(xs1, ys)
//        else y :: merge(xs, ys1)
//    }
//    val n = xs.length / 2
//    if (n == 0) xs
//    else {
//      val (ys, zs) = xs splitAt n
//      merge(msort(less)(ys), msort(less)(zs))
//    }
//  }

  def rotate(arr: Array[Int], n: Int): Unit = {
    println(arr.splitAt(0))
    val a = arr.toList.slice(0, n)

    val b = arr.toList.slice(n, arr.length)
    println(b ++ rev(a))
  }

  //  val e=new MyList(List(1,2,1,3,4))
  //  e.map()

  def rev(ls: List[Int]): List[Int] =
    ls.foldLeft(List[Int]()) { (r, h) => h :: r }

  def fill(x: Int, n: Int): mutable.MutableList[Int] = {
    val l = mutable.MutableList[Int]()

    (1 to n) foreach { i =>
      l += x //adding beginning of list
      //      l=x+:l
    }
    l

  }

  def filll(x: Int, n: Int): List[Int] = {
    //    var l = List[Int]()
    //empty list
    var l: List[Int] = List()

    (1 to n) foreach { i =>
      l = x :: l //adding beginning of list
      //      l=x+:l
    }
    l

  }

  //def countFiles(dir: String): Int = {
  //      val folder: File = new File("/home/sharanya/Downloads")
  //      def check(folder:File,res:List[File]): List[File] ={
  //        val l_files: Array[File] =folder.listFiles()
  //
  //        for(l<-l_files){
  //
  //          if(l.isDirectory){
  //            check(l,res)
  //          }
  //          else{
  //            l::res
  //          }
  //        }
  //        res
  //      }
  //  val len=check(folder,List())
  //  len.size
  //}

  def dupp(l: List[Int]): List[Int] = {
    //  var y=l.toSet
    //  var z=y.toList
    //  z

    //  l.distinct
    var l2: List[Int] = List()
    l.foreach { x =>
      if (!l2.contains(x)) {
        l2 = l2 :+ x //adding end of list
      }
    }
    l2
  }

  def mapp(str: String): List[(String, Int)] = {
    val words = str.split(" ")
    val wordscount: Map[String, Int] = words.groupBy(w => w).map { case (key, value) => (key, value.length) }
    //    wordscount.toList.sortBy{case elem=> elem._2}
    println(wordscount)
    //    wordscount.toList.sortWith((k,v)=> k._2>v._2||k._2==v._2&&k._1<v._1)
    val l = wordscount.toArray
    println(l)
    for (i <- 0 to l.length) {
      for (j <- i + 1 to l.length - 1) {
        if (l(i)._2 < l(j)._2 || l(i)._2 == l(j)._2 && l(i)._1 > l(j)._1) {
          val temp = l(i)
          l(i) = l(j)
          l(j) = temp
        }
      }
    }
    l.toList

  }

  def wordfreq(map: Map[String, Int]): Unit = {
    val e = map.toList.sortBy(_._2).toMap

    println("eee" + e)

  }

  //  def ma(str: String): Map[String, Int] = {
  //    val l = List(str)
  //    val lis = l.flatMap((x => x.split(" ")))
  //    val x = lis.map(w => (w, 1))
  //    val z = x.groupBy(_._1)
  //    val res = z.mapValues(y => {
  //      y.map(_._2).sum
  //    })
  //    res
  //  }
  def sortonce(x: Int, list: List[Int]):List[Int]=list match {
    case Nil=>List(x)
    case h::t=>
      if(h>=x) x::list
      else h::sortonce(x,t)
  }
  def sort(l: List[Int]): List[Int] =l match {
    case Nil=>Nil
    case h::t=>sortonce(h,sort(t))

    //        l.sortWith((a,b)=>a<b)
    //    for (i <- 0 to l.length) {
    //      for (j <- i + 1 to l.length - 1) {
    //        if (l(i) > l(j)) {
    //          val temp = l(i)
    //          l(i) = l(j)
    //          l(j) = temp
    //        }
    //      }
    //    }
    //    l
  }

  def removeOdd(map: Map[String, Int]): Map[String, Int] = {
    val res = map.filter((t) => t._2 % 2 == 0) //value check
    res
    //    map.filter((t) => t._1 =="hello") //key check
    //    map.filterKeys(Set("hello","are") )//key check

  }

  def removeKeys(keys: List[String], map: Map[String, Int]): Map[String, Int] = {
    map.filter { case (key, value) => !keys.contains(key) }
  }

  def concatenate(map1: Map[String, Int], map2: Map[String, Int]) = {
    //    val res2 = map1 ++ map2 //add two maps
    val res = map1 ++ map2.map { case (k, v) => k -> (v + map1.getOrElse(k, 0)) }
    println(res)
  }

  //tail recursion list
  def reverse2(list: List[Int]): Unit = {

    @tailrec def helper(result: List[Int], rest: List[Int]): List[Int] = {
      if (rest isEmpty)
        result
      else
        helper(rest.head :: result, rest.tail)
    }
    println(helper(Nil, list))

  }
  val l=List(2,5,1,8)
  println("maxxxxxxx"+l.reduceLeft((a,b)=>if(a>b)a else b))

  def part(Str: String) = {
    val s = "Newyork"
    val s1 = s.partition(_.isUpper)
    println(s1)
  }

  def countFiles(dir: String): Unit = {
    val folder = new File("/home/sharanya/Downloads")
    if (folder.exists && folder.isDirectory) {
      val e = folder.listFiles
        .toList.size
      println(e)
    }
    else
      println("no directory")

  }

  def spiltByBranch(list: List[Student]): (List[Student], List[Student], List[Student], List[Student]) = {

    //    list.foldLeft((List[Student],List[Student],List[Student],List[Student])){}
    var l1: List[Student] = List()
    var l2: List[Student] = List()
    var l3: List[Student] = List()
    var l4: List[Student] = List()

    list.map { x =>
      val branch = x.branch
      if (branch == "cs") {
        l1 = x :: l1
      }
      else if (branch == "me") {
        l2 = x :: l2
      }
      else if (branch == "ee") {
        l3 = x :: l3
      }
      else if (branch == "ce") {
        l4 = x :: l4
      }

    }
    (l1, l2, l3, l4)

  }

  // implement map using foldleft
  def mapFold(l: List[Int]): Map[String, Int] = {
    l.foldLeft(Map.empty[String, Int]) { case (map, elem) =>
      map + (elem.toString -> elem)
    }
  }

  def fuse[A, B](a: Option[A], b: Option[B]): Option[(A, B)] = {
    a.flatMap(x => b.map(y => (x, y)))

  }

  //  def fuse[A, B](a: Option[A], b: Option[B]): Option[(A, B)] =
  //    for {
  //      aNew <- a
  //      bNew <- b
  //    } yield (aNew, bNew)
  //  def compose[A,B,C](g:B=>C, f:A=> B):A=>C=
  //    a => f(g(a))
  //  def f(b: Int): Int = b / 2
  //  def g(a: Int): Int = a + 2
  //
  //  compose(f, g)(0) == compose(g, f)(0)
  //  compose(f, g)(2)
  //  compose(g, f)(2)

  def split(xs: List[Int], n: Int): List[List[Int]] = {
    if (xs.size <= n) xs :: Nil
    else (xs take n) :: split(xs drop n, n)
  }

  def window(n: Int, xs: List[Int]): List[List[Int]] = {
    //    split(xs, n)
    if (xs.isEmpty) Nil
    else (xs take n) :: window(n, xs drop n)
  }

  def myZip(xs: List[Int], ys: List[String]): List[(Int, String)] = {
    def my(xs: List[Int], ys: List[String], n: List[(Int, String)]): List[(Int, String)] = {
      if (xs.isEmpty) n
      else my(xs.tail, ys.tail, n :+ (xs.head, ys.head))
    }

    my(xs, ys, List[(Int, String)]())
  }

  def zip(list1: List[Int], list2: List[String]): List[(Int, String)] = {
    val res: List[(Int, String)] = list1 zip list2
    //        val un:(List[Int],List[String])=res.unzip
    res
  }

  def reduce(list: List[Map[String, Int]]): Map[String, Int] = {
    list.flatten.toMap
    //    list.reduce(_+_)

  }

  def reduce2(list: List[Map[String, Map[String, Int]]]): Map[String, Map[String, Int]] = {
    list.flatten.toMap
  }

  def mergee(map1: Map[String, Int], map2: Map[String, Int]): Map[String, Int] = {
    map1 ++ map2
    //    def mer(map1: Map[String, Int], map2: Map[String, Int],a:Map[String,Int]): Map[String, Int]={
    //
    //    }
    //    mer(map1,map2,Map[String,Int]())
  }

  def merge2(map1: Map[String, Map[String, Int]], map2: Map[String, Map[String, Int]]):
  Map[String, Map[String, Int]] = {
    map1 ++ map2.map { case (k, v) => k -> (v ++ map1.getOrElse(k, Nil)) }
  }

  def merge3(map1: Map[String, Map[String, Map[String, Int]]], map2: Map[String,
    Map[String, Map[String, Int]]]): Map[String, Map[String, Map[String, Int]]] = {
    map1 ++ map2.map { case (k, v) => k -> (v ++ map1.getOrElse(k, Nil)) }
  }
  def isSubString(str1: String, str2: String): Boolean = {
    str1.matches(s".*\\b$str2\\b.*")
  }
  def help2(list: List[String], head: String): Boolean = {
    val lis= list.map { x => if (x != head) isSubString(x, head) else false}
//    println(lis)
    def help3(list:List[Boolean],res:Boolean):Boolean={

      if(res==true||list.isEmpty) res
      else help3(list.tail,if(list.head==true) true else false)
    }
//    println(help3(lis,false))
    help3(lis,false)
  }
  def removeSubString(list2: List[String]): List[String] = {
    def help(list: List[String], result: List[String]): List[String] = {
      if (list.isEmpty) result
      else {
        if (help2(list2, list.head) == false) {

          help(list.tail, list.head :: result)
        }
        else help(list.tail, result)
      }

    }
    help(list2,Nil)
  }
  def main(args: Array[String]): Unit = {
    //    println("m2"+merge2(Map("hello"->Map("hi"->1)),Map("good"->Map("wer"->3),"hello"->Map("ab"->6))))
    println("remove substring="+removeSubString(List("language","scala is functional programming language", "functional programming language", "scala", "functional programming")))
    println("m3=" + merge3(Map("ab" -> Map("bc" -> Map("cd" -> 4))), Map("pq" -> Map("rs" -> Map("tu" -> 6)), "ab" -> Map("ks" -> Map("lm" -> 4)))))
    val n = 4
    val f = 3
    val x = Array.range(1, n + 1)
    println("merge sort=="+msort(List(4,3,1,2)))
//    println(msort((x: Int, y: Int) => x < y)(List(5, 7, 1, 3)))
    val x1 = List.range(1, n + 1)
    val listOfEmployees = List(Employee(1, "abc@techsophy.com", Salary(2345, 1256, 1234), 22), Employee(2, "pqr@techsophy.com", Salary(4567, 1456, 1564), 56))
    //println(listOfEmployees)
    val result = appraisal(listOfEmployees)
    //    println(result)
    println("sort list=" + sort(List(4, 2, 8, 1)))
    val st = List(Student(1, "abc", 22, "cs"), Student(2, "pqr", 22, "me"), Student(3, "avc", 22, "cs"), Student(4, "klo", 22, "me"), Student(5, "klo", 22, "ee"), Student(6, "klo", 22, "ce"))
    //    println(spiltByBranch2(st))
    println("fusee=" + fuse(Some(2), Some(4)))
    println(mapFold(List(1, 2, 3, 4, 5)))
    //    rotate(Array(1,2,3,4,5),2)
    //val comp = Company(List(Branch(List(Consultant(List(Customer(234)))))))
    //getCompanyValue(comp)

    println(Array(1, 2, 3, 4, 5))


    println(concatlisfold(List(1, 2, 3), List(4, 5, 6)))
    println(reduce(List(Map("hello" -> 5, "hi" -> 4))))
    //    println(reduce2(List(Map("hello"->Map("wer"->3),"hi"->Map("as"->6)))))
    //    println(mergee(Map("hi"->6,"wer"->3),Map("he"->2,"she"->3)))

    //    println(show(f,x1))
    //
    //    //rev list foldleft
    //    println(rev(x1))
    ////    reverse array
    //    val y=reverse(x)
    //    for(in<-y){
    //      println(in)
    //    }

    //sum using foldleft
    //    println(sumf(None,Some(2)))
    //
    //    println(filll(3,4))

    //    println(dupp(List(1,2,1,3,2,4)))

    println("map==" + mapp("hello how are you hello"))


    //    var obj=new Square()
    //    println(obj.sq(2))

    //      println(removeOdd(mapp("hello how are you hello")))

    //      println(removeKeys(List("hello","are"),Map("hello" -> 1, "how" -> 2)))

    //    println(concat(List(1,2,3),List(4,5,6)))

    //    concatenate(Map("hello" -> 1, "how" -> 2),Map("why" -> 5, "how" -> 2))

    //    println(zip(List(1,2,3),List("hello","hi")))

    //    println(merge(List(1, 3, 5), List(2, 4, 6)))

    //        println("key value sorting"+wordfreq(Map("hello" -> 1, "haw" -> 1,"are"->3)))
    //    wordfreq(Map("qer"->2,"hello" -> 1, "haw" -> 1,"are"->3))
    //
    //    reverse2(List(1,2,3,4))

    //    countFiles("home/sharanya/Downloads")
    //
    val expr = Multiply(Number(2), Multiply(Number(5), Number(2)))
    println("eval=" + eval(expr))

    //        println("window"+window(3,List(1,2,3,4,5,6)))
    //        println("myzip="+myZip(List(1,2,3),List("one","two","three")))
  }

  // implement map using foldleft
  //  def mapFold(l: List[Int]): Map[String, Int] = {
  //    l.foldLeft(Map.empty[String, Int]) { case (map, elem) =>
  //      map + (elem.toString -> elem)
  //    }
  //  }

  def merge(list1: List[Int], list2: List[Int]): List[Int] = {
    var l = list1 ::: list2
    l.sorted
  }

  def appraisal(emps: List[Employee]): List[Employee] = {
    emps.map { x =>
      val empSalary = x.salary
      val modifiedHra = if (x.age > 50) 123 else empSalary.hra
      x.copy(salary = x.salary.copy(basic = empSalary.basic + (empSalary.basic * 0.1), hra = modifiedHra))
    }
    /*val e=emps.map(x=>(x.salary.copy(basic = x.salary.basic+(x.salary.basic*0.1))))
    val f=emps.map(x=>if(x.age>50) (x.salary.copy(hra=123)))
    println(e)
    println(f)
    emps*/
  }

  def spiltByBranch2(list: List[Student]): (List[Student], List[Student], List[Student], List[Student]) = {
    (list.filter(student => student.branch == "cs"), list.filter(student => student.branch == "ee"), list.filter(student => student.branch == "ce"), list.filter(student => student.branch == "me"))
  }

  //  def eval(expr: Expr): Int = expr match {
  //    case Number(i) => i
  //    case Sum(Number(i), Number(j)) => i + j
  //    case Subtract(Number(i), Number(j)) => i - j
  //    case Multiply(Number(i), Number(j)) => i * j
  //
  //  }
  def eval(expr: Expr): Int = expr match {
    case Number(i) => i
    case Sum(expr1, expr2) => eval(expr1) + eval(expr2)
    case Subtract(expr1, expr2) => eval(expr1) - eval(expr2)
    case Multiply(expr1, expr2) => eval(expr1) * eval(expr2)

  }

  def concatlisfold(list1: List[Int], list2: List[Int]): List[Int] = {
    list2.foldLeft(list1) { (acc, ele) => acc :+ ele }
  }

  sealed trait Expr

  class MyList(list: List[Int]) {
    def map(f: Int => Int, list: List[Int]): List[Int] = list match {
      // body of method
      case Nil => Nil
      case x :: t => f(x) :: map(f, t)

    }

    println("lissssss" + list)
  }

  case class Number(i: Int) extends Expr

  case class Sum(expr1: Expr, expr2: Expr) extends Expr

  case class Subtract(expr1: Expr, expr2: Expr) extends Expr

  //
  //    def  flatMap(f: Int => List[Int]):List[Int] ={
  //      // body of method here
  //    }

  case class Employee(id: Int, email: String, salary: Salary, age: Int)

  case class Salary(basic: Double, hra: Double, ta: Double)

  case class Student(id: Int, name: String, age: Int, branch: String)


  //  case class Customer(value: Int)
  //  case class Consultant(portfolio: List[Customer])
  //  case class Branch(consultants: List[Consultant])
  //  case class Company(branches: List[Branch])
  //  def getCompanyValue(company: Company): Int = {
  //      for {
  //        company.branches
  //      } yield {
  //        //code here
  //      }
  //    valueList reduce (_ + _)
  //  }

  case class Multiply(expr1: Expr, expr2: Expr) extends Expr

}
