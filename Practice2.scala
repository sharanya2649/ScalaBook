package com.techsophy.excercise

import java.io.File

import scala.annotation.tailrec
import scala.collection.immutable.ListMap

class Practice2 {

  def concatenate(map1: Map[String, Int], map2: Map[String, Int]): Map[String, Int] = {
    //    val res2 = map1 ++ map2 //add two maps
    val res: Map[String, Int] = map1 ++ map2.map { case (k, v) => k -> (v + map1.getOrElse(k, 0)) }
    res

  }

  def revRec(lis: List[Int]): List[Int] = {
    @tailrec def help(result: List[Int], rest: List[Int]): List[Int] = {
      if (rest isEmpty)
        result
      else
        help(rest.head :: result, rest.tail)
    }

    val result = help(Nil, lis)
    result
  }

  def maxLis(l: List[Int]): Int = {
    l.foldLeft(0) { (acc, ele) => if (ele > acc) ele else acc }
  }

  def show(i: Int, n: Int): List[Int] = {
    val l = List.range(1, n + 1)
    l.flatMap(List.fill(i)(_))
  }

  //  rotate array
  def rotate(arr: Array[Int], n: Int): List[Int] = {
    val a = arr.toList.slice(0, n)

    val b = arr.toList.slice(n, arr.length)
    b ++ rev(a)

  }
  def wordCount(text: String): Map[String, Int] = {
    if (text.isEmpty) {
      Map()
    }
    val words: Array[String] = text.split(" ")
    val wordCounts: Map[String, Int] =
      words
        .groupBy(word => word)
        .map { case (key, value) => (key, value.length) }
    wordCounts
  }

  //rev list using foldleft
  def rev(lis: List[Int]): List[Int] = {
    lis.foldLeft(List[Int]()) { (r, h) => h :: r }
  }

  def sumf(i: Option[Int], j: Option[Int]): Option[Int] = {

    val op = Some(i.getOrElse(0) + j.getOrElse(0))
    op

  }

  def dup(lis: List[Int]): List[Int] = {
    lis.foldLeft(List[Int]()) {
      case (acc, ele) if acc.contains(ele) => acc
      case (acc, ele) => ele :: acc
    }
  }

  def countFiles(dir: String): Int = {
    val folder = new File("/home/sharanya/Downloads")
    if (folder.exists && folder.isDirectory) {
      val e = folder.listFiles
        .toList.size
      e
    }
    else
      0
  }

  def removeKeys(keys: List[String], map: Map[String, Int]): Map[String, Int] = {
    map.filter { case (key, value) => !keys.contains(key) }
  }

  def concatList(list1: List[Int], list2: List[Int]): List[Int] = {
    list2.foldLeft(list1) { (acc, ele) => acc :+ ele }
  }

  def zip(list1: List[Int], list2: List[String]): List[(Int, String)] = {
    val res: List[(Int, String)] = list1 zip list2
    //        val un:(List[Int],List[String])=res.unzip
    res
  }

  // Any sort implement in Scala
  def mergeSort(list1: List[Int], list2: List[Int]): List[Int] = {
    val a = list1 ::: list2
    val l=a.toArray
    for(i<-0 to l.length){
      for(j<- i+1 to l.length-1){
        if(l(i)>l(j)){
          val temp=l(i)
          l(i)=l(j)
          l(j)=temp
        }
      }
    }
    l.toList
  }

  def appraisal(emps: List[Employee]): List[Employee] = {
    emps.map { x =>
      val empSalary = x.salary
      val modifiedHra = if (x.age > 50) (empSalary.hra * 0.1) else empSalary.hra
      x.copy(salary = x.salary.copy(basic = empSalary.basic + (empSalary.basic * 0.1), hra = modifiedHra))
    }
  }

  def spiltByBranch(list: List[Student]): (List[Student], List[Student], List[Student], List[Student]) = {
    (list.filter(student => student.branch == "cs"),
      list.filter(student => student.branch == "ee"),
      list.filter(student => student.branch == "ce"),
      list.filter(student => student.branch == "me"))
  }

  case class Employee(id: Int, email: String, salary: Salary, age: Int)

  case class Salary(basic: Double, hra: Double, ta: Double)

  case class Student(id: Int, name: String, age: Int, branch: String)

  class Square {
    def sq(n: Int): Int = {
      val k = n * n
      k
    }
  }


}
/*
//
//package com.techsophy.excercise
//
//import org.scalatest.FunSuite
//
//
//class Exercise01Test extends FunSuite {
//
//  val exercise = new Exercise01()
//  val exercise2 = new Exercise02()
//
//  test("Verify word count") {
//    val wordCounts = exercise.wordCount("hello how are you hello")
//    val expectedResult1 = Map("are" -> 1, "you" -> 1, "how" -> 1, "hello" -> 2)
//    assert(wordCounts == expectedResult1)
//  }
//  test("Verify rev list") {
//    val reverse = exercise.rev(List(1, 2, 3, 4))
//    val expect1 = List(4, 3, 2, 1)
//    assert(reverse == expect1)
//  }
//
//  test("Verify concatenate") {
//    val concat = exercise.concatenate(Map("hello" -> 2, "hi" -> 3), Map("hello" -> 2, "wer" -> 1))
//    val expect2 = Map("hello" -> 4, "hi" -> 3, "wer" -> 1)
//    assert(concat == expect2)
//  }
//  test("Verify rev rec") {
//    val rev = exercise.revRec(List(1, 2, 3, 4))
//    val expect3 = List(4, 3, 2, 1)
//    assert(rev == expect3)
//  }
//  test("Verify max list") {
//    val max=exercise.maxLis(List(6,2,8,1))
//    val exp1=8
//    assert(max==exp1)
//  }
//  test("show fun"){
//    val lis=exercise.show(3,4)
//    val rlis=List(1,1,1,2,2,2,3,3,3,4,4,4)
//    assert(lis==rlis)
//  }
//  test("rotate array"){
//    val rot=exercise.rotate(Array(1,2,3,4,5),2)
//    val res=List(3,4,5,1,2)
//    assert(rot==res)
//  }
//  test("sum function"){
//    val sum=exercise.sumf(None,Some(2))
//    val res=2
//    assert(sum==res)
//  }
//  test("duplicates in list"){
//    val lis=exercise.dup(List(1,2,3,1,2,4))
//    val res=List(1,2,3,4)
//    assert(lis==res)
//  }
//
//  test("Implicit square class"){
//    val obj=new exercise.Square()
//    val square=obj.sq(2)
//    val res=4
//    assert(square==res)
//  }
//  test("count files in directory"){
//    val files=exercise.countFiles("home/sharanya/Downloads")
//    val res=4
//    assert(files==res)
//  }
//  test("Remove keys"){
//    val keys=exercise.removeKeys(List("hello","are"),Map("hello" -> 1, "how" -> 2))
//    val res=Map("how" -> 2)
//    assert(keys==res)
//  }
//  test("concat list using foldLeft"){
//    val lis=exercise.concatList(List(1,2,3),List(4,5,6))
//    val res=List(1,2,3,4,5,6)
//    assert(lis==res)
//  }
//  test("zip lists"){
//    val lis=exercise.zip(List(1,2,3),List("hello","hi"))
//    val res=List((1,"hello"), (2,"hi"))
//    assert(lis==res)
//  }
//  test("merge sort list"){
//    val lis=exercise.mergeSort(List(1,3,5),List(2,4,6))
//    val res=List(1, 2, 3, 4, 5, 6)
//    assert(lis==res)
//  }
//  test("appraisal of employee"){
//    val listOfEmployees = List(exercise.Employee(1,"abc@techsophy.com",exercise.Salary(2345,1256,1234),22),exercise.Employee(2,"pqr@techsophy.com",exercise.Salary(4567,1456,1564),56))
//    val result = exercise.appraisal(listOfEmployees)
//    val res=List(exercise.Employee(1,"abc@techsophy.com",exercise.Salary(2579.5,1256.0,1234.0),22), exercise.Employee(2,"pqr@techsophy.com",exercise.Salary(5023.7,123.0,1564.0),56))
//  }
//  test("split by branch"){
//    val st=List(exercise.Student(1,"abc",22,"cs"),exercise.Student(2,"pqr",22,"me"),exercise.Student(3,"avc",22,"cs"),exercise.Student(4,"klo",22,"me"),exercise.Student(5,"klo",22,"ee"),exercise.Student(6,"klo",22,"ce"))
//    val split=exercise.spiltByBranch(st)
//    val res=(List(exercise.Student(3,"avc",22,"cs"), exercise.Student(1,"abc",22,"cs")),List(exercise.Student(4,"klo",22,"me"), exercise.Student(2,"pqr",22,"me")),List(exercise.Student(5,"klo",22,"ee")),List(exercise.Student(6,"klo",22,"ce")))
//  }
//  test("exer"){
//    val l=
//  }
//
//}
*/

