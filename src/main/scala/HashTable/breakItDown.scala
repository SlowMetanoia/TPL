package HashTable

import java.time.LocalTime

object breakItDown extends App{
  
  var curTime: Long = 0
  def tick( ): Unit = curTime = LocalTime.now().toNanoOfDay
  def tack = LocalTime.now().toNanoOfDay - curTime
  
  val c = OpenTable[Any,String]()
  c.insert(1->"a")
  c.insert(1->"a")
  c.insert(1->"a")
  c.insert(1->"a")
  c.insert(1->"b")
  
  (1 to 10).foreach(i=>c.insert(i->i.toString))
  c.insert(97,"9797")
  c.insert("a","979797")
  println(c.lookUp(97))
  println(c.lookUp("a"))
  println(c)
  println("-"*80)
  /*c.insert(3->"c")
  println(c.lookUp(1))
  println(c.lookUp(2))
  println(c.lookUp(3))*/
  val c1 =  OpenTable[String,Int]()
  var cMap = Map[String,Int]()
  def testStream:Int=>LazyList[Int] = i=> i#::testStream(i+1)
  tick()
  testStream(0).map(_.toString)
               .zipWithIndex
               .take(10)
               .foreach(cMap += _)
  println(s"took $tack for chain")
  
  tick()
  testStream(0).map(_.toString)
               .zipWithIndex
               .take(10)
               .foreach(c1.insert)
  println(s"took $tack for open")
}
