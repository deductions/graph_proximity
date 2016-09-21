import org.scalatest.FunSuite

import scala.collection.mutable


import algorithm.graph._

import graph_proximity._

class Proximity extends FunSuite {

  test("test something") {
    
    val graph = TopKShortestPaths.importGraph("data/test_6_2")

    println("findPaths:")

    val finder = new TopKShortestPaths[Int](graph)
    finder.findPaths(4, 5, 3).foreach(println)

    println("proximity:")

    class Test extends ProximityComputations[Int] {}
    val t = new Test()
    println(t.proximity(4, 5, 1, graph))
    
    
  }

}
