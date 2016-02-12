package uk.ac.ed.inf
package soccog

import scala.collection.mutable
import scala.util.Random

class VoterModel(
  val social: Double = 1.0,
  val cognitive: Double = 1.0,
  val temperature: Double = 1.0,
  val numNodes: Int = 100,
  val numConcepts: Int = 10) {

  val nodes: Array[Node] = Array.fill(numNodes)(new Node)

  class Node {
    val friends = mutable.Set.empty[Node]
    val beliefs: Array[Array[Int]] =
      Array.fill(numConcepts, numConcepts)(0)
    def befriend(x: Node) = { friends += x; x.friends += this; this }
    // only 0->(+-1) can create a cycle and the reverse destroy one
    // (-1)->1 keeps the number of cycles and makes
    // all the stable ones unstable and vice versa
    // so we just need to count the number of stable and unstable
    // cycles in a (-1)->1 transition (and its inverse) to know the
    // delta energy due to cognitive factors.
  }

  // i and j are concept indices, value can be 1 or -1
  // returns difference in energy
  // TODO: missing normalisation in cognitive energy
  def addBelief(x: Node, i: Int, j: Int, value: Int): Double = {
    // assume(i != j)
    // assume(x.beliefs(i)(j) == 0)
    // assume(!x.friends.contains(x))
    x.beliefs(i)(j) = value
    x.beliefs(j)(i) = value
    var diffenergy = 0.0
    for (k <- 0 until numConcepts if (k != i) && (k != j))
      diffenergy -= x.beliefs(i)(j) * x.beliefs(j)(k) * x.beliefs(k)(i) * cognitive
    for (y <- x.friends)
      diffenergy -= x.beliefs(i)(j) * y.beliefs(i)(j) * social
    diffenergy
  }

  def removeBelief(x: Node, i: Int, j: Int): Double = {
    // assume(i != j)
    // assume(x.beliefs(i)(j) == 1 || x.beliefs(i)(j) == -1)
    // assume(!x.friends.contains(x))
    var diffenergy = 0.0
    for (k <- 0 until numConcepts if (k != i) && (k != j))
      diffenergy += x.beliefs(i)(j) * x.beliefs(j)(k) * x.beliefs(k)(i) * cognitive
    for (y <- x.friends)
      diffenergy += x.beliefs(i)(j) * y.beliefs(i)(j) * social
    x.beliefs(i)(j) = 0
    x.beliefs(j)(i) = 0
    diffenergy
  }

  def changeBelief(x: Node, i: Int, j: Int): Double = {
    // assume(i != j)
    // assume(x.beliefs(i)(j) == 1 || x.beliefs(i)(j) == -1)
    // assume(!x.friends.contains(x))
    x.beliefs(i)(j) *= -1
    x.beliefs(j)(i) *= -1
    var diffenergy = 0.0
    for (k <- 0 until numConcepts if (k != i) && (k != j))
      diffenergy -= x.beliefs(i)(j) * x.beliefs(j)(k) * x.beliefs(k)(i) * 2 * cognitive
    for (y <- x.friends)
      diffenergy -= x.beliefs(i)(j) * y.beliefs(i)(j) * 2 * social
    diffenergy
  }

  // i and j are concept indices, value can be 1 or -1
  // returns difference in energy
  // TODO: missing normalisation in cognitive energy
  def updateBelief(x: Node, i: Int, j: Int, value: Int): Double = {
    // assume(i != j)
    // assume(!x.friends.contains(x))
    var diffenergy = 0.0
    for (k <- 0 until numConcepts if (k != i) && (k != j))
      diffenergy += x.beliefs(i)(j) * x.beliefs(j)(k) * x.beliefs(k)(i)
    for (y <- x.friends)
      diffenergy += x.beliefs(i)(j) * y.beliefs(i)(j)
    x.beliefs(i)(j) = value
    x.beliefs(j)(i) = value
    for (k <- 0 until numConcepts if (k != i) && (k != j))
      diffenergy -= x.beliefs(i)(j) * x.beliefs(j)(k) * x.beliefs(k)(i)
    for (y <- x.friends)
      diffenergy -= x.beliefs(i)(j) * y.beliefs(i)(j)
    diffenergy
  }

  def energy: Double = {
    var e = 0.0
    for (x <- nodes) {
      for (i <- 0 until numConcepts; j <- (i+1) until numConcepts) {
        for (k <- (j+1) until numConcepts)
          e -= x.beliefs(i)(j) * x.beliefs(j)(k) * x.beliefs(k)(i) * cognitive
        for (y <- x.friends)
          // divided by 2, otherwise we are double counting
          // the contribution of each link
          e -= x.beliefs(i)(j) * y.beliefs(i)(j) * social / 2
      }
    }
    e
  }

  def randomised(linkProb: Double, rnd: Random): Unit = {
    for (x <- nodes; y <- nodes if (x != y) && (rnd.nextDouble < linkProb))
      x befriend y
    for (x <- nodes; i <- 0 until numConcepts; j <- (i+1) until numConcepts) {
      x.beliefs(i)(j) = rnd.nextInt(3) - 1
      x.beliefs(j)(i) = x.beliefs(i)(j)
    }
  }

  def step(rnd: Random): Double = {
    // pick a node and a belief at random
    val x = nodes(rnd.nextInt(numNodes))
    val i = rnd.nextInt(numConcepts)
    var j = rnd.nextInt(numConcepts)
    while (j == i) j = rnd.nextInt(numConcepts)
    val w = x.beliefs(i)(j)
    val u = rnd.nextInt(2)
    val v = if (u == w) -1 else u
    val diffenergy = updateBelief(x, i, j, v)
      // if (v == 0) removeBelief(x, i, j)
      // else if (w == 0) addBelief(x, i, j, v)
      // else changeBelief(x, i, j)
    // val d = energy
    // require(d == e + diffenergy, s"$d != $e + $diffenergy")
    val q = scala.math.exp(-diffenergy / temperature)
    if (diffenergy < 0 || rnd.nextDouble < q) diffenergy
    else { updateBelief(x, i, j, w); 0.0 } // backtrack
    //   if (v == 0) addBelief(x, i, j, w)
    //   else if (w == 0) removeBelief(x, i, j)
    //   else changeBelief(x, i, j)
    // }
    // require(e == energy)
  }
}

object VoterModel {

  def main(args: Array[String]): Unit = {
    val numReps = 1 // 2000
    val numSteps = 100000
    // var sum = 0.0
    // var squaresum = 0.0
    for (n <- 1 to numReps) yield {
      val m = new VoterModel()
      // create random initial graph
      val rnd = new Random()
      m.randomised(10.0 / m.numNodes, rnd) // about 10 friends per node?
      // observables
      var e = m.energy
      // run simulation
      for (sc <- 0 until numSteps) {
        println(s"$sc $e")
        e += m.step(rnd)
      }
      println(s"$numSteps $e")
      // sum += e
      // squaresum += e*e
      // println(s"$n ${scala.math.sqrt((squaresum/n)-(sum/n))}")
    }
  }
}

