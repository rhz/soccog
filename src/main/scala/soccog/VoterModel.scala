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
    def isOptimal: Boolean =
      (0 until (numConcepts-2)).forall(i =>
        ((i+1) until (numConcepts-1)).forall(j =>
          ((j+1) until numConcepts).forall(k =>
            (beliefs(i)(j) * beliefs(j)(k) * beliefs(k)(i)) == 1)))
  }

  // i and j are concept indices, value can be 1 or -1
  // returns difference in energy
  // TODO: missing normalisation in cognitive energy?
  def updateBelief(x: Node, i: Int, j: Int, value: Int)
      : (Double, Double) = {
    // assume(i != j)
    // assume(!x.friends.contains(x))
    var sediff = 0.0 // social energy difference
    var cediff = 0.0 // cognitive energy difference
    for (k <- 0 until numConcepts if (k != i) && (k != j))
      cediff += x.beliefs(i)(j) * x.beliefs(j)(k) * x.beliefs(k)(i)
    for (y <- x.friends)
      sediff += x.beliefs(i)(j) * y.beliefs(i)(j)
    x.beliefs(i)(j) = value
    x.beliefs(j)(i) = value
    for (k <- 0 until numConcepts if (k != i) && (k != j))
      cediff -= x.beliefs(i)(j) * x.beliefs(j)(k) * x.beliefs(k)(i)
    for (y <- x.friends)
      sediff -= x.beliefs(i)(j) * y.beliefs(i)(j)
    (sediff * social, cediff * cognitive)
  }

  def energy: Double = {
    var e = 0.0
    for (x <- nodes)
      e += individualEnergy(x)
    e
  }

  def individualEnergy(x: Node): Double = {
    var e = 0.0
    for (i <- 0 until numConcepts; j <- (i+1) until numConcepts) {
      for (k <- (j+1) until numConcepts)
        e -= x.beliefs(i)(j) * x.beliefs(j)(k) * x.beliefs(k)(i) * cognitive
      for (y <- x.friends)
        // divided by 2, otherwise we are double counting
        // the contribution of each link
        e -= x.beliefs(i)(j) * y.beliefs(i)(j) * social / 2
    }
    e
  }

  def socialEnergy: Double = {
    var e = 0.0
    for (x <- nodes)
      for (i <- 0 until numConcepts; j <- (i+1) until numConcepts)
        for (y <- x.friends)
          // divided by 2, otherwise we are double counting
          // the contribution of each link
          e -= x.beliefs(i)(j) * y.beliefs(i)(j) * social / 2
    e
  }

  def cognitiveEnergy: Double = {
    var e = 0.0
    for (x <- nodes)
      for (i <- 0 until numConcepts; j <- (i+1) until numConcepts)
        for (k <- (j+1) until numConcepts)
          e -= x.beliefs(i)(j) * x.beliefs(j)(k) * x.beliefs(k)(i) * cognitive
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

  def step(rnd: Random): (Double, Double) = {
    // pick a node and a belief at random
    val x = nodes(rnd.nextInt(numNodes))
    val i = rnd.nextInt(numConcepts)
    var j = rnd.nextInt(numConcepts)
    while (j == i) j = rnd.nextInt(numConcepts)
    val w = x.beliefs(i)(j)
    val u = rnd.nextInt(2)
    val v = if (u == w) -1 else u
    val (sediff, cediff) = updateBelief(x, i, j, v)
    val energydiff = sediff + cediff
    val q = scala.math.exp(-energydiff / temperature)
    if (energydiff <= 0 || rnd.nextDouble <= q) (sediff, cediff)
    else { updateBelief(x, i, j, w); (0.0, 0.0) } // backtrack
  }

  def stepDiff(rnd: Random): (Node, Int, Int, Int, Int) = {
    // pick a node and a belief at random
    val x = nodes(rnd.nextInt(numNodes))
    val i = rnd.nextInt(numConcepts)
    var j = rnd.nextInt(numConcepts)
    while (j == i) j = rnd.nextInt(numConcepts)
    val w = x.beliefs(i)(j)
    val u = rnd.nextInt(2)
    val v = if (u == w) -1 else u
    val (sediff, cediff) = updateBelief(x, i, j, v)
    val energydiff = sediff + cediff
    val q = scala.math.exp(-energydiff / temperature)
    if (energydiff > 0 && rnd.nextDouble > q)
      updateBelief(x, i, j, w) // backtrack
    (x, i, j, w, v)
  }

  def stepDiffAndEnergy(rnd: Random): (Node, Int, Int, Int, Int, Double, Double) = {
    // pick a node and a belief at random
    val x = nodes(rnd.nextInt(numNodes))
    val i = rnd.nextInt(numConcepts)
    var j = rnd.nextInt(numConcepts)
    while (j == i) j = rnd.nextInt(numConcepts)
    val w = x.beliefs(i)(j)
    val u = rnd.nextInt(2)
    val v = if (u == w) -1 else u
    val (sediff, cediff) = updateBelief(x, i, j, v)
    val energydiff = sediff + cediff
    val q = scala.math.exp(-energydiff / temperature)
    if (energydiff <= 0 || rnd.nextDouble <= q)
      (x, i, j, w, v, sediff, cediff)
    else {
      updateBelief(x, i, j, w) // backtrack
      (x, i, j, w, v, 0.0, 0.0)
    }
  }
}

object VoterModel {

  // model parameters
  val numReps = 1000
  val numSteps = 100000
  val meanDegree = 10.0
  var cognitive = 2.5 // 3.75 for equal social and cognitive
                      // minimum energy (see google doc)
  val social = 1.0
  val temperature = 1.0

  def runOnceEnergy: Unit = {
    val m = new VoterModel(social, cognitive, temperature)
    // create random initial graph
    val rnd = new Random()
    // about 10 friends per node
    m.randomised(meanDegree / m.numNodes, rnd)
    // track social and cognitive energy
    var se = m.socialEnergy
    var ce = m.cognitiveEnergy
    println(s"# I=$social J=$cognitive T=$temperature <d>=$meanDegree")
    println("time total soc cog")
    // run simulation
    for (sc <- 0 until numSteps) {
      println(s"$sc ${se+ce} $se $ce")
      val (sediff, cediff) = m.step(rnd)
      se += sediff
      ce += cediff
    }
    println(s"$numSteps ${se+ce} $se $ce")
  }

  def runManyEnergy: Unit = {
    println(s"# I=$social T=$temperature <d>=$meanDegree")
    print("J \"mean final social energy\" \"standard deviation social energy\" ")
    print("\"mean final cognitive energy\" \"standard deviation cognitive energy\" ")
    println("\"mean final total energy\" \"standard deviation total energy\"")
    // List(2.0, 2.1, 2.2, 2.3, 2.4, 2.41, 2.42, 2.43, 2.44, 2.45, 2.46, 2.47, 2.48, 2.49,
    // 2.5, 2.51, 2.52, 2.53, 2.54, 2.55, 2.56, 2.57, 2.58, 2.59, 2.6, 2.7, 2.8, 2.9, 3.0)
    // for (c <- (2.0 to 2.31 by .1) ++ (2.4 to 2.601 by .01) ++ (2.7 to 3.1 by 0.1)) {
    for (c <- 2.61 to 2.691 by .01) {
      cognitive = c
      // sum of the energies and their squares
      // to compute the mean and standard deviation
      var sum = 0.0
      var sqsum = 0.0
      var ssum = 0.0
      var sqssum = 0.0
      var csum = 0.0
      var sqcsum = 0.0
      for (n <- 1 to numReps) {
        val m = new VoterModel(social, cognitive, temperature)
        // create random initial graph
        val rnd = new Random()
        // about 10 friends per node
        m.randomised(meanDegree / m.numNodes, rnd)
        // run simulation
        for (sc <- 0 until numSteps) m.step(rnd)
        // accumulate the energies
        val se = m.socialEnergy
        ssum += se
        sqssum += se*se
        val ce = m.cognitiveEnergy
        csum += ce
        sqcsum += ce*ce
        val e = se+ce
        sum += e
        sqsum += e*e
      }
      val n = numReps
      def sd(sum: Double, sqsum: Double): Double =
        scala.math.sqrt((sqsum/n)-((sum/n)*(sum/n)))
      // TODO: print theoretical maximum as well or reach/max
      print(s"$cognitive ${ssum/n} ${sd(ssum, sqssum)} ${csum/n}")
      println(s" ${sd(csum, sqcsum)} ${sum/n} ${sd(sum, sqsum)}")
    }
  }

  def runOpt: Unit = {
    val m = new VoterModel(social, cognitive, temperature)
    // create random initial graph
    val rnd = new Random()
    // about 10 friends per node
    m.randomised(meanDegree / m.numNodes, rnd)
    for (sc <- 0 until numSteps) {
      m.step(rnd)
      for (x <- m.nodes if x.isOptimal) yield x
    }
  }

  def runMatrix: Unit = {
    for (social <- List(0.001, 0.01, 0.1, 1, 10)) {
      println(
        (for (temperature <- List(0.001, 0.01, 0.1, 1, 10)) yield {
          var sum = 0.0
          for (n <- 1 to numReps) {
            val m = new VoterModel(social, cognitive, temperature)
            // create random initial graph
            val rnd = new Random()
            // about 10 friends per node
            m.randomised(meanDegree / m.numNodes, rnd)
            // run simulation
            for (sc <- 0 until numSteps) m.step(rnd)
            sum += m.energy
          }
          sum / numReps
        }).mkString(" "))
    }
  }

  def main(args: Array[String]): Unit = {
    runManyEnergy
  }
}

