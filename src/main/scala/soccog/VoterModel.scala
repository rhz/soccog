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
    def cognitiveEnergy: Double = {
      var e = 0.0
      for (i <- 0 until numConcepts; j <- (i+1) until numConcepts) {
        for (k <- (j+1) until numConcepts)
          e -= beliefs(i)(j) * beliefs(j)(k) * beliefs(k)(i) * cognitive
      }
      e
    }
    def socialEnergy: Double = {
      var e = 0.0
      for (i <- 0 until numConcepts; j <- (i+1) until numConcepts)
        for (y <- friends)
          e -= beliefs(i)(j) * y.beliefs(i)(j) * social / 2
      e
    }
    def energy: Double = {
      var e = 0.0
      for (i <- 0 until numConcepts; j <- (i+1) until numConcepts) {
        for (k <- (j+1) until numConcepts)
          e -= beliefs(i)(j) * beliefs(j)(k) * beliefs(k)(i) * cognitive
        for (y <- friends)
          e -= beliefs(i)(j) * y.beliefs(i)(j) * social / 2
      }
      e
    }
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

  def cognitiveEnergy: Double =
    (for (x <- nodes) yield x.cognitiveEnergy).sum

  def socialEnergy: Double =
    (for (x <- nodes) yield x.socialEnergy).sum

  def energy: Double =
    (for (x <- nodes) yield x.energy).sum

  def randomised(linkProb: Double, rnd: Random): Unit = {
    for (x <- nodes; y <- nodes if (x != y) && (rnd.nextDouble < linkProb))
      x befriend y
    for (x <- nodes; i <- 0 until numConcepts; j <- (i+1) until numConcepts) {
      x.beliefs(i)(j) = rnd.nextInt(3) - 1
      x.beliefs(j)(i) = x.beliefs(i)(j)
    }
  }

  // pick a node and a belief at random
  def randomBeliefChange(rnd: Random): (Node, Int, Int, Int, Int) = {
    val x = nodes(rnd.nextInt(numNodes))
    val i = rnd.nextInt(numConcepts)
    var j = rnd.nextInt(numConcepts)
    while (j == i) j = rnd.nextInt(numConcepts)
    val w = x.beliefs(i)(j)
    val u = rnd.nextInt(2)
    val v = if (u == w) -1 else u
    (x, i, j, w, v)
  }

  def step(rnd: Random): (Double, Double) = {
    val (x, i, j, w, v) = randomBeliefChange(rnd)
    val (sediff, cediff) = updateBelief(x, i, j, v)
    val energydiff = sediff + cediff
    val q = scala.math.exp(-energydiff / temperature)
    if (energydiff <= 0 || rnd.nextDouble <= q) (sediff, cediff)
    else { updateBelief(x, i, j, w); (0.0, 0.0) } // backtrack
  }

  def stepDiff(rnd: Random): (Node, Int, Int, Int, Int) = {
    val (x, i, j, w, v) = randomBeliefChange(rnd)
    val (sediff, cediff) = updateBelief(x, i, j, v)
    val energydiff = sediff + cediff
    val q = scala.math.exp(-energydiff / temperature)
    if (energydiff > 0 && rnd.nextDouble > q)
      updateBelief(x, i, j, w) // backtrack
    (x, i, j, w, v)
  }

  def stepDiffAndEnergy(rnd: Random): (Node, Int, Int, Int, Int, Double, Double) = {
    val (x, i, j, w, v) = randomBeliefChange(rnd)
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
    // for (c <- (2.0 to 2.31 by .1) ++ (2.4 to 2.601 by .01) ++ (2.7 to 3.01 by 0.1)) {
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

  def runOnceOpt: Unit = {
    val m = new VoterModel(social, cognitive, temperature)
    // create random initial graph
    val rnd = new Random()
    // about 10 friends per node
    m.randomised(meanDegree / m.numNodes, rnd)
    // observables
    // val opts = mutable.Map.empty[m.Node, Boolean]
    // for (x <- m.nodes)
    //   opts(x) = x.isOptimal
    // def count: Int = opts.count({ case (_, o) => o })
    // def cogstates = for ((x, o) <- opts if o) yield x.beliefs.flatten
    // def countDistinct = cogstates.toSet.size
    val belief = mutable.Map.empty[m.Node, Seq[Int]]
    val cogstates = mutable.Map.empty[Seq[Int], Int]
    val optimals = mutable.Set.empty[Seq[Int]]
    for (x <- m.nodes) {
      val v = // x.beliefs.flatten.toSeq
        (for (i <- 0 until (m.numConcepts-1);
              j <- (i+1) until m.numConcepts)
         yield x.beliefs(i)(j)).toSeq
      belief(x) = v
      if (cogstates contains v) cogstates(v) += 1
      else cogstates(v) = 1
      if (x.isOptimal) optimals += v
    }
    def opts: Int = optimals.toSeq.map(cogstates(_)).sum
    def distincts: Int = optimals.count(cogstates(_) > 0)
    def shared(u: Seq[Int], v: Seq[Int]): Double = {
      val n = m.numConcepts * (m.numConcepts-1) / 2
      require(u.size == n, s"${u.size} != $n")
      require(v.size == n, s"${v.size} != $n")
      var sames = 0
      for (i <- 0 until n if u(i) == v(i)) sames += 1
      // require(sames <= n, s"$sames > n")
      sames.toDouble / n
    }
    def totalhg: Double = {
      val u = cogstates.toSeq.maxBy({ case (v, n) => n })._1
      (for ((v, n) <- cogstates) yield n * shared(u, v)).sum
    }
    def opthg: Double = {
      var max = 0
      var u: Seq[Int] = null
      for ((v, n) <- cogstates if optimals(v)) {
        if (n > max) {
          max = n
          u = v
        }
      }
      if (u == null) 0.0
      else (for ((v, n) <- cogstates if optimals(v))
            yield n * shared(u, v)).sum
    }
    // track social and cognitive energy
    var se = m.socialEnergy
    var ce = m.cognitiveEnergy
    // run simulation
    println(s"# I=$social J=$cognitive T=$temperature <d>=$meanDegree")
    print("time optimal \"distinct optimal\" \"total homogeneity\" ")
    println("\"optimal homogeneity\" total soc cog")
    for (sc <- 0 until numSteps) {
      println(s"$sc $opts $distincts $totalhg $opthg ${se+ce} $se $ce")
      val (x, i, j, a, b, sediff, cediff) = m.stepDiffAndEnergy(rnd)
      if (x.beliefs(i)(j) == b) { // belief changed
        cogstates(belief(x)) -= 1
        val v = // x.beliefs.flatten
          (for (i <- 0 until (m.numConcepts-1);
                j <- (i+1) until m.numConcepts)
           yield x.beliefs(i)(j)).toSeq
        belief(x) = v
        if (cogstates contains v) cogstates(v) += 1
        else cogstates(v) = 1
        // TODO: x.isOptimal is run even if
        // we know already that it's not optimal
        if (!optimals(v) && x.isOptimal) optimals += v
        se += sediff
        ce += cediff
      }
    }
    println(s"$numSteps $opts $distincts $totalhg $opthg ${se+ce} $se $ce")
  }

  // TODO: it would be nice to make a video of the changing histogram
  // (or ranking?) of the number of individuals in each distinct
  // cognitive state.
  // together with a matrix that tells you have many shared beliefs
  // each pair of distinct cognitive states have.
  // possible observable: take the most populated cognitive state
  // and count 1 for each individual in it + the fraction of shared
  // beliefs times the number of the individuals in each different
  // cognitive state. so: \sum_{c \in C} s(c) * n(c)
  // with s(c) the number of shared beliefs between c and the most
  // populated cognitive state c\st (this is 1 when c = c\st) and
  // n(c) the number of individuals in that state.
  // it should be equivalent to \sum_{v \in V} s(v)
  // with s(v) the number of shared beliefs between node v and c\st.

  def runManyOpt: Unit = {
    println(s"# I=$social T=$temperature <d>=$meanDegree")
    print("J \"mean final optimal count\" ")
    print("\"standard deviation optimal count\" ")
    print("\"mean final distinct optimal count\" ")
    println("\"standard deviation distinct optimal count\"")
    // for (c <- 2.0 +: ((2.1 to 2.701 by .01) ++ (2.8 to 3.01 by 0.1))) {
    for (c <- 5.21 to 6.001 by .01) {
      cognitive = c
      // sum of the energies and their squares
      // to compute the mean and standard deviation
      var osum = 0.0
      var sqosum = 0.0
      var dsum = 0.0
      var sqdsum = 0.0
      // homogeneity observable
      var ssum = 0.0
      var sqssum = 0.0
      for (n <- 1 to numReps) {
        val m = new VoterModel(social, cognitive, temperature)
        // create random initial graph
        val rnd = new Random()
        // about 10 friends per node
        m.randomised(meanDegree / m.numNodes, rnd)
        // run simulation
        for (sc <- 0 until numSteps) {
          val (x, i, j, w, v) = m.stepDiff(rnd)
        }
        // observe final state
        var opts: Int = 0
        val cogstates = mutable.Map.empty[Seq[Int], Int]
        for (x <- m.nodes if x.isOptimal) {
          opts += 1
          val v = x.beliefs.flatten.toSeq
          if (cogstates contains v) cogstates(v) += 1
          else cogstates(v) = 1
          // for (i <- 0 until (m.numConcepts-1);
          //      j <- (i+1) until m.numConcepts)
          // yield x.beliefs(i)(j)
        }
        osum += opts
        sqosum += opts*opts
        val distincts = cogstates.size
        dsum += distincts
        sqdsum += distincts*distincts
        // val u = cogstates.toSeq.maxBy({ case (v, n) => n })._1
        // def shared(u: Seq[Int], v: Seq[Int]): Double = {
        //   val n = m.numConcepts * (m.numConcepts-1) / 2
        //   require(u.size == n)
        //   require(v.size == n)
        //   var sames = 0
        //   for (i <- 0 until n if u(i) == v(i)) sames += 1
        //   sames / n
        // }
        // val s = (for ((v, n) <- cogstates) yield n*shared(u, v)).sum
        // ssum += s
        // sqssum += s*s
      }
      val n = numReps
      def sd(sum: Double, sqsum: Double): Double =
        scala.math.sqrt((sqsum/n)-((sum/n)*(sum/n)))
      print(s"$cognitive ${osum/n} ${sd(osum, sqosum)}")
      println(s" ${dsum/n} ${sd(dsum, sqdsum)}")
      // print(s"$cognitive ${osum/n} ${sd(osum, sqosum)} ${dsum/n} ")
      // println(s"${sd(dsum, sqdsum)} ${ssum/n} ${sd(ssum, sqssum)}")
    }
  }

  def main(args: Array[String]): Unit = {
    // runManyOpt
    runOnceOpt
  }
}

