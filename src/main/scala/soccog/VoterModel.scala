package uk.ac.ed.inf
package soccog

import scala.collection.mutable
import scala.util.Random

class VoterModel(
  val numNodes: Int = 100,
  val numConcepts: Int = 10,
  var social: Double = 1.0,
  var cognitive: Double = 1.0) {

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
          e -= beliefs(i)(j) * y.beliefs(i)(j) * social
      e
    }
    def energy: Double = {
      var e = 0.0
      for (i <- 0 until numConcepts; j <- (i+1) until numConcepts) {
        for (k <- (j+1) until numConcepts)
          e -= beliefs(i)(j) * beliefs(j)(k) * beliefs(k)(i) * cognitive
        for (y <- friends)
          e -= beliefs(i)(j) * y.beliefs(i)(j) * social
      }
      e
    }
  }

  // i and j are concept indices, value can be 1 or -1
  // returns difference in energy
  def updateBelief(x: Node, i: Int, j: Int, value: Int)
      : (Double, Double) = {
    // assume(i != j)
    // assume(!x.friends.contains(x))
    var sediff = 0.0 // social energy difference
    var cediff = 0.0 // cognitive energy difference
    for (k <- 0 until numConcepts if (k != i) && (k != j))
      cediff += x.beliefs(j)(k) * x.beliefs(k)(i)
    for (y <- x.friends)
      sediff += y.beliefs(i)(j) * 2
    val diff = x.beliefs(i)(j) - value
    x.beliefs(i)(j) = value
    x.beliefs(j)(i) = value
    (sediff * diff * social, cediff * diff * cognitive)
  }

  def cognitiveEnergy: Double =
    (for (x <- nodes) yield x.cognitiveEnergy).sum

  def socialEnergy: Double =
    (for (x <- nodes) yield x.socialEnergy).sum

  def energy: Double =
    (for (x <- nodes) yield x.energy).sum

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

  case class Step(node: Node, i: Int, j: Int, oldvalue: Int, newvalue: Int,
    social: Double, cognitive: Double)

  def step(rnd: Random): Option[Step] = {
    val (x, i, j, w, v) = randomBeliefChange(rnd)
    val (sediff, cediff) = updateBelief(x, i, j, v)
    val energydiff = sediff + cediff
    val q = scala.math.exp(-energydiff)
    if (energydiff <= 0 || rnd.nextDouble <= q)
      Some(Step(x, i, j, w, v, sediff, cediff))
    else {
      updateBelief(x, i, j, w) // backtrack
      None
    }
  }

  def stepEnergy(rnd: Random): (Double, Double) = {
    val (x, i, j, w, v) = randomBeliefChange(rnd)
    val (sediff, cediff) = updateBelief(x, i, j, v)
    val energydiff = sediff + cediff
    val q = scala.math.exp(-energydiff)
    if (energydiff <= 0 || rnd.nextDouble <= q) (sediff, cediff)
    else { updateBelief(x, i, j, w); (0.0, 0.0) } // backtrack
  }

  def stepDiff(rnd: Random): (Node, Int, Int, Int, Int) = {
    val (x, i, j, w, v) = randomBeliefChange(rnd)
    val (sediff, cediff) = updateBelief(x, i, j, v)
    val energydiff = sediff + cediff
    val q = scala.math.exp(-energydiff)
    if (energydiff > 0 && rnd.nextDouble > q)
      updateBelief(x, i, j, w) // backtrack
    (x, i, j, w, v)
  }

  def stepDiffAndEnergy(rnd: Random): (Node, Int, Int, Int, Int, Double, Double) = {
    val (x, i, j, w, v) = randomBeliefChange(rnd)
    val (sediff, cediff) = updateBelief(x, i, j, v)
    val energydiff = sediff + cediff
    val q = scala.math.exp(-energydiff)
    if (energydiff <= 0 || rnd.nextDouble <= q)
      (x, i, j, w, v, sediff, cediff)
    else {
      updateBelief(x, i, j, w) // backtrack
      (x, i, j, w, v, 0.0, 0.0)
    }
  }

  def randomiseCogstates(rnd: Random): Unit = {
    for (x <- nodes; i <- 0 until numConcepts; j <- (i+1) until numConcepts) {
      x.beliefs(i)(j) = rnd.nextInt(3) - 1
      x.beliefs(j)(i) = x.beliefs(i)(j)
    }
  }

  def randomiseFriendsP(linkProb: Double, rnd: Random): Unit = {
    for (x <- nodes; y <- nodes if (x != y) && (rnd.nextDouble < linkProb))
      x befriend y
  }

  def randomiseFriendsE(numEdges: Int, rnd: Random): Unit = {
    for (_ <- 1 to numEdges) {
      val x = rnd.nextInt(numNodes)
      var y = rnd.nextInt(numNodes)
      val n = nodes(x)
      while ((x == y) || n.friends(nodes(y)))
        y = rnd.nextInt(numNodes)
      n befriend nodes(y)
    }
  }

  def numBeliefs = numConcepts * (numConcepts-1) / 2
  def numTriangles = numConcepts * (numConcepts-1) * (numConcepts-2) / 6
  def numEdges = ((for (n <- nodes) yield n.friends.size).sum / 2).toInt

  // global minimum energy
  // G = -(2 (M choose 2) E I + (M choose 3) N J)
  def minSocialEnergy: Double = -numBeliefs * numEdges * social * 2
  def minCognitiveEnergy: Double = -numTriangles * numNodes * cognitive
  def minEnergy: Double = minSocialEnergy + minCognitiveEnergy

  // given a fixed minimum energy and a ratio between social and
  // cognitive (I/J), compute the values for social and cognitive
  def setTemp(minEnergy: Double, ratio: Double): Unit = {
    // I = -G / [M (M-1) [E + ((M-2) N)/(6 R)]] (check google docs)
    social = -minEnergy/(numConcepts*(numConcepts-1)*(
      numEdges+((numConcepts-2)*numNodes)/(6*ratio)))
    cognitive = social/ratio
  }

  def setTemp(ratio: Double): Unit = setTemp(minEnergy, ratio)
}

object VoterModel {

  // model parameters
  val numReps = 10000
  val numSteps = 200000
  var cognitive = 1.0
  var social = 1.0
  val numNodes = 100
  val meanDegree = 5.0
  val numEdges = 250 // about 5 per node
  val numConcepts = 10

  def numBeliefs = numConcepts * (numConcepts-1) / 2
  def numTriangles = numConcepts * (numConcepts-1) * (numConcepts-2) / 6

  // global minimum energy
  // G = -(2 (M choose 2) E I + (M choose 3) N J)
  def minSocEnergy: Double = -numBeliefs * numEdges * social * 2
  def minCogEnergy: Double = -numTriangles * numNodes * cognitive
  def minimumEnergy: Double = minSocEnergy + minCogEnergy

  // given a fixed minimum energy and a ratio between social and
  // cognitive (I/J), compute the values for social and cognitive
  def setTemp(minEnergy: Double, ratio: Double): Unit = {
    // I = -G / [M (M-1) [E + ((M-2) N)/(6 R)]] (check google docs)
    social = -minEnergy/(numConcepts*(numConcepts-1)*(
      numEdges+((numConcepts-2)*numNodes)/(6*ratio)))
    cognitive = social/ratio
  }

  // TODO: check evolution of rejection rate vs energy

  def runOnceEnergy(fileName: String): Unit = {
    val m = new VoterModel(numNodes, numConcepts, social, cognitive)
    // create random initial graph
    val rnd = new Random()
    // about 10 friends per node
    m.randomiseFriendsP(meanDegree / m.numNodes, rnd)
    m.randomiseCogstates(rnd)
    // track social and cognitive energy
    var se = m.socialEnergy
    var ce = m.cognitiveEnergy
    writeToFile (fileName) { out =>
      out.println(s"# N=$numNodes K=$meanDegree M=$numConcepts I=$social J=$cognitive ")
      out.println("time total soc cog")
      out.println(s"0 ${se+ce} $se $ce")
    }
    // run simulation
    for (sc <- 1 to numSteps) {
      val (sediff, cediff) = m.stepEnergy(rnd)
      se += sediff
      ce += cediff
      writeToFile (fileName, true) { out =>
        out.println(s"$sc ${se+ce} $se $ce")
      }
    }
  }

  def runManyEnergy(start: Double, end: Double, step: Double,
    fileName: String): Unit = {
    writeToFile (fileName) { out =>
      out.println(s"# N=$numNodes K=$meanDegree M=$numConcepts I=$social numSteps=$numSteps numReps=$numReps")
      out.print("J \"mean final social energy\" \"standard deviation social energy\" ")
      out.print("\"mean final cognitive energy\" \"standard deviation cognitive energy\" ")
      out.println("\"mean final total energy\" \"standard deviation total energy\"")
    }
    for (c <- start to end by step) {
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
        val m = new VoterModel(numNodes, numConcepts, social, cognitive)
        // create random initial graph
        val rnd = new Random()
        // about 10 friends per node
        m.randomiseFriendsP(meanDegree / m.numNodes, rnd)
        m.randomiseCogstates(rnd)
        // run simulation
        for (_ <- 1 to numSteps) m.stepEnergy(rnd)
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
      writeToFile (fileName, true) { out =>
        out.print(s"$cognitive ${ssum/n} ${sd(ssum, sqssum)} ${csum/n}")
        out.println(s" ${sd(csum, sqcsum)} ${sum/n} ${sd(sum, sqsum)}")
      }
    }
  }


  // Cogstate observables

  var freshId: Int = 0
  val idOfCogstate = mutable.Map.empty[Seq[Int], Int]
  val cogstateOfId = mutable.Map.empty[Int, Seq[Int]]
  val cogstateIdOfNode = mutable.Map.empty[VoterModel#Node, Int]
  val cogstateCount = mutable.Map.empty[Int, Int]

  def initCogstates(m: VoterModel): Unit = {
    idOfCogstate.clear
    cogstateOfId.clear
    cogstateIdOfNode.clear
    cogstateCount.clear
    freshId = 0
    for (x <- m.nodes) {
      val v =
        (for (i <- 0 until (m.numConcepts-1);
              j <- (i+1) until m.numConcepts)
         yield x.beliefs(i)(j)).toSeq
      if (idOfCogstate contains v) {
        val id = idOfCogstate(v)
        cogstateCount(id) += 1
        cogstateIdOfNode(x) = id
      } else {
        val id = freshId
        freshId += 1
        idOfCogstate(v) = id
        cogstateOfId(id) = v
        cogstateCount(id) = 1
        cogstateIdOfNode(x) = id
      }
    }
  }


  // Cogstate analysis

  def maxBy(xs: Iterable[Int], f: Int => Double): (Int, Double) = {
    var max: Int = -1
    var maxValue: Double = 0.0
    for (x <- xs) {
      val fx = f(x)
      if (fx > maxValue) {
        max = x
        maxValue = fx
      }
    }
    (max, maxValue)
  }

  def mostPopular(p: (Int => Boolean) = (_ => true))
      : mutable.Set[Int] = {
    var max: Int = 0
    val ids = mutable.Set.empty[Int]
    for ((i, n) <- cogstateCount if p(i)) {
      if (n > max) {
        max = n
        ids.clear
        ids += i
      } else if (n == max) {
        ids += i
      }
    }
    ids
  }

  // i and j are cogstate ids
  def shared(m: VoterModel, i: Int, j: Int): Double = {
    val n = m.numConcepts * (m.numConcepts-1) / 2
    val u = cogstateOfId(i)
    val v = cogstateOfId(j)
    require(u.size == n, s"${u.size} != $n")
    require(v.size == n, s"${v.size} != $n")
    var sames = 0
    for (i <- 0 until n if u(i) == v(i)) sames += 1
    require(sames <= n, s"$sames > n")
    sames.toDouble / n
  }

  def homogeneity(m: VoterModel, i: Int, p: (Int => Boolean) = (_ => true)): Double =
    (for ((j, n) <- cogstateCount if (n > 0) && p(j))
     yield n * shared(m, i, j)).sum

  def dissent(m: VoterModel, i: Int, numDissidents: Int,
    p: (Int => Boolean) = (_ => true)): Double =
    (for ((j, n) <- cogstateCount if (n > 0) && (j != i) && p(j)) yield
      (n.toDouble / numDissidents) * (1.0 - shared(m, i, j))).sum


  // Optimality observables

  val optimals = mutable.Set.empty[Int]

  def initOpts(m: VoterModel): Unit = {
    optimals.clear
    val seen = mutable.Set.empty[Int]
    for (x <- m.nodes) {
      val id = cogstateIdOfNode(x)
      if (!seen(id)) {
        if (x.isOptimal) optimals += id
        seen += id
      }
    }
  }

  def opts: Int = optimals.toSeq.map(cogstateCount(_)).sum
  def distincts: Int = optimals.count(cogstateCount(_) > 0)


  // Simulation methods that analyse cogstates

  def runOnceOpt(fileName: String): Unit = {
    val m = new VoterModel(numNodes, numConcepts, social, cognitive)
    // create random initial graph
    val rnd = new Random()
    // about 10 friends per node
    m.randomiseFriendsP(meanDegree / m.numNodes, rnd)
    m.randomiseCogstates(rnd)
    // observables
    initCogstates(m)
    initOpts(m)
    // memoised version of shared
    val sharedmap = mutable.Map.empty[(Int, Int), Double]
    def mshared(i: Int, j: Int): Double =
      sharedmap get (i, j) match {
        case Some(s) => s
        case None => {
          val fraction = shared(m, i, j)
          sharedmap((i, j)) = fraction
          sharedmap((j, i)) = fraction
          fraction
        }
      }
    def homogeneity(i: Int, p: (Int => Boolean) = (_ => true)): Double =
      (for ((j, n) <- cogstateCount if (n > 0) && p(j))
       yield n * mshared(i, j)).sum
    def dissent(i: Int, numDissidents: Int,
      p: (Int => Boolean) = (_ => true)): Double =
      (for ((j, n) <- cogstateCount if (n > 0) && (j != i) && p(j)) yield
        (n.toDouble / numDissidents) * (1.0 - mshared(i, j))).sum
    // track social and cognitive energy
    var se = m.socialEnergy
    var ce = m.cognitiveEnergy
    // run simulation
    writeToFile (fileName) { out =>
      out.println(s"# N=$numNodes K=$meanDegree M=$numConcepts I=$social J=$cognitive")
      out.print("time optimal \"distinct optimal\" \"total homogeneity\" ")
      out.print("\"optimal homogeneity\" \"total dissent\" \"optimal dissent\" ")
      out.print("\"total dissidents\" \"optimal dissidents\" total soc cog ")
      out.println("\"successful moves\"")
    }
    var lastId: Int = -1
    var lastOptId: Int = -1
    var changed: Int = 0
    writeDot(m, "0.dot")
    for (sc <- 0 until numSteps) {
      val popIds = mostPopular()
      val optPopIds = mostPopular(optimals)
      val (maxId, maxHg) = // maxBy(popIds, i => homogeneity(i))
        if (popIds(lastId)) (lastId, homogeneity(lastId))
        else maxBy(popIds, i => homogeneity(i))
      val (maxOptId, maxOptHg) = // maxBy(optPopIds, i => homogeneity(i, optimals))
        if (optPopIds(lastOptId)) (lastOptId, homogeneity(lastOptId))
        else maxBy(optPopIds, i => homogeneity(i, optimals))
      val numDissidents = (for ((j, n) <- cogstateCount
        if (n > 0) && (j != maxId)) yield n).sum
      val dis = dissent(maxId, numDissidents)
      val numOptDissidents = (for ((j, n) <- cogstateCount
        if (n > 0) && (j != maxOptId) && optimals(j)) yield n).sum
      val optDis = dissent(maxOptId, numOptDissidents, optimals)
      // TODO: can the creation of a new cogstate increase the
      // total homogeneity by more than 1?  Yes it can! How?
      // What's the maximum possible change?
      lastId = maxId
      lastOptId = maxOptId
      writeToFile (fileName, true) { out =>
        out.print(s"$sc $opts $distincts $maxHg $maxOptHg ")
        out.print(s"$dis $optDis $numDissidents $numOptDissidents ")
        out.println(s"${se+ce} $se $ce $changed")
      }
      val (x, i, j, a, b, sediff, cediff) = m.stepDiffAndEnergy(rnd)
      if (x.beliefs(i)(j) == b) { // belief changed
        val oldcogstate = cogstateIdOfNode(x)
        cogstateCount(oldcogstate) -= 1
        if (cogstateCount(oldcogstate) == 0) {
          val keys = for ((i, j) <- sharedmap.keys
            if (i == oldcogstate) || (j == oldcogstate)) yield (i, j)
          for (k <- keys) sharedmap -= k
          // TODO: should I remove oldcogstate from cogstateCount as well?
        }
        val v =
          (for (i <- 0 until (m.numConcepts-1);
                j <- (i+1) until m.numConcepts)
           yield x.beliefs(i)(j)).toSeq
        val id = if (idOfCogstate contains v) {
          val id = idOfCogstate(v)
          cogstateCount(id) += 1
          id
        } else { // new cogstate found
          val id = freshId
          freshId += 1
          idOfCogstate(v) = id
          cogstateOfId(id) = v
          cogstateCount(id) = 1
          if (x.isOptimal) optimals += id
          id
        }
        cogstateIdOfNode(x) = id
        se += sediff
        ce += cediff
        changed += 1
        writeDot(m, s"${sc+1}.dot")
      }
    }
  }

  def runManyOpt(start: Double, end: Double, step: Double,
    fileName: String): Unit = {
    writeToFile (fileName) { out =>
      out.println(s"# N=$numNodes K=$meanDegree M=$numConcepts I=$social numSteps=$numSteps numReps=$numReps")
      out.print("J \"mean final optimal count\" ")
      out.print("\"standard deviation optimal count\" ")
      out.print("\"mean final distinct optimal count\" ")
      out.println("\"standard deviation distinct optimal count\"")
    }
    for (c <- start to end by step) {
      cognitive = c
      // sum of the observables and their squares
      // to compute the mean and standard deviation
      // number of cog-optimal nodes
      var osum = 0.0
      var sqosum = 0.0
      // number of distinct optimal cogstates
      var dsum = 0.0
      var sqdsum = 0.0
      // homogeneity observable
      var hsum = 0.0
      var sqhsum = 0.0
      for (n <- 1 to numReps) {
        val m = new VoterModel(numNodes, numConcepts, social, cognitive)
        // create random initial graph
        val rnd = new Random()
        m.randomiseFriendsP(meanDegree / m.numNodes, rnd)
        m.randomiseCogstates(rnd)
        // run simulation
        for (_ <- 1 to numSteps) m.stepEnergy(rnd)
        // observe final state
        initCogstates(m)
        initOpts(m)
        val o = opts
        osum += o
        sqosum += o*o
        val d = distincts
        dsum += d
        sqdsum += d*d
        val popIds = mostPopular()
        val (_, h) = maxBy(popIds, i => homogeneity(m, i))
        hsum += h
        sqhsum += h*h
      }
      val n = numReps
      def sd(sum: Double, sqsum: Double): Double =
        scala.math.sqrt((sqsum/n)-((sum/n)*(sum/n)))
      writeToFile (fileName, true) { out =>
        out.print(s"$cognitive ${osum/n} ${sd(osum, sqosum)} ${dsum/n} ")
        out.println(s"${sd(dsum, sqdsum)} ${hsum/n} ${sd(hsum, sqhsum)}")
      }
    }
  }

  def runManyCogs(start: Double, end: Double, step: Double,
    baseName: String): Unit = {
    val fileName = baseName + ".out"
    writeToFile (fileName) { out =>
      out.println(s"# N=$numNodes E=$numEdges M=$numConcepts numSteps=$numSteps numReps=$numReps")
      out.print("log_M(ratio) I J soc sd(soc) \"soc efficiency\" \"sd(soc efficiency)\"")
      out.print(" cog sd(cog) \"cog efficiency\" \"sd(cog efficiency)\"")
      out.print(" total sd(total) \"total efficiency\" \"sd(total efficiency)\"")
      out.println(" \"number of distinct cogstates\" \"sd(distinct cogstates)\"")
    }
    val formatter = new java.text.DecimalFormat("#.###")
    val minEnergy = minimumEnergy
    val psumss = mutable.ArrayBuffer.empty[Array[Double]]
    val sqpsumss = mutable.ArrayBuffer.empty[Array[Double]]
    val numCogs = 9
    val n = numReps
    def sd(sum: Double, sqsum: Double): Double =
      scala.math.sqrt((sqsum/n)-((sum/n)*(sum/n)))
    for (logratio <- start to end by step) {
      // TODO: change name of the function setTemp
      setTemp(minEnergy, scala.math.pow(numConcepts, logratio))
      val me = minimumEnergy
      assert((minEnergy > me-(1e-8)) && (minEnergy < me+(1e-8)), s"$minEnergy != $me")
      // sum of the energies and their squares
      // to compute the mean and standard deviation
      var sum = 0.0
      var sqsum = 0.0
      var ssum = 0.0
      var sqssum = 0.0
      var csum = 0.0
      var sqcsum = 0.0
      // efficiencies
      var esum = 0.0
      var sqesum = 0.0
      var sesum = 0.0
      var sqsesum = 0.0
      var cesum = 0.0
      var sqcesum = 0.0
      // and for the cogstate counts
      var nsum = 0.0 // number of different cogstates
      var sqnsum = 0.0
      // number of individuals in each cogstate ordered by popularity
      val psums = Array.fill(numCogs)(0.0)
      val sqpsums = Array.fill(numCogs)(0.0)
      val histFile = baseName + "-" + formatter.format(logratio) + ".hist"
      writeToFile (histFile) { out =>
        out.print(s"# N=$numNodes E=$numEdges M=$numConcepts I=$social")
        out.println(s" J=$cognitive numSteps=$numSteps numReps=$numReps")
      }
      for (n <- 1 to numReps) {
        val m = new VoterModel(numNodes, numConcepts, social, cognitive)
        // create random initial graph
        val rnd = new Random()
        m.randomiseFriendsE(numEdges, rnd)
        m.randomiseCogstates(rnd)
        // run simulation
        for (_ <- 1 to numSteps) m.stepEnergy(rnd)
        // observe final state
        initCogstates(m)
        // accumulate the energies
        val mse = minSocEnergy
        val mse2 = mse+mse
        val mce = minCogEnergy
        val mce2 = mce+mce
        val me2 = minEnergy+minEnergy
        val se = m.socialEnergy
        assert(se >= mse-(1e-8), s"soc: $se < $mse")
        ssum += se
        sqssum += se*se
        val seff = (se+mse)/mse2
        assert(seff <= 1.0+(1e-8), s"soc: $seff > 1.0")
        sesum += seff
        sqsesum += seff*seff
        val ce = m.cognitiveEnergy
        assert(ce >= mce-(1e-8), s"cog: $ce < $mce")
        csum += ce
        sqcsum += ce*ce
        val ceff = (ce+mce)/mce2
        assert(ceff <= 1.0+(1e-8), s"cog: $ceff > 1.0")
        cesum += ceff
        sqcesum += ceff*ceff
        val e = se+ce
        assert(e >= minEnergy-(1e-8), s"total: $e < $minEnergy")
        sum += e
        sqsum += e*e
        val eff = (e+minEnergy)/me2
        assert(eff <= 1.0+(1e-8), s"total: $eff > 1.0")
        esum += eff
        sqesum += eff*eff
        // and the cogstate counts
        val cogstates = cogstateCount.values.toSeq.sorted.reverse
        val cs = cogstates.size
        nsum += cs
        sqnsum += cs*cs
        // for ((p, i) <- cogstates.zipWithIndex if psums contains i) {
        for (i <- 0 until numCogs if i < cogstates.size) {
          val p = cogstates(i)
          psums(i) += p
          sqpsums(i) += p*p
        }
        // add a line to the histogram
        writeToFile (histFile, true) { out =>
          out.println(cogstates.mkString(" "))
        }
      }
      writeToFile (fileName, true) { out =>
        out.print(s"${formatter.format(logratio)} $social $cognitive")
        out.print(s" ${ssum/n} ${sd(ssum, sqssum)} ${sesum/n} ${sd(sesum, sqsesum)}")
        out.print(s" ${csum/n} ${sd(csum, sqcsum)} ${cesum/n} ${sd(cesum, sqcesum)}")
        out.print(s" ${sum/n} ${sd(sum, sqsum)} ${esum/n} ${sd(esum, sqesum)}")
        out.println(s" ${nsum/n} ${sd(nsum, sqnsum)}")
      }
      psumss += psums
      sqpsumss += sqpsums
    }
    val ccFile = baseName + "-cc.out"
    writeToFile (ccFile) { out =>
      out.println(s"# N=$numNodes E=$numEdges M=$numConcepts numSteps=$numSteps numReps=$numReps")
      out.print("ranking ")
      // print headers for the mean and stddev
      out.println((for (logratio <- start to end by step) yield {
        val r = formatter.format(logratio)
        s"$r $r"
      }).mkString(" "))
    }
    for (i <- 0 until numCogs) {
      writeToFile (ccFile, true) { out =>
        out.print(s"${i+1} ")
        out.println((for (j <- 0 until psumss.size; s = psumss(j)(i)) yield
          s"${s/n} ${sd(s, sqpsumss(j)(i))}").mkString(" "))
      }
    }
  }


  // auxiliary functions

  def using[A <: java.io.Closeable, B](a: A)(f: A => B): B =
    try { f(a) } finally { a.close() }

  def writeToFile(fileName: String, append: Boolean = false)(
    op: java.io.PrintWriter => Unit): Unit =
    using (new java.io.FileWriter(fileName, append)) { fileWriter =>
      using (new java.io.PrintWriter(fileWriter)) { printWriter =>
        op(printWriter)
      }
    }


  // drawing functions

  // https://en.wikibooks.org/wiki/Color_Theory/Color_gradient
  // https://stackoverflow.com/questions/26106695/converting-int-value-to-a-color-in-a-gradient
  def rainbow(pos: Double): String = {
    val nmax = 5 // number of colour segments
    val m = pos*nmax
    val n = m.toInt
    val f = m-n
    val t = (f*255).toInt
    n match {
      case 0 => rgbToHex(255, t, 0)
      case 1 => rgbToHex(255-t, 255, 0)
      case 2 => rgbToHex(0, 255, t)
      case 3 => rgbToHex(0, 255-t, 255)
      case 4 => rgbToHex(t, 0, 255)
      case _ => throw new IllegalArgumentException(
        "pos parameter $pos > 1")
    }
  }

  def rgbToHex(red: Int, green: Int, blue: Int): String =
    toHex(red) + toHex(green) + toHex(blue)

  def toHex(x: Int): String = {
    val h = Integer.toString(x, 16)
    if (h.size == 1) "0" + h else h
  }

  def writeDot(m: VoterModel, fileName: String): Unit = {
    writeToFile (fileName) { out =>
      out.println("graph {")
      // https://stackoverflow.com/questions/3967600/how-to-prevent-edges-in-graphviz-to-overlap-each-other
      out.println("  overlap=scale;")
      out.println("  splines=true;")
      // print nodes
      val nn = m.nodes.zipWithIndex.toMap
      for (x <- m.nodes) {
        val cog = x.cognitiveEnergy/m.cognitive
        val t = m.numConcepts*(m.numConcepts-1)*(m.numConcepts-2)/6
        val f = 1.0-((cog+t)/(t*2))
        val alpha = (f*f*255).toInt
        require(alpha < 256, s"$alpha > 256")
        var numNegativeBeliefs = 0
        for (i <- 0 until m.numConcepts; j <- (i+1) until m.numConcepts
          if x.beliefs(i)(j) == -1) numNegativeBeliefs += 1
        val rgba = rainbow(numNegativeBeliefs / 45.0) + toHex(alpha)
        out.print(s"""  ${nn(x)} [label="", shape="circle", """)
        out.println(s"""style="filled", color="#$rgba"];""")
      }
      // print edges
      val visited = mutable.Set.empty[m.Node]
      for (x <- m.nodes; y <- x.friends if !visited(y)) {
        out.println(s"  ${nn(x)} -- ${nn(y)};")
        visited += x
      }
      out.println(s"}")
    }
  }


  def main(args: Array[String]): Unit = {
    // if (args.size == 4)
    //   runManyEnergy(args(0).toDouble, args(1).toDouble,
    //     args(2).toDouble, args(3))
    // else
    //   println("Usage: VoterModel <start> <end> <step> <filename>")
    //
    // if (args.size == 1)
    //   runOnceOpt(args(0))
    // else
    //   println("Usage: VoterModel <filename>")
    //
    // if (args.size == 4)
    //   runManyOpt(args(0).toDouble, args(1).toDouble, args(2).toDouble,
    //     args(3))
    // else
    //   println("Usage: VoterModel <start> <end> <step> <minimum energy> <filename>")
    //
    if (args.size == 4)
      runManyCogs(args(0).toDouble, args(1).toDouble,
        args(2).toDouble, args(3))
    else
      println("Usage: VoterModel <start> <end> <step> <filename>")
  }
}

