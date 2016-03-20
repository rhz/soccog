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
  val numSteps = 200000
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

  def using[A <: java.io.Closeable, B](a: A)(f: A => B): B =
    try { f(a) } finally { a.close() }

  def writeToFile(fileName: String, append: Boolean = false)(
    op: java.io.PrintWriter => Unit): Unit =
    using (new java.io.FileWriter(fileName, append)) { fileWriter =>
      using (new java.io.PrintWriter(fileWriter)) { printWriter =>
        op(printWriter)
      }
    }

  def runManyEnergy(start: Double, end: Double, step: Double,
    fileName: String): Unit = {
    writeToFile (fileName) { out =>
      out.println(s"# I=$social T=$temperature <d>=$meanDegree numSteps=$numSteps numReps=$numReps")
      out.print("J \"mean final social energy\" \"standard deviation social energy\" ")
      out.print("\"mean final cognitive energy\" \"standard deviation cognitive energy\" ")
      out.println("\"mean final total energy\" \"standard deviation total energy\"")
    }
    for (c <- start to end by step) { // 0.5 to 0.991 by .01) {
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
      writeToFile (fileName, true) { out =>
        out.print(s"$cognitive ${ssum/n} ${sd(ssum, sqssum)} ${csum/n}")
        out.println(s" ${sd(csum, sqcsum)} ${sum/n} ${sd(sum, sqsum)}")
      }
    }
  }

  def runOnceOpt(fileName: String): Unit = {
    val m = new VoterModel(social, cognitive, temperature)
    // create random initial graph
    val rnd = new Random()
    // about 10 friends per node
    m.randomised(meanDegree / m.numNodes, rnd)
    // observables
    var freshId: Int = 0
    val idOfCogstate = mutable.Map.empty[Seq[Int], Int]
    val cogstateOfId = mutable.Map.empty[Int, Seq[Int]]
    val cogstateIdOfNode = mutable.Map.empty[m.Node, Int]
    val cogstateCount = mutable.Map.empty[Int, Int]
    val optimals = mutable.Set.empty[Int]
    for (x <- m.nodes) {
      val v =
        (for (i <- 0 until (m.numConcepts-1);
              j <- (i+1) until m.numConcepts)
         yield x.beliefs(i)(j)).toSeq
      val id = if (idOfCogstate contains v) {
        val id = idOfCogstate(v)
        cogstateCount(id) += 1
        id
      } else {
        val id = freshId
        freshId += 1
        idOfCogstate(v) = id
        cogstateOfId(id) = v
        cogstateCount(id) = 1
        if (x.isOptimal) optimals += id
        id
      }
      cogstateIdOfNode(x) = id
    }
    val sharedmap = mutable.Map.empty[(Int, Int), Double]
    def opts: Int = optimals.toSeq.map(cogstateCount(_)).sum
    def distincts: Int = optimals.count(cogstateCount(_) > 0)
    def shared(i: Int, j: Int): Double =
      sharedmap get (i, j) match {
        case Some(s) => s
        case None => {
          val n = m.numConcepts * (m.numConcepts-1) / 2
          val u = cogstateOfId(i)
          val v = cogstateOfId(j)
          require(u.size == n, s"${u.size} != $n")
          require(v.size == n, s"${v.size} != $n")
          var sames = 0
          for (i <- 0 until n if u(i) == v(i)) sames += 1
          require(sames <= n, s"$sames > n")
          val fraction = sames.toDouble / n
          sharedmap((i, j)) = fraction
          sharedmap((j, i)) = fraction
          fraction
        }
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
    def homogeneity(i: Int, p: (Int => Boolean) = (_ => true)): Double =
      (for ((j, n) <- cogstateCount if (n > 0) && p(j))
       yield n * shared(i, j)).sum
    def dissent(i: Int, numDissidents: Int, p: (Int => Boolean) = (_ => true)): Double =
      (for ((j, n) <- cogstateCount if (n > 0) && (j != i) && p(j)) yield
        (n.toDouble / numDissidents) * (1.0 - shared(i, j))).sum
    // track social and cognitive energy
    var se = m.socialEnergy
    var ce = m.cognitiveEnergy
    // run simulation
    writeToFile (fileName) { out =>
      out.println(s"# I=$social J=$cognitive T=$temperature <d>=$meanDegree")
      out.print("time optimal \"distinct optimal\" \"total homogeneity\" ")
      out.print("\"optimal homogeneity\" \"total dissent\" \"optimal dissent\" ")
      out.print("\"total dissidents\" \"optimal dissidents\" total soc cog ")
      out.println("\"successful moves\"")
    }
    // var lastHg: Double = 0.0
    // var lastOptHg: Double = 0.0
    // var lastDis: Double = 0.0
    // var lastOptDis: Double = 0.0
    var lastId: Int = -1
    var lastOptId: Int = -1
    var changed: Int = 0
    writeDot(m, "0.dot")
    for (sc <- 0 until numSteps) {
      val popIds = mostPopular()
      val optPopIds = mostPopular(optimals)
      val (maxId, maxHg) = maxBy(popIds, i => homogeneity(i))
        // if (popIds(lastId)) (lastId, homogeneity(lastId))
        // else maxBy(popIds, i => homogeneity(i))
      val (maxOptId, maxOptHg) = maxBy(optPopIds, i => homogeneity(i, optimals))
        // if (optPopIds(lastOptId)) (lastOptId, homogeneity(lastOptId))
        // else maxBy(optPopIds, i => homogeneity(i, optimals))
      val numDissidents = (for ((j, n) <- cogstateCount
        if (n > 0) && (j != maxId)) yield n).sum
      val dis = dissent(maxId, numDissidents)
      val numOptDissidents = (for ((j, n) <- cogstateCount
        if (n > 0) && (j != maxOptId) && optimals(j)) yield n).sum
      val optDis = dissent(maxOptId, numOptDissidents, optimals)
      // TODO: can the creation of a new cogstate increase the
      // total homogeneity by more than 1?  Yes it can!
      // ** what's the maximum possible change then? **
      // if (sc > 0) {
      //   val tolerance = 1
      //   require((maxHg < lastHg + tolerance) && (maxHg > lastHg - tolerance),
      //     s"maxHg=$maxHg, lastHg=$lastHg, maxId=$maxId, lastId=$lastId, cogstateCount(maxId)=${cogstateCount(maxId)}")
      //   require((maxOptHg < lastOptHg + tolerance) && (maxOptHg > lastOptHg - tolerance),
      //     s"maxOptHg=$maxOptHg, lastOptHg=$lastOptHg, maxOptId=$maxOptId, lastOptId=$lastOptId")
      //   require((dis < lastDis + tolerance) && (dis > lastDis - tolerance),
      //     s"dis=$dis, lastDis=$lastDis, maxId=$maxId, lastId=$lastId")
      //   require((optDis < lastOptDis + tolerance) && (optDis > lastOptDis - tolerance),
      //     s"optDis=$optDis, lastOptDis=$lastOptDis, maxOptId=$maxOptId, lastOptId=$lastOptId")
      // }
      // lastHg = maxHg
      // lastOptHg = maxOptHg
      // lastDis = dis
      // lastOptDis = optDis
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
          // println(s"sharedmap.size = ${sharedmap.size}")
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
        writeDot(m, s"$sc.dot")
      }
    }
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
        val f = 1.0-((cog+120)/240)
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
    // runManyOpt
    if (args.size == 1)
      runOnceOpt(args(0))
    else
      println("Usage: VoterModel <filename>")
    // if (args.size == 4)
    //   runManyEnergy(args(0).toDouble, args(1).toDouble,
    //     args(2).toDouble, args(3))
    // else
    //   println("Usage: VoterModel <start> <end> <step> <filename>")
  }
}

