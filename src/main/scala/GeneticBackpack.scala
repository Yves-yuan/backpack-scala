import java.io.{BufferedReader, File, FileReader}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object GeneticBackpack extends App {
  implicit val switch: SwitchLevel.Value = SwitchLevel.Off
  val checkSwitch: SwitchLevel.Value = SwitchLevel.Off
  val file = new File("./data/data1.txt")
  val mutateFactor = 0.1
  val initFactor = 16
  val mutateMaxNum = 0.01
  val maxConvergenceNum = 100
  val maxIteration = 6000
  val minIteration = 50
  val continuousVariance = 50
  val varianceFactor = 1.0 / 100
  val maxTime = 4000
  val reader = new FileReader(file)
  val lineReader = new BufferedReader(reader)
  val firstLine = lineReader.readLine()
  val firstSplit = firstLine.split(" ")
  val maxWeight = firstSplit(0).toInt
  val itemNum = firstSplit(1).toInt
  val items = ArrayBuffer[Item]()
  for (_ <- 0 until itemNum) {
    val il = lineReader.readLine().split(" ")
    items.append(Item(il(1).toInt, il(0).toInt))
  }
  val fromTime = System.currentTimeMillis()
  var maxPrice = 0L
  var weightOfMaxPrice = 0L
  var maxGroup: Option[Group] = None
  for (i <- 0 until 3) {
    val t = new Thread(new Runnable {
      override def run(): Unit = {
        var curTime = System.currentTimeMillis()
        while (curTime - fromTime < maxTime) {
          val maxFit = runGenetic()
//          println("cur max:" + maxFit.g.price + " weight:" + maxFit.g.weight)
          if (maxFit.g.price > maxPrice) {
            maxPrice = maxFit.g.price
            weightOfMaxPrice = maxFit.g.weight
            maxGroup = Some(maxFit.g)
          }
          println("maxFit:" + maxPrice + " weight:" + weightOfMaxPrice)
          curTime = System.currentTimeMillis()
        }
      }
    })
    t.start()
  }

  //scalastyle:off
  def runGenetic(): GroupWithFitness = {
    implicit val random: Random = new Random(new java.util.Random)
    val M = initFactor * items.length
    val (initGroups, totalPrice, maxGroup) = init(items, maxWeight, M)
    val (initFitGroups, initTotalFit, initMaxFit, _) = calcFitness(initGroups, totalPrice)
    var maxFit = initMaxFit
    var (genFitGroups, genTotalFit) = (initFitGroups, initTotalFit)
    var wj = 0
    var curTime = System.currentTimeMillis()
    var stop = false
    var continousSmallVarianceNum = 0
    val base = varianceFactor / M
    val varianceThreshold = scala.math.pow(base, 2) * M
    var convergenceNum = 0
    while (wj < maxIteration && curTime - fromTime < maxTime && !stop) {
      convergenceNum += 1
      val (curGenFitGroups, curGenTotalFit, curGenMaxFit, variance) = gen(genFitGroups, genTotalFit)
      if (variance < varianceThreshold) {
        continousSmallVarianceNum += 1
      } else {
        continousSmallVarianceNum = 0
      }
      if (continousSmallVarianceNum > continuousVariance && wj > minIteration) {
        stop = true
      }
      if (convergenceNum > maxConvergenceNum) {
        stop = true
      }
      genFitGroups = curGenFitGroups
      genTotalFit = curGenTotalFit
      if (curGenMaxFit.g.weight <= maxWeight && maxFit.g.weight > maxWeight) {
        maxFit = GroupWithFitness(Group(curGenMaxFit.g.g.clone(), curGenMaxFit.g.weight, curGenMaxFit.g.price), curGenMaxFit.fitness)
        convergenceNum = 0
      } else if (curGenMaxFit.g.weight <= maxWeight && curGenMaxFit.g.price > maxFit.g.price) {
        maxFit = GroupWithFitness(Group(curGenMaxFit.g.g.clone(), curGenMaxFit.g.weight, curGenMaxFit.g.price), curGenMaxFit.fitness)
        convergenceNum = 0
      }
      wj += 1
      curTime = System.currentTimeMillis()
    }
//    println("iteration:" + wj)
    maxFit
  }

  def gen(fitnessGroups: ArrayBuffer[GroupWithFitness], totalFitness: Double)
         (implicit random: Random): (ArrayBuffer[GroupWithFitness], Double, GroupWithFitness, Double) = {
    SwitchY.run {
      println("fitness")
      fitnessGroups.foreach(println)
    }
    val matingSize = fitnessGroups.length
    val (toMatingGroups, _, _) = select(fitnessGroups, totalFitness, matingSize)
    checkFb(toMatingGroups)
    SwitchY.run {
      println("mating")
      toMatingGroups.foreach(println)
    }
    val (children, childrenTotalPrice) = multiCross(toMatingGroups)
    check(children)
    SwitchY.run {
      println("children")
      children.foreach(println)
    }
    val nextGen = children
    check(nextGen)
    SwitchY.run {
      println("nextGen")
      nextGen.foreach(println)
      println("price :" + childrenTotalPrice)
    }
    val (mutateGen, mutateTotalPrice) = mutate(nextGen, childrenTotalPrice)
    check(mutateGen)
    SwitchY.run {
      println("nextGenMutate")
      mutateGen.foreach(println)
      println("price :" + mutateTotalPrice)
    }
    val (nextGenFit, nextGenTotalFitness, nextGenMaxFit, variance) = calcFitness(mutateGen, mutateTotalPrice)
    checkFb(nextGenFit)
    (nextGenFit, nextGenTotalFitness, nextGenMaxFit, variance)
  }

  def mutate(gb: ArrayBuffer[Group], totalPrice: Long)(implicit random: Random): (ArrayBuffer[Group], Long) = {
    val mutateP = mutateFactor
    var mutateTotalPrice = totalPrice
    val mutateGb = ArrayBuffer[Group]()
    gb.foreach { g => {
      val r = random.nextFloat()
      if (r < mutateP) {
        val mutateNum = random.nextInt(scala.math.round(mutateMaxNum * g.g.length).toInt) + 1
        val newG = g.g.clone()
        var newWeight = g.weight
        var newPrice = g.price
        for (_ <- 0 until mutateNum) {
          val i = random.nextInt(g.g.length)
          if (newG(i) == 0) {
            newG.update(i, 1)
            newWeight += items(i).weight
            newPrice += items(i).price
            mutateTotalPrice += items(i).price
          } else if (newG(i) == 1) {
            newG.update(i, 0)
            newWeight -= items(i).weight
            newPrice -= items(i).price
            mutateTotalPrice -= items(i).price
          } else {
            println("error binary not 0 and 1")
          }
        }
        val mutateGroup = Group(newG, newWeight, newPrice)
        mutateGb.append(mutateGroup)
      } else {
        mutateGb.append(g)
      }
    }
    }
    check(mutateGb)
    (mutateGb, mutateTotalPrice)

  }

  def multiCross(gs: ArrayBuffer[GroupWithFitness])(implicit random: Random): (ArrayBuffer[Group], Long) = {
    val crossP = 0.3
    val indexes = gs.indices
    val randomIndexes = random.shuffle(indexes.toList)
    val result = ArrayBuffer[Group]()
    var totalPrice = 0L
    for (i <- 0 until randomIndexes.length / 2) {
      val left = gs(randomIndexes(i))
      val right = gs(randomIndexes(i + 1))
      val childLeft = left.g.g.clone()
      val childRight = right.g.g.clone()
      for (j <- left.g.g.indices) {
        val r = random.nextFloat()
        if (r < crossP) {
          val t = childLeft(j)
          childLeft.update(j, childRight(j))
          childRight.update(j, t)
        }
      }
      val childLeftGroup = calcGroup(childLeft)
      val childRightGroup = calcGroup(childRight)
      if (left.g.price < childLeftGroup.price && childLeftGroup.weight <= maxWeight){
        result.append(childLeftGroup)
        totalPrice += childLeftGroup.price
      }else{
        result.append(left.g)
        totalPrice += left.g.price
      }
      if (right.g.price < childRightGroup.price && childRightGroup.weight <= maxWeight){
        result.append(childRightGroup)
        totalPrice += childRightGroup.price
      }else{
        result.append(right.g)
        totalPrice += right.g.price
      }
      SwitchY.run {
        println("multiCross left:" + left)
        println("multiCross right:" + right)
        println("multiCross left child:" + childLeftGroup)
        println("multiCross right child:" + childRightGroup)
      }
    }
    (result, totalPrice)
  }

  def select(groups: ArrayBuffer[GroupWithFitness], totalFitness: Double, num: Int)
            (implicit random: Random): (ArrayBuffer[GroupWithFitness], Long, GroupWithFitness) = {
    var totalPrice = 0L
    var maxFitnessGroup = groups(0)
    val resortedGroups = groups.sortBy(_.fitness)(Ordering.Double.reverse)
    val selectSize = num
    val retainRandoms = scala.collection.mutable.ArrayBuffer[Double]()
    for (_ <- 0 until selectSize) {
      retainRandoms.append(random.nextDouble() * totalFitness)
    }
    val sortedRandoms = retainRandoms.sortBy(x => x)(Ordering.Double)
    var i, j = 0
    var accumulatedPriority = 0.0
    val selectedGroups = scala.collection.mutable.ArrayBuffer[GroupWithFitness]()
    while (j < sortedRandoms.length) {
      val curRandom = sortedRandoms(j)
      while (accumulatedPriority + resortedGroups(i).fitness < curRandom) {
        accumulatedPriority += resortedGroups(i).fitness
        i += 1
      }
      selectedGroups.append(resortedGroups(i))
      if (resortedGroups(i).fitness > maxFitnessGroup.fitness) {
        maxFitnessGroup = resortedGroups(i)
      }
      totalPrice += resortedGroups(i).g.price
      j += 1
    }
    (selectedGroups, totalPrice, maxFitnessGroup)
  }

  def checkFb(fb: ArrayBuffer[GroupWithFitness]): Boolean = {
    check(fb.map(_.g))
  }

  def check(gb: ArrayBuffer[Group]): Boolean = {
    if (checkSwitch == SwitchLevel.On) {
      gb.foreach(g => {
        var totalPrice = 0.0
        var totalWeight = 0.0
        for (i <- g.g.indices) {
          totalPrice += g.g(i) * items(i).price
          totalWeight += g.g(i) * items(i).weight
        }
        if (totalPrice != g.price || totalWeight != g.weight) {
          val e = new Exception("error")
          e.printStackTrace()
          System.exit(1)
        }
      })
    }
    true
  }

  def init(items: ArrayBuffer[Item], maxWeight: Long, M: Int)(implicit random: Random): (ArrayBuffer[Group], Long, Group) = {
    var maxGroup: Group = Group(ArrayBuffer(), 0, 0)
    val groups = scala.collection.mutable.ArrayBuffer[Group]()
    var groupTotalPrice = 0L
    for (_ <- 0 until M) {
      val g = items.map { i => {
        0.toByte
      }
      }
      var totalWeight = 0L
      while (totalWeight <= maxWeight) {
        val r = random.nextInt(g.length)
        totalWeight += items(r).weight
        if (totalWeight <= maxWeight) {
          g.update(r, 1)
        }
      }
      val group = calcGroup(g)
      if (maxGroup.price < group.price && group.weight <= maxWeight) {
        maxGroup = group
      }
      groups.append(group)
      groupTotalPrice += group.price
    }
    (groups, groupTotalPrice, maxGroup)
  }

  def calcGroup(g: ArrayBuffer[Byte]): Group = {
    var totalWeight = 0L
    var totalPrice = 0L
    for (j <- g.indices) {
      totalWeight += (g(j) * items(j).weight)
      totalPrice += (g(j) * items(j).price)
    }
    Group(g, totalWeight, totalPrice)
  }

  def calcFitness(groups: ArrayBuffer[Group], totalPrice: Long): (ArrayBuffer[GroupWithFitness], Double, GroupWithFitness, Double) = {
    val result = scala.collection.mutable.ArrayBuffer[GroupWithFitness]()
    var maxGroupWithFitness = GroupWithFitness(Group(ArrayBuffer(0), 0, 0), 0)
    var totalFitness = 0.0
    groups.foreach { g => {
      if (g.weight > maxWeight) {
        val f = 0.2 * (maxWeight / g.weight) + 0.1 / groups.length
        val fg = GroupWithFitness(g, f)
        result.append(fg)
        if (f > maxGroupWithFitness.fitness) {
          maxGroupWithFitness = fg
        }
        totalFitness += f
      } else {
        val f = 0.7 * g.price / totalPrice + 0.3 / groups.length
        val fg = GroupWithFitness(g, f)
        result.append(fg)
        if (f > maxGroupWithFitness.fitness) {
          maxGroupWithFitness = fg
        }
        totalFitness += f
      }
    }
    }
    val meanFitness = totalFitness / groups.size
    val variance = result.map(gwf => {
      (gwf.fitness - meanFitness) * (gwf.fitness - meanFitness)
    }).sum
    (result, totalFitness, maxGroupWithFitness, variance)
  }

}


case class Group(g: ArrayBuffer[Byte], weight: Long, price: Long)

case class GroupWithFitness(g: Group, fitness: Double)

case class Item(price: Long, weight: Long)
