import scala.compat.Platform.EOL
import scala.io.Source

val day = 8

val filename = s"input/input$day.txt"
val bufferedSource = Source.fromResource(filename)
val inputIterator: Iterator[String] = bufferedSource.getLines()

val input = inputIterator

//val input = Seq(
//  "30373",
//  "25512",
//  "65332",
//  "33549",
//  "35390"
//)

val forest = input
  .map(_.sliding(1).map(_.toInt).toArray)
  .toArray
val turnedForest = forest.transpose

val lenght = forest.length
val width = forest.head.length

//println(forest.map(_.mkString(" ")).mkString(EOL))

def isVisibleBehind(tree: Int, treeArray: Array[Int]) = treeArray.forall(_ < tree)
def visibility(tree: Int, treeArray: Array[Int]) = {
  val visibleTrees = treeArray.takeWhile(_ < tree)
  if (visibleTrees.length < treeArray.length)
    visibleTrees.length + 1
  else
    visibleTrees.length
}

var visibleTreesCount = lenght * 2 + (width - 2) * 2
var scenicCountsMax = 0

for (i <- 1 until lenght - 1) {
  for (j <- 1 until width - 1) {
    val currentTree = forest(i)(j)

    val leftTrees = forest(i).take(j).reverse
    val rightTrees = forest(i).drop(j + 1)
    val topTrees = turnedForest(j).take(i).reverse
    val bottomTrees = turnedForest(j).drop(i + 1)

    val isVisible = isVisibleBehind(currentTree, leftTrees) ||
      isVisibleBehind(currentTree, rightTrees) ||
      isVisibleBehind(currentTree, topTrees) ||
      isVisibleBehind(currentTree, bottomTrees)

    val scenicScore = visibility(currentTree, leftTrees) *
      visibility(currentTree, rightTrees) *
      visibility(currentTree, topTrees) *
      visibility(currentTree, bottomTrees)

    val visibleString = if (isVisible) "*" else " "
    val scenicScoreString = (scenicScore.toString + "   ").take(3)
//    print(
//      s" $currentTree$visibleString " +
//        s"l[${leftTrees.mkString(",")}]-${visibilityRight(currentTree, leftTrees)} " +
//        s"r[${rightTrees.mkString(",")}]-${visibility(currentTree, rightTrees)} " +
//        s"t[${topTrees.mkString(",")}]-${visibilityRight(currentTree, topTrees)} " +
//        s"b[${bottomTrees.mkString(",")}]-${visibility(currentTree, bottomTrees)} " +
//        s"- $scenicScoreString "
//    )

    if (isVisible) {
      visibleTreesCount = visibleTreesCount + 1
    }
    if (scenicCountsMax < scenicScore) {
      scenicCountsMax = scenicScore
    }
  }
//  println
}

println(s"count=$visibleTreesCount")
println(s"scenicCountsMax=$scenicCountsMax")

bufferedSource.close()

