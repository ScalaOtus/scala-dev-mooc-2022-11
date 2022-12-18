package module1
import scala.util.Random

class BallsExperiment {

  def isFirstBlackSecondWhite(): Boolean = {
    ???
  }
}

object BallsTest {
  def main(args: Array[String]): Unit = {
    val count = 10000
    val listOfExperiments: List[BallsExperiment] = ???
    val countOfExperiments = ???
    val countOfPositiveExperiments: Float = countOfExperiments.count(_ == true)
    println(countOfPositiveExperiments / count)
  }
}
