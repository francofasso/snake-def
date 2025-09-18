package snake.logic

import engine.random.RandomGenerator

// you can alter this file!

case class Dimensions(width : Int, height : Int) {
  // scanned from left to right, top to bottom
  def allPointsInside : Seq[Point] =
    for(y <- 0 until height; x <- 0 until width)
      yield Point(x,y)

  def placeApple(snakeBody: List[Point], random: RandomGenerator): Point = {
    val freePoints = allPointsInside.filterNot(snakeBody.contains)
    if (freePoints.isEmpty) null
    else freePoints(random.randomInt(freePoints.length))
  }
}