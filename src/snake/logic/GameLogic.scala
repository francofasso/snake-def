package snake.logic

import engine.random.RandomGenerator
import snake.logic.GameLogic._

/** To implement Snake, complete the `TODOs` below.
 *
 * If you need additional files,
 * please also put them in the `snake` package.
 */
class GameLogic(val random: RandomGenerator,
                val gridDims: Dimensions) {

  // Snake body represented as list of points (head first)
  private var snakeBody: List[Point] = List(Point(2, 0), Point(1, 0), Point(0, 0))
  private var currentDirection: Direction = East()
  private var directionQueue: List[Direction] = List()
  private var applePosition: Point = _
  private var growthRemaining: Int = 0
  private var gameOverFlag: Boolean = false

  // Initialize apple position
  applePosition = generateApplePosition()

  def getCellType(p: Point): CellType = {
    if (snakeBody.nonEmpty && snakeBody.head == p) {
      SnakeHead(currentDirection)
    } else if (snakeBody.contains(p)) {
      val index = snakeBody.indexOf(p)
      val distance = if (snakeBody.length <= 1) 0f else index.toFloat / (snakeBody.length - 1).toFloat
      SnakeBody(distance)
    } else if (p == applePosition && applePosition != null) {
      Apple()
    } else {
      Empty()
    }
  }

  def step(): Unit = {
    if (gameOverFlag) return

    // Find the last valid direction in the queue
    var newDirection = currentDirection
    for (dir <- directionQueue) {
      if (dir != newDirection.opposite) {
        newDirection = dir
      }
    }
    directionQueue = List()  // Empties the queue
    currentDirection = newDirection

    val currentHead = snakeBody.head
    val newHead = movePoint(currentHead, currentDirection)

    val willCollide = if (growthRemaining > 0) {
      // If it grows, check all body
      snakeBody.contains(newHead)
    } else {
      // If it doesn't grow, don't check the tail
      snakeBody.init.contains(newHead)
    }

    if (willCollide) {
      gameOverFlag = true
      return
    }

    // Check if apple was eaten, but before moving
    val ateApple = newHead == applePosition

    // Move snake - add new head
    snakeBody = newHead :: snakeBody

    // Handle growth/shrinking
    if (growthRemaining > 0) {
      growthRemaining -= 1
    } else {
      // Remove tail if not growing
      if (snakeBody.length > 1) {
        snakeBody = snakeBody.init
      }
    }

    // If apple was eaten, generate new apple position and start growing
    if (ateApple) {
      growthRemaining += 3
      applePosition = generateApplePosition()
    }
  }

  def changeDir(d: Direction): Unit = {
    if (!gameOverFlag) {
      directionQueue = directionQueue :+ d
    }
  }

  def gameOver: Boolean = gameOverFlag

  def setReverse(r: Boolean): Unit = {
    // Assignment 2.1: Do nothing for reverse mode
  }

  private def movePoint(point: Point, direction: Direction): Point = {
    direction match {
      case East() => Point((point.x + 1) % gridDims.width, point.y)
      case West() => Point((point.x - 1 + gridDims.width) % gridDims.width, point.y)
      case North() => Point(point.x, (point.y - 1 + gridDims.height) % gridDims.height)
      case South() => Point(point.x, (point.y + 1) % gridDims.height)
    }
  }

  private def generateApplePosition(): Point = {
    val allPoints = gridDims.allPointsInside
    val freePoints = allPoints.filterNot(p => snakeBody.contains(p))

    if (freePoints.isEmpty) {
      // No free space - return null to indicate no apple can be placed
      null
    } else {
      val index = random.randomInt(freePoints.length)
      freePoints(index)
    }
  }
}

/** GameLogic companion object */
object GameLogic {

  val FramesPerSecond: Int = 5 // change this to increase/decrease speed of game

  val DrawSizeFactor = 1.0 // increase this to make the game bigger (for high-res screens)
  // or decrease to make game smaller

  // These are the dimensions used when playing the game.
  // When testing the game, other dimensions are passed to
  // the constructor of GameLogic.
  //
  // DO NOT USE the variable DefaultGridDims in your code!
  //
  // Doing so will cause tests which have different dimensions to FAIL!
  //
  // In your code only use gridDims.width and gridDims.height
  // do NOT use DefaultGridDims.width and DefaultGridDims.height
  val DefaultGridDims: Dimensions = Dimensions(width = 25, height = 25)
}
