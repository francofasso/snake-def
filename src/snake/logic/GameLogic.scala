package snake.logic

import engine.random.RandomGenerator
import snake.logic.GameLogic._

/** To implement Snake, complete the ``TODOs`` below.
 *
 * If you need additional files,
 * please also put them in the ``snake`` package.
 */
class GameLogic(val random: RandomGenerator,
                val gridDims: Dimensions) {

  // Snake body represented as list of points (head first)
  private var snakeBody: List[Point] = List(Point(2, 0), Point(1, 0), Point(0, 0))
  private var currentDirection: Direction = East()
  private var directionQueue: List[Direction] = List()
  private var applePosition: Point = gridDims.placeApple(snakeBody, random)
  private var growthRemaining: Int = 0
  private var gameOverFlag: Boolean = false

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
    // Prova 1312

    if (gameOverFlag) return

    // Process direction queue to find the last valid direction
    var newDirection = currentDirection
    for (dir <- directionQueue) {
      if (dir.isValidFrom(newDirection)) {
        newDirection = dir
      }
    }
    directionQueue = List()
    currentDirection = newDirection

    // Calculate new head position
    val currentHead = snakeBody.head
    val newHead = currentHead.move(currentDirection, gridDims)

    // Check for collision with body
    // Special case: if we're not growing and the new head position is the current tail,
    // this is OK (the tail will move away)
    val willCollide = if (growthRemaining > 0) {
      snakeBody.contains(newHead)
    } else {
      // Check collision with all body parts except the tail
      snakeBody.init.contains(newHead)
    }

    if (willCollide) {
      gameOverFlag = true
      return
    }

    // Check if apple was eaten BEFORE moving
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
      growthRemaining += Apple().growthValue
      applePosition = gridDims.placeApple(snakeBody, random)
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