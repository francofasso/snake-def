package snake.logic

import engine.random.RandomGenerator
import snake.logic.GameLogic._

// Immutable game state
case class GameState(
                      snakeBody: List[Point],
                      direction: Direction,
                      applePosition: Point,
                      growthRemaining: Int,
                      gameOver: Boolean,
                      randomSeed: Int = 0
                    ) {

  def step(newDirection: Direction, random: RandomGenerator, gridDims: Dimensions): GameState = {
    if (gameOver) return this

<<<<<<< HEAD
=======
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

>>>>>>> 87c33a93297436c8d0c4ec5d2fcf8c70524baa4c
    val currentHead = snakeBody.head
    val newHead = movePoint(currentHead, newDirection, gridDims)

<<<<<<< HEAD
    // Check collision - if growing, check all body; if not growing, exclude tail
=======
>>>>>>> 87c33a93297436c8d0c4ec5d2fcf8c70524baa4c
    val willCollide = if (growthRemaining > 0) {
      // If it grows, check all body
      snakeBody.contains(newHead)
    } else {
<<<<<<< HEAD
=======
      // If it doesn't grow, don't check the tail
>>>>>>> 87c33a93297436c8d0c4ec5d2fcf8c70524baa4c
      snakeBody.init.contains(newHead)
    }

    if (willCollide) {
      return this.copy(gameOver = true, direction = newDirection)
    }

<<<<<<< HEAD
    // Check if apple was eaten
=======
    // Check if apple was eaten, but before moving
>>>>>>> 87c33a93297436c8d0c4ec5d2fcf8c70524baa4c
    val ateApple = newHead == applePosition

    // Move snake - add new head
    var newSnakeBody = newHead :: snakeBody

    // Handle growth/shrinking
    var newGrowth = growthRemaining
    if (growthRemaining > 0) {
      newGrowth -= 1
    } else {
      // Remove tail if not growing
      if (newSnakeBody.length > 1) {
        newSnakeBody = newSnakeBody.init
      }
    }

    // If apple was eaten, generate new apple position and start growing
    var newApplePos = applePosition
    if (ateApple) {
      newGrowth += 3
      newApplePos = generateApplePosition(newSnakeBody, random, gridDims)
    }

    GameState(
      snakeBody = newSnakeBody,
      direction = newDirection,
      applePosition = newApplePos,
      growthRemaining = newGrowth,
      gameOver = false
    )
  }

  private def movePoint(point: Point, dir: Direction, gridDims: Dimensions): Point = {
    dir match {
      case East() => Point((point.x + 1) % gridDims.width, point.y)
      case West() => Point((point.x - 1 + gridDims.width) % gridDims.width, point.y)
      case North() => Point(point.x, (point.y - 1 + gridDims.height) % gridDims.height)
      case South() => Point(point.x, (point.y + 1) % gridDims.height)
    }
  }

  private def generateApplePosition(snake: List[Point], random: RandomGenerator, gridDims: Dimensions): Point = {
    val allPoints = gridDims.allPointsInside
    val freePoints = allPoints.filterNot(p => snake.contains(p))

    if (freePoints.isEmpty) {
      null
    } else {
      val index = random.randomInt(freePoints.length)
      freePoints(index)
    }
  }
}

class GameLogic(val random: RandomGenerator, val gridDims: Dimensions) {

  // Initial game state
  private val initialState = GameState(
    snakeBody = List(Point(2, 0), Point(1, 0), Point(0, 0)),
    direction = East(),
    applePosition = generateInitialApple(),
    growthRemaining = 0,
    gameOver = false
  )

  // These are our allowed vars according to requirements
  private var currentState: GameState = initialState
  private var stateHistory: List[GameState] = List(initialState)
  private var reverseMode: Boolean = false
  private var directionQueue: List[Direction] = List()

  private def generateInitialApple(): Point = {
    val initialSnake = List(Point(2, 0), Point(1, 0), Point(0, 0))
    val allPoints = gridDims.allPointsInside
    val freePoints = allPoints.filterNot(p => initialSnake.contains(p))

    if (freePoints.isEmpty) {
      null
    } else {
      val index = random.randomInt(freePoints.length)
      freePoints(index)
    }
  }

  def getCellType(p: Point): CellType = {
    val state = currentState
    if (state.snakeBody.nonEmpty && state.snakeBody.head == p) {
      SnakeHead(state.direction)
    } else if (state.snakeBody.contains(p)) {
      val index = state.snakeBody.indexOf(p)
      val distance = if (state.snakeBody.length <= 1) 0f else index.toFloat / (state.snakeBody.length - 1).toFloat
      SnakeBody(distance)
    } else if (p == state.applePosition && state.applePosition != null) {
      Apple()
    } else {
      Empty()
    }
  }

  def step(): Unit = {
    if (reverseMode) {
      // In reverse mode, go back one state if possible
      if (stateHistory.length > 1) {
        // Remove the current state from history
        stateHistory = stateHistory.tail
        // Set current state to the previous state
        currentState = stateHistory.head
      }
      // Clear direction queue when reversing
      directionQueue = List()
    } else {
      // Normal forward mode
      if (!currentState.gameOver) {
        // Process direction queue - take first valid direction
        val newDirection = directionQueue
          .find(_ != currentState.direction.opposite)
          .getOrElse(currentState.direction)

        // Clear the queue after processing
        directionQueue = List()

        // Generate new state
        val newState = currentState.step(newDirection, random, gridDims)

        // Update current state and add to history
        currentState = newState
        stateHistory = newState :: stateHistory
      }
    }
  }

  def changeDir(d: Direction): Unit = {
    if (!currentState.gameOver) {
      directionQueue = directionQueue :+ d
    }
  }

  def gameOver: Boolean = currentState.gameOver

  def setReverse(r: Boolean): Unit = {
    reverseMode = r
    if (!r) {
      // When exiting reverse mode, clear direction queue
      directionQueue = List()
    }
  }
}

object GameLogic {
  val FramesPerSecond: Int = 5
  val DrawSizeFactor = 1.0
  val DefaultGridDims: Dimensions = Dimensions(width = 25, height = 25)
}
