package snake.logic

import engine.random.RandomGenerator
import snake.logic.GameLogic._

// Immutable game state
case class GameState(
                      snakeBody: List[Point],
                      direction: Direction,
                      applePosition: Point,
                      growthRemaining: Int,
                      gameOver: Boolean
                    ) {

  def step(newDirection: Direction, random: RandomGenerator, gridDims: Dimensions): GameState = {
    if (gameOver) {
      // Even when game is over, we update direction for display purposes
      return this.copy(direction = newDirection)
    }

    val currentHead = snakeBody.head
    val newHead = movePoint(currentHead, newDirection, gridDims)

    // Check collision - if growing, check all body; if not growing, exclude tail
    val willCollide = if (growthRemaining > 0) {
      snakeBody.contains(newHead)
    } else {
      snakeBody.init.contains(newHead)
    }

    if (willCollide) {
      return GameState(
        snakeBody = snakeBody,
        direction = newDirection,
        applePosition = applePosition,
        growthRemaining = growthRemaining,
        gameOver = true
      )
    }

    // Check if apple was eaten
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

  // These are our vars - using a simple list + index approach
  private var allStates: List[GameState] = List(initialState)
  private var currentIndex: Int = 0
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
    val state = allStates(currentIndex)
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
      if (currentIndex > 0) {
        currentIndex -= 1
      }
      // Clear direction queue when reversing
      directionQueue = List()
    } else {
      // Normal forward mode
      val currentState = allStates(currentIndex)

      // Process direction queue - take ONLY the first valid direction
      val newDirection = directionQueue
        .find(d => d != currentState.direction.opposite)
        .getOrElse(currentState.direction)

      // Clear the queue after processing
      directionQueue = List()

      // Generate new state
      val newState = currentState.step(newDirection, random, gridDims)

      // When moving forward from a past state (after reversing),
      // we need to truncate the future states
      if (currentIndex < allStates.length - 1) {
        // We're not at the end of history, so truncate
        allStates = allStates.take(currentIndex + 1)
      }

      // Add the new state
      allStates = allStates :+ newState
      currentIndex += 1
    }
  }

  def changeDir(d: Direction): Unit = {
    // Allow direction changes anytime except when in reverse mode
    if (!reverseMode) {
      directionQueue = directionQueue :+ d
    }
  }

  def gameOver: Boolean = allStates(currentIndex).gameOver

  def setReverse(r: Boolean): Unit = {
    reverseMode = r
    // Clear direction queue when entering or exiting reverse mode
    directionQueue = List()
  }
}

object GameLogic {
  val FramesPerSecond: Int = 5
  val DrawSizeFactor = 1.0
  val DefaultGridDims: Dimensions = Dimensions(width = 25, height = 25)
}