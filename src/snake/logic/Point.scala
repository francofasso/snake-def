package snake.logic

// you can alter this file!
// case class Point(x: Int, y : Int)
// means
// * x and y are vals (cannot be changed)
// * You can do  Point(2,3) instead of new Point(2,3)
// * == compares x and y values and does not use reference equality
// * toString is implemented as you expect ("Point(2,3)")
//
// This is explained in lecture 4
case class Point(x : Int, y : Int) {
  def move(direction: Direction, bounds: Dimensions): Point = {
    direction match {
      case East() => Point((x + 1) % bounds.width, y)
      case West() => Point((x - 1 + bounds.width) % bounds.width, y)
      case North() => Point(x, (y - 1 + bounds.height) % bounds.height)
      case South() => Point(x, (y + 1) % bounds.height)
    }
  }
}