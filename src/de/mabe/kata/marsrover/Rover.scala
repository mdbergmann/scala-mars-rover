package de.mabe.kata.marsrover

/**
  * @since 2019-04-13
  */
class Rover {
  import Heading._
  import Direction._

  private var position: Position = Position(1, 1)
  private var heading: Heading = North
  private var grid: Grid = Grid(10, 10)
  private var obstacles: List[Position] = Nil

  def initialize(startCoords: (Int, Int),
                 startHeading: Char,
                 grid: (Int, Int),
                 obstacles: List[(Int, Int)] = Nil): (Position, Heading) = {
    this.position = Position(startCoords._1, startCoords._2)
    this.heading = Heading(startHeading)
    this.grid = Grid(grid._1, grid._2)
    this.obstacles = obstacles.map(p => Position(p._1, p._2))
    (position, heading)
  }

  private def move(dir: Direction): (Position, Boolean) = {
    require(dir == Forward || dir == Backward)

    var newPosition = heading match {
      case North =>
        if(dir == Forward) Position(position.x, position.y+1)
        else Position(position.x, position.y-1)
      case East =>
        if(dir == Forward) Position(position.x+1, position.y)
        else Position(position.x-1, position.y)
      case South =>
        if(dir == Forward) Position(position.x, position.y-1)
        else Position(position.x, position.y+1)
      case West =>
        if(dir == Forward) Position(position.x-1, position.y)
        else Position(position.x+1, position.y)
    }

    def fixOverlaps = {
      if (newPosition.x <= 0) Position(grid.width, newPosition.y)
      else if (newPosition.x > grid.width) Position(1, newPosition.y)
      else if (newPosition.y <= 0) Position(newPosition.x, grid.height)
      else if (newPosition.y > grid.height) Position(newPosition.x, 1)
      else Position(newPosition.x, newPosition.y)
    }

    newPosition = fixOverlaps

    val isObstacleInWay = obstacles.find(o => o.x == newPosition.x && o.y == newPosition.y)
    (newPosition, isObstacleInWay.isDefined)
  }

  private def turn(dir: Direction): Heading = {
    require(dir == Left || dir == Right)

    heading match {
      case North => if(dir == Left) West else East
      case West => if(dir == Left) South else North
      case South => if(dir == Left) East else West
      case East => if(dir == Left) North else South
    }
  }

  def executeCmds(cmds: Array[Char]): (Position, Heading, Option[Position]) = {
    if(cmds.isEmpty) return (position, heading, None)

    cmds.foreach {
      case cmd if cmd == 'f' || cmd == 'b' => position = {
        val newPosition = move(Direction(cmd))
        if(newPosition._2) return (position, heading, Some(newPosition._1))
        else newPosition._1
      }
      case cmd@'l' => heading = turn(Direction(cmd))
      case cmd@'r' => heading = turn(Direction(cmd))
    }

    (position, heading, None)
  }
}

case class Grid(width: Int, height: Int)

case class Position(x: Int, y: Int)

case class Direction(c: Char)
object Direction {
  val Forward = Direction('f')
  val Backward = Direction('b')
  val Left = Direction('l')
  val Right = Direction('r')
}

case class Heading(c: Char)
object Heading {
  val North = Heading('N')
  val East = Heading('E')
  val South = Heading('S')
  val West = Heading('W')
}
