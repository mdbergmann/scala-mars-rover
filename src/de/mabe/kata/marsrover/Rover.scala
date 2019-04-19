package de.mabe.kata.marsrover

/**
  * @since 2019-04-13
  */
class Rover() {
  import Direction._
  import Heading._

  private var _position: Position = Position(1, 1)
  private var _heading: Heading = North
  private var _grid: Grid = Grid(10, 10)
  private var _obstacles: List[Position] = Nil

  def position: Position = _position
  def heading: Heading = _heading

  def init(startingCoords: (Int, Int),
           startHeading: Char,
           startGrid: (Int, Int) = (10, 10),
           obstacles: List[(Int, Int)] = Nil): Unit = {
    _position = Position(startingCoords._1, startingCoords._2)
    _heading = Heading(startHeading)
    _grid = Grid(startGrid._1, startGrid._2)
    _obstacles = obstacles.map(p => Position(p._1, p._2))
  }

  def executeCmds(cmds: Array[Char]): Option[(Int, Int)] = {
    try {
      cmds.foreach {
        case cmd if cmd == 'f' || cmd == 'b' => _position = detectObstacle(move(Direction(cmd)))
        case cmd if cmd == 'l' || cmd == 'r' => _heading = turn(Direction(cmd))
        case _ => ()
      }
      None
    } catch {
      case oe: ObstacleException => Some((oe.position.x, oe.position.y))
      case _ => None
    }
  }

  @throws[ObstacleException]
  private def detectObstacle(position: Position): Position = {
    if(_obstacles.exists(obstacle => obstacle.x == position.x && obstacle.y == position.y)) {
      throw new ObstacleException(position)
    }
    position
  }

  private def turn(direction: Direction): Heading = {
    heading match {
      case North => if(direction == Left) West else East
      case West => if(direction == Left) South else North
      case South => if(direction == Left) East else West
      case East => if(direction == Left) North else South
    }
  }

  private def move(direction: Direction): Position = {
    val newPos = heading match {
      case North => if(direction == Forward) incYPosition() else decYPosition()
      case South => if(direction == Forward) decYPosition() else incYPosition()
      case East => if(direction == Forward) incXPosition() else decXPosition()
      case West => if(direction == Forward) decXPosition() else incXPosition()
      case _ => Position(_position.x, _position.y)
    }

    fixPosition(newPos)
  }

  private def fixPosition(position: Position): Position = {
    if(position.x > _grid.width) return position.copy(x = 1)
    if(position.x <= 0) return position.copy(x = _grid.width)
    if(position.y > _grid.height) return position.copy(y = 1)
    if(position.y <= 0) return position.copy(y = _grid.height)

    position.copy()
  }

  private def incXPosition() = {
    Position(position.x+1, position.y)
  }

  private def decXPosition() = {
    Position(position.x-1, position.y)
  }

  private def decYPosition() = {
    Position(position.x, position.y - 1)
  }

  private def incYPosition() = {
    Position(position.x, position.y + 1)
  }
}

class ObstacleException(val position: Position) extends RuntimeException

case class Heading(value: Char)
object Heading {
  val North = Heading('N')
  val South = Heading('S')
  val East = Heading('E')
  val West = Heading('W')
}

case class Direction(dir: Char)
object Direction {
  val Forward = Direction('f')
  val Backward = Direction('b')
  val Left = Direction('l')
}

case class Position(x: Int, y: Int)
case class Grid(width: Int, height: Int)
