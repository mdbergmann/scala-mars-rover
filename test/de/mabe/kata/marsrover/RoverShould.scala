package de.mabe.kata.marsrover

import org.scalatest.{BeforeAndAfter, WordSpec}

/**
  * @since 2019-04-13
  */
class RoverShould extends WordSpec with BeforeAndAfter {
  import Heading._

  private var rover: Rover = _

  before {
    rover = new Rover()
    rover.initialize((1, 1), 'N', (10, 10), List((1, 3)))
  }

  "initialize with starting coordinates" in {
    assert(new Rover().initialize((1, 1), 'N', (10, 10)) == (Position(1, 1), North))
  }

  "execute commands - empty" in {
    assert(rover.executeCmds(Array()) == (Position(1, 1), North, None))
  }

  "move rover" in {
    assert(rover.executeCmds(Array('f', 'f')) == (Position(1, 2), North, Some(Position(1,3))))
    assert(rover.executeCmds(Array('b', 'b')) == (Position(1, 10), North, None))

    rover.initialize((1, 1), 'E', (10, 10))
    assert(rover.executeCmds(Array('f', 'f')) == (Position(3, 1), East, None))
    assert(rover.executeCmds(Array('b', 'b')) == (Position(1, 1), East, None))

    rover.initialize((1, 1), 'S', (10, 10))
    assert(rover.executeCmds(Array('f', 'f')) == (Position(1, 9), South, None))
    assert(rover.executeCmds(Array('b', 'b')) == (Position(1, 1), South, None))

    rover.initialize((1, 1), 'W', (10, 10))
    assert(rover.executeCmds(Array('f', 'f')) == (Position(9, 1), West, None))
    assert(rover.executeCmds(Array('b', 'b')) == (Position(1, 1), West, None))

    rover.initialize((1, 9), 'N', (10, 10))
    assert(rover.executeCmds(Array('f', 'f')) == (Position(1, 1), North, None))
    assert(rover.executeCmds(Array('b', 'b')) == (Position(1, 9), North, None))

    rover.initialize((9, 1), 'E', (10, 10))
    assert(rover.executeCmds(Array('f', 'f')) == (Position(1, 1), East, None))
    assert(rover.executeCmds(Array('b', 'b')) == (Position(9, 1), East, None))
  }

  "turn rover" in {
    assert(rover.executeCmds(Array('l', 'l', 'l', 'l')) == (Position(1, 1), North, None))
    assert(rover.executeCmds(Array('r', 'r', 'r', 'r')) == (Position(1, 1), North, None))
    assert(rover.executeCmds(Array('l', 'r', 'r')) == (Position(1, 1), East, None))
  }

  "detect and report obstacles" in {
    rover.initialize((1, 1), 'N', (10, 10), List((1, 3)))
    assert(rover.executeCmds(Array('f', 'f', 'f')) == (Position(1, 2), North, Some(Position(1, 3))))
  }
}
