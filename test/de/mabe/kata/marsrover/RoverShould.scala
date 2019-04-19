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
    rover.init((1, 1), 'N')
  }

  "initialize with starting point and heading" in {
    assert(rover.position == Position(1, 1))
    assert(rover.heading == North)
  }

  "receive and execute commands - empty" in {
    rover.executeCmds(Array())

    assert(rover.position == Position(1, 1))
    assert(rover.heading == North)
  }

  "move forward/backward" in {
    // heading North
    rover.executeCmds(Array('f', 'f'))
    assert(rover.position == Position(1, 3))
    assert(rover.heading == North)

    rover.executeCmds(Array('b', 'b'))
    assert(rover.position == Position(1, 1))
    assert(rover.heading == North)

    // heading South
    rover.init((1, 1), 'S')
    rover.executeCmds(Array('f', 'f'))
    assert(rover.position == Position(1, 9))
    assert(rover.heading == South)

    rover.executeCmds(Array('b', 'b'))
    assert(rover.position == Position(1, 1))
    assert(rover.heading == South)

    // heading East
    rover.init((1, 1), 'E')
    rover.executeCmds(Array('f', 'f'))
    assert(rover.position == Position(3, 1))
    assert(rover.heading == East)

    rover.executeCmds(Array('b', 'b'))
    assert(rover.position == Position(1, 1))
    assert(rover.heading == East)

    // heading West
    rover.init((1, 1), 'W')
    rover.executeCmds(Array('f', 'f'))
    assert(rover.position == Position(9, 1))
    assert(rover.heading == West)

    rover.executeCmds(Array('b', 'b'))
    assert(rover.position == Position(1, 1))
    assert(rover.heading == West)

    // North boundary
    rover.init((1, 9), 'N')
    rover.executeCmds(Array('f', 'f'))
    assert(rover.position == Position(1, 1))

    // East boundary
    rover.init((9, 1), 'E')
    rover.executeCmds(Array('f', 'f'))
    assert(rover.position == Position(1, 1))
  }

  "turn left/right to change the heading" in {
    rover.init((1, 1), 'N')
    rover.executeCmds(Array('l', 'l', 'l', 'l'))
    assert(rover.heading == North)

    rover.init((1, 1), 'N')
    rover.executeCmds(Array('r', 'r', 'r', 'r'))
    assert(rover.heading == North)

    rover.init((1, 1), 'N')
    rover.executeCmds(Array('l', 'r', 'r'))
    assert(rover.heading == East)

    rover.init((1, 1), 'N')
    rover.executeCmds(Array('r', 'r', 'r'))
    assert(rover.heading == West)
  }

  "detect and report obstacles" in {
    rover.init((1, 1), 'N', obstacles = List((1, 3)))

    assert(rover.executeCmds(Array('f', 'f')).contains((1, 3)))
  }

}
