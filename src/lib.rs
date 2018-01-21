#[macro_use]
extern crate log;

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum Direction {
    N,
    S,
    E,
    W,
}

impl Default for Direction {
    fn default() -> Self {
        Direction::N
    }
}

impl Direction {
    fn reversed(&self) -> Self {
        use Direction::*;

        match *self {
            N => S,
            S => N,
            E => W,
            W => E,
        }
    }

    fn right(&self) -> Self {
        use Direction::*;

        match *self {
            N => E,
            S => W,
            E => S,
            W => N,
        }
    }

    fn left(&self) -> Self {
        use Direction::*;

        match *self {
            N => W,
            S => E,
            E => N,
            W => S,
        }
    }
}

impl std::fmt::Display for Direction {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use Direction::*;

        write!(f, "{}",
               match *self {
                   N => "North",
                   S => "South",
                   E => "East",
                   W => "West",
               }
        )
    }
}

pub type Position = i32;

#[derive(Default, Debug, Clone, Eq, PartialEq)]
pub struct Coordinate(pub Position, pub Position);

fn move_position(coord: &Coordinate, direction: &Direction) -> Coordinate {
    use Direction::*;

    match *direction {
        N => Coordinate(coord.0, coord.1 + 1),
        S => Coordinate(coord.0, coord.1 - 1),
        E => Coordinate(coord.0 + 1, coord.1),
        W => Coordinate(coord.0 - 1, coord.1),
    }
}

pub trait World: std::fmt::Debug {
    fn move_to(&self, coord: &Coordinate, direction: &Direction) -> Coordinate {
        self.normalize_coord(move_position(coord, direction))
    }


    fn normalize_coord(&self, coord: Coordinate) -> Coordinate;
}

#[derive(Debug)]
struct InfinityWorld {}

impl World for InfinityWorld {
    fn normalize_coord(&self, coord: Coordinate) -> Coordinate {
        coord
    }
}


static DEFAULT_WORLD: InfinityWorld = InfinityWorld {};

#[derive(Debug)]
pub struct BoundedWorld {
    width: Position,
    height: Position
}

impl BoundedWorld {
    pub fn new(width: usize, height: usize) -> Self {
        Self { width: width as Position, height: height as Position }
    }
}

impl World for BoundedWorld {
    fn normalize_coord(&self, coord: Coordinate) -> Coordinate {
        Coordinate(coord.0 % self.width, coord.1 % self.height)
    }
}

#[derive(Debug)]
pub struct Rover<'a> {
    coord: Coordinate,
    direction: Direction,
    world: Box<&'a World>,
}

impl<'a> Rover<'a> {
    pub fn coord(&self) -> &Coordinate {
        &self.coord
    }

    pub fn direction(&self) -> &Direction {
        &self.direction
    }

    fn apply(&mut self, cmd: Command) {
        use Command::*;
        match cmd {
            Forward => { self.coord = self.world.move_to(&self.coord, &self.direction) }
            Backward => { self.coord = self.world.move_to(&self.coord, &self.direction.reversed()) }
            Right => { self.direction = self.direction.right() }
            Left => { self.direction = self.direction.left() }
        }
    }
}

#[derive(Copy, Clone)]
enum Command {
    Forward,
    Backward,
    Right,
    Left,
}

impl std::fmt::Display for Command {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use Command::*;

        write!(f, "{}",
               match *self {
                   Forward => "Forward",
                   Backward => "Backward",
                   Right => "Right",
                   Left => "Left",
               }
        )
    }
}

/// Control the [Rover](struct.Rover.html) by char commands.
///
/// # Examples
///
/// ```
/// use mars_rover::{RoverCharConsole, Coordinate};
///
/// # use mars_rover::{Lander, Direction};
/// # let rover = Lander::new().coord(10, 3).direction(Direction::N).land();
///
/// let mut controller = RoverCharConsole::new(rover);
///
/// controller.send(&['f', 'f', 'b', 'r', 'b', 'b', 'l', 'f', 'f']);
///
/// assert_eq!(&Coordinate(8, 6), controller.rover().coord());
/// ```
///
pub struct RoverCharConsole<'a> {
    rover: Rover<'a>
}

impl<'a> RoverCharConsole<'a> {
    pub fn new(rover: Rover<'a>) -> Self {
        Self { rover }
    }

    pub fn send<C: AsRef<[char]>>(&mut self, commands: C) {
        use Command::*;

        for c in commands.as_ref() {
            info!("Apply char command {} to rover {:?}", c, self.rover);
            match *c {
                'f' => self.rover.apply(Forward),
                'b' => self.rover.apply(Backward),
                'r' => self.rover.apply(Right),
                'l' => self.rover.apply(Left),
                unknown => { warn!("Unknown char command '{}'", unknown) }
            }
        }
    }

    pub fn rover(&self) -> &Rover {
        &self.rover
    }
}

impl<'a> From<Rover<'a>> for RoverCharConsole<'a> {
    fn from(rover: Rover<'a>) -> Self {
        RoverCharConsole::new(rover)
    }
}

/// Land the [Rover](struct.Rover.html) at the [World](trait.World.html) to configured
/// [Coordinate](struct.Coordinate.html) and [Direction](enum.Direction.html). The default
/// world is [InfinityWorld](struct.InfinityWorld.html).
///
/// # Examples
///
/// ```
/// use mars_rover::{Lander, Coordinate, Direction};
///
/// let rover = Lander::new()
///         .coord(3, 4)
///         .direction(Direction::N)
///         .land();
///
/// assert_eq!(&Coordinate(3, 4), rover.coord());
/// assert_eq!(&Direction::N, rover.direction());
/// ```
#[derive(Default)]
pub struct Lander<'a> {
    coord: Coordinate,
    direction: Direction,
    world: Box<&'a World>,
}

impl<'a> Default for &'a World {
    fn default() -> Self {
        &DEFAULT_WORLD as &World
    }
}

impl<'a> Lander<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn land(&self) -> Rover<'a> {
        Rover {
            coord: self.world.normalize_coord(self.coord.clone()),
            direction: self.direction.clone(),
            world: self.world.clone(),
        }
    }

    /// Select the [World](trait.World.html) where landing.
    ///
    /// # Examples
    /// ```
    /// use mars_rover::{Lander, Coordinate, BoundedWorld};
    ///
    /// let bounded_world = BoundedWorld::new(5, 12);
    /// let rover = Lander::new()
    ///         .world(&bounded_world)
    ///         .coord(12, 23)
    ///         .land();
    ///
    /// assert_eq!(&Coordinate(2, 11), rover.coord());
    /// ```
    pub fn world(&mut self, world: &'a World) -> &mut Self {
        self.world = Box::new(world);
        self
    }

    pub fn coord(&mut self, x: Position, y: Position) -> &mut Self {
        self.coord = Coordinate(x, y);
        self
    }

    pub fn direction(&mut self, direction: Direction) -> &mut Self {
        self.direction = direction;
        self
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    /// Useful syntactic sugar for testing
    impl From<(Position, Position)> for Coordinate {
        fn from(c: (Position, Position)) -> Self {
            Coordinate(c.0, c.1)
        }
    }

    mod rover {
        use super::*;
        use Direction::*;

        #[test]
        fn should_move_forward() {
            let p = (2, 5);
            for (d, expected) in
                vec![
                    (N, (2, 6)),
                    (S, (2, 4)),
                    (E, (3, 5)),
                    (W, (1, 5)),
                ]
                {
                    let mut rover = Lander::new().coord(p.0, p.1).direction(d).land();
                    let cmd = Command::Forward;

                    rover.apply(cmd);

                    assert_eq!(Coordinate::from(expected), rover.coord,
                               "Wrong destination != {:?} from [{:?}, {}] by apply {}", expected, p, d, cmd);
                    assert_eq!(d, rover.direction, "Apply {} should not change direction", cmd);
                }
        }

        #[test]
        fn should_move_backward() {
            let p = (2, 5);
            for (d, expected) in
                vec![
                    (N, (2, 4)),
                    (S, (2, 6)),
                    (E, (1, 5)),
                    (W, (3, 5)),
                ]
                {
                    let mut rover = Lander::new().coord(p.0, p.1).direction(d).land();
                    let cmd = Command::Backward;

                    rover.apply(cmd);

                    assert_eq!(Coordinate::from(expected), rover.coord,
                               "Wrong destination != {:?} from [{:?}, {}] by apply {}", expected, p, d, cmd);
                    assert_eq!(d, rover.direction, "Apply {} should not change direction", cmd);
                }
        }

        #[test]
        fn should_rotate_right() {
            for (d, expected) in
                vec![
                    (N, E),
                    (S, W),
                    (E, S),
                    (W, N),
                ]
                {
                    let mut rover = Lander::new().coord(4, 5).direction(d).land();
                    let cmd = Command::Right;

                    rover.apply(cmd);

                    assert_eq!(expected, rover.direction,
                               "Wrong direction != {} from [{}] by apply {}", expected, d, cmd);
                }
        }

        #[test]
        fn should_rotate_left() {
            for (d, expected) in
                vec![
                    (N, W),
                    (S, E),
                    (E, N),
                    (W, S),
                ]
                {
                    let mut rover = Lander::new().coord(4, 5).direction(d).land();
                    let cmd = Command::Left;

                    rover.apply(cmd);

                    assert_eq!(expected, rover.direction,
                               "Wrong direction != {} from [{}] by apply {}", expected, d, cmd);
                }
        }
    }

    mod rover_controller {
        use super::*;

        fn controller(x: Position, y: Position, d: Direction) -> RoverCharConsole<'static> {
            let rover = Lander::new().coord(x, y).direction(d).land();
            rover.into()
        }

        #[test]
        fn should_send_forward_command() {
            let mut controller = controller(10, 3, Direction::W);

            controller.send(&['f']);

            assert_eq!(&Coordinate(9, 3), controller.rover().coord())
        }

        #[test]
        fn should_send_backward_command() {
            let mut controller = controller(7, 12, Direction::S);

            controller.send(&['b']);

            assert_eq!(&Coordinate(7, 13), controller.rover().coord())
        }

        #[test]
        fn should_send_right_command() {
            let mut controller = controller(10, 3, Direction::W);

            controller.send(&['r']);

            assert_eq!(&Direction::N, controller.rover().direction())
        }

        #[test]
        fn should_send_left_command() {
            let mut controller = controller(10, 3, Direction::S);

            controller.send(&['l']);

            assert_eq!(&Direction::E, controller.rover().direction())
        }

        #[test]
        fn should_ignore_unknown_commands() {
            let mut controller = controller(7, 12, Direction::S);

            controller.send(['a', '🎁']);

            assert_eq!(&Coordinate(7, 12), controller.rover().coord())
        }

        #[test]
        fn should_ignore_just_commands() {
            let mut controller = controller(7, 12, Direction::S);

            controller.send(['☙', '❣', 'f', 't']);

            assert_eq!(&Coordinate(7, 11), controller.rover().coord())
        }
    }
}
