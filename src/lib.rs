#[macro_use]
extern crate log;
#[allow(unused_imports)]
#[macro_use]
extern crate lazy_static;

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

#[derive(Default, Debug, Clone, Eq, PartialEq, Hash)]
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

    fn is_free_cell(&self, _coord: &Coordinate) -> bool {
        true
    }
}

#[derive(Debug)]
struct InfinityWorld {}

impl World for InfinityWorld {
    fn normalize_coord(&self, coord: Coordinate) -> Coordinate {
        coord
    }
}


static DEFAULT_WORLD: InfinityWorld = InfinityWorld {};

use std::collections::HashSet;

#[derive(Debug)]
pub struct BoundedWorld {
    width: Position,
    height: Position,
    obstacles: std::collections::HashSet<Coordinate>,
}

impl BoundedWorld {
    pub fn new(width: usize, height: usize) -> Self {
        Self { width: width as Position, height: height as Position, obstacles: HashSet::default() }
    }

    pub fn add_obstacle(&mut self, coord: Coordinate) {
        self.obstacles.insert(coord);
    }

    #[inline]
    fn norm_position(pos: Position, bound: Position) -> Position {
        (pos % bound + bound) % bound
    }
}

impl World for BoundedWorld {
    fn normalize_coord(&self, coord: Coordinate) -> Coordinate {
        // Rust module is just a remainder
        Coordinate(Self::norm_position(coord.0, self.width),
                   Self::norm_position(coord.1, self.height),
        )
    }

    fn is_free_cell(&self, coord: &Coordinate) -> bool {
        !self.obstacles.contains(coord)
    }
}

#[derive(Debug)]
pub struct Rover<'a> {
    coord: Coordinate,
    direction: Direction,
    world: Box<&'a World>,
}

type RoverResult = Result<(), String>;

impl<'a> Rover<'a> {
    pub fn coord(&self) -> &Coordinate {
        &self.coord
    }

    pub fn direction(&self) -> &Direction {
        &self.direction
    }

    fn forward(&mut self) -> RoverResult {
        self.coord = self.try_move(self.direction)?;
        Ok(())
    }

    fn backward(&mut self) -> RoverResult {
        self.coord = self.try_move(self.direction.reversed())?;
        Ok(())
    }

    fn right(&mut self) {
        self.direction = self.direction.right()
    }

    fn left(&mut self) {
        self.direction = self.direction.left()
    }

    fn try_move(&self, direction: Direction) -> Result<Coordinate, String> {
        let coord = self.world.move_to(&self.coord, &direction);
        match self.world.is_free_cell(&coord) {
            true => Ok(coord),
            false => Err(format!("Cannot move in {:?} on {:?}", coord, self.world))
        }
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
/// controller.send(&['f', 'f', 'b', 'r', 'b', 'b', 'l', 'f', 'f']).unwrap();
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

    pub fn send<C: AsRef<[char]>>(&mut self, commands: C) -> RoverResult {

        for c in commands.as_ref() {
            info!("Apply char command {} to rover {:?}", c, self.rover);
            match *c {
                'f' => self.rover.forward()?,
                'b' => self.rover.backward()?,
                'r' => self.rover.right(),
                'l' => self.rover.left(),
                unknown => {
                    warn!("Unknown char command '{}'", unknown);
                }
            };
        }
        Ok(())
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

                    rover.forward().unwrap();

                    assert_eq!(Coordinate::from(expected), rover.coord,
                               "Wrong destination != {:?} from [{:?}, {}] by move forward", expected, p, d);
                    assert_eq!(d, rover.direction, "Move forward should not change direction");
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

                    rover.backward().unwrap();

                    assert_eq!(Coordinate::from(expected), rover.coord,
                               "Wrong destination != {:?} from [{:?}, {}] by move backward", expected, p, d);
                    assert_eq!(d, rover.direction, "Move backward should not change direction");
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

                    rover.right();

                    assert_eq!(expected, rover.direction,
                               "Wrong direction != {} from [{}] to rotate right", expected, d);
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

                    rover.left();

                    assert_eq!(expected, rover.direction,
                               "Wrong direction != {} from [{}] by rotate left", expected, d);
                }
        }

        #[test]
        fn report_an_error_if_found_an_obstacle() {
            let mut world = BoundedWorld::new(10, 8);
            world.add_obstacle(Coordinate(3, 6));

            let mut rover = Lander::new()
                .world(&world)
                .coord(3, 5)
                .direction(Direction::N)
                .land();

            assert!(rover.forward().is_err());

            // Sanity check
            assert!(rover.backward().is_ok());
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

            controller.send(&['f']).unwrap();

            assert_eq!(&Coordinate(9, 3), controller.rover().coord())
        }

        #[test]
        fn should_send_backward_command() {
            let mut controller = controller(7, 12, Direction::S);

            controller.send(&['b']).unwrap();

            assert_eq!(&Coordinate(7, 13), controller.rover().coord())
        }

        #[test]
        fn should_send_right_command() {
            let mut controller = controller(10, 3, Direction::W);

            controller.send(&['r']).unwrap();

            assert_eq!(&Direction::N, controller.rover().direction())
        }

        #[test]
        fn should_send_left_command() {
            let mut controller = controller(10, 3, Direction::S);

            controller.send(&['l']).unwrap();

            assert_eq!(&Direction::E, controller.rover().direction())
        }

        #[test]
        fn should_ignore_unknown_commands() {
            let mut controller = controller(7, 12, Direction::S);

            controller.send(['a', '🎁']).unwrap();

            assert_eq!(&Coordinate(7, 12), controller.rover().coord())
        }

        #[test]
        fn should_ignore_just_commands() {
            let mut controller = controller(7, 12, Direction::S);

            controller.send(['☙', '❣', 'f', 't']).unwrap();

            assert_eq!(&Coordinate(7, 11), controller.rover().coord())
        }

        fn world_controller(world: &World, x: Position, y: Position, d: Direction) -> RoverCharConsole {
            let rover = Lander::new().world(world).coord(x, y).direction(d).land();
            rover.into()
        }

        #[test]
        fn should_report_an_obstacle_and_stop_the_rover() {
            let mut world = BoundedWorld::new(5, 6);
            world.add_obstacle(Coordinate(4, 4));

            let mut controller = world_controller(&world, 3, 3, Direction::N);

            assert!(controller.send(&['f', 'r', 'f', 'f', 'f', 'r']).is_err());

            assert_eq!(&Coordinate(3, 4), controller.rover().coord());
            assert_eq!(&Direction::E, controller.rover().direction());
        }

        #[test]
        fn should_handle_more_obstacles() {
            let mut world = BoundedWorld::new(7, 8);
            world.add_obstacle(Coordinate(4, 4));
            world.add_obstacle(Coordinate(5, 4));
            world.add_obstacle(Coordinate(5, 3));

            let mut controller = world_controller(&world, 3, 3, Direction::E);

            assert!(controller.send(&['f', 'f', 'f']).is_err());

            assert_eq!(&Coordinate(4, 3), controller.rover().coord());

            assert!(controller.send(&['l', 'f', 'f']).is_err());

            assert_eq!(&Coordinate(4, 3), controller.rover().coord());

            assert!(controller.send(&['b', 'b']).is_ok());
            assert_eq!(&Coordinate(4, 1), controller.rover().coord());
        }
    }

    mod bounded_world {
        use super::*;
        lazy_static! {
            static ref BOUNDED_WORLD: BoundedWorld = {
                let b = BoundedWorld::new(6, 4);
                b
            };
        }

        fn controller(x: Position, y: Position, d: Direction) -> RoverCharConsole<'static> {
            let rover = Lander::new().world(&*BOUNDED_WORLD).coord(x, y).direction(d).land();
            rover.into()
        }

        #[test]
        fn should_wrapping_at_north_bound() {
            let mut controller = controller(3, 2, Direction::N);

            controller.send(&['f', 'f']).unwrap();

            assert_eq!(&Coordinate(3, 0), controller.rover().coord())
        }

        #[test]
        fn should_wrapping_at_south_bound() {
            let mut controller = controller(3, 2, Direction::S);

            controller.send(&['f', 'f', 'f']).unwrap();

            assert_eq!(&Coordinate(3, 3), controller.rover().coord())
        }

        #[test]
        fn should_wrapping_at_east_bound() {
            let mut controller = controller(3, 2, Direction::E);

            controller.send(&['f', 'f', 'f']).unwrap();

            assert_eq!(&Coordinate(0, 2), controller.rover().coord())
        }

        #[test]
        fn should_wrapping_at_west_bound() {
            let mut controller = controller(3, 2, Direction::W);

            controller.send(&['f', 'f', 'f', 'f']).unwrap();

            assert_eq!(&Coordinate(5, 2), controller.rover().coord())
        }
    }
}
