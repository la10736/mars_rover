#[macro_use]
extern crate log;

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum Directions {
    N,
    S,
    E,
    W,
}

impl Default for Directions {
    fn default() -> Self {
        Directions::N
    }
}

impl Directions {
    fn reversed(&self) -> Self {
        use Directions::*;

        match *self {
            N => S,
            S => N,
            E => W,
            W => E,
        }
    }
}

impl std::fmt::Display for Directions {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use Directions::*;

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
pub struct Coordinates(pub Position, pub Position);

fn move_position(coord: &Coordinates, direction: &Directions) -> Coordinates {
    use Directions::*;

    match *direction {
        N => Coordinates(coord.0, coord.1 + 1),
        S => Coordinates(coord.0, coord.1 - 1),
        E => Coordinates(coord.0 + 1, coord.1),
        W => Coordinates(coord.0 - 1, coord.1),
    }
}

#[derive(Debug)]
pub struct Rover {
    coord: Coordinates,
    direction: Directions,
}

impl Rover {
    pub fn coord(&self) -> &Coordinates {
        &self.coord
    }

    pub fn direction(&self) -> &Directions {
        &self.direction
    }

    fn apply(&mut self, cmd: Command) {
        use Command::*;
        match cmd {
            Forward => { self.coord = move_position(&self.coord, &self.direction) }
            Backward => { self.coord = move_position(&self.coord, &self.direction.reversed()) }
        }
    }
}

#[derive(Copy, Clone)]
enum Command {
    Forward,
    Backward,
}

impl std::fmt::Display for Command {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use Command::*;

        write!(f, "{}",
               match *self {
                   Forward => "Forward",
                   Backward => "Backward"
               }
        )
    }
}

/// Control the [Rover](struct.Rover.html) by char commands.
///
/// # Examples
///
/// ```
/// use mars_rover::{RoverCharConsole, Coordinates};
///
/// # use mars_rover::{Lander, Directions};
/// # let rover = Lander::new().coord(10, 3).direction(Directions::N).land();
///
/// let mut controller = RoverCharConsole::new(rover);
///
/// controller.send(&['f', 'f', 'b', 'f']);
///
/// assert_eq!(&Coordinates(10, 5), controller.rover().coord());
/// ```
///
pub struct RoverCharConsole {
    rover: Rover
}

impl RoverCharConsole {
    pub fn new(rover: Rover) -> Self {
        Self { rover }
    }

    pub fn send<C: AsRef<[char]>>(&mut self, commands: C) {
        use Command::*;

        for c in commands.as_ref() {
            info!("Apply char command {} to rover {:?}", c, self.rover);
            match *c {
                'f' => self.rover.apply(Forward),
                'b' => self.rover.apply(Backward),
                unknown => { warn!("Unknown char command '{}'", unknown)}
            }
        }
    }

    pub fn rover(&self) -> &Rover {
        &self.rover
    }
}

impl From<Rover> for RoverCharConsole {
    fn from(rover: Rover) -> Self {
        RoverCharConsole::new(rover)
    }
}

/// Land the [Rover](struct.Rover.html) to configured [Coordinates](struct.Coordinates.html) and
/// [Directions](enum.Directions.html)
///
/// # Examples
///
/// ```
/// use mars_rover::{Lander, Coordinates, Directions};
///
/// let rover = Lander::new()
///         .coord(3, 4)
///         .direction(Directions::N)
///         .land();
///
/// assert_eq!(&Coordinates(3, 4), rover.coord());
/// assert_eq!(&Directions::N, rover.direction());
/// ```
#[derive(Default)]
pub struct Lander {
    coord: Coordinates,
    direction: Directions,
}

impl Lander {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn land(&self) -> Rover {
        Rover { coord: self.coord.clone(), direction: self.direction.clone() }
    }

    pub fn coord(&mut self, x: Position, y: Position) -> &mut Self {
        self.coord = Coordinates(x, y);
        self
    }

    pub fn direction(&mut self, direction: Directions) -> &mut Self {
        self.direction = direction;
        self
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    /// Useful syntactic sugar for testing
    impl From<(Position, Position)> for Coordinates {
        fn from(c: (Position, Position)) -> Self {
            Coordinates(c.0, c.1)
        }
    }

    mod rover {
        use super::*;
        use Directions::*;

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

                    assert_eq!(Coordinates::from(expected), rover.coord,
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

                    assert_eq!(Coordinates::from(expected), rover.coord,
                               "Wrong destination != {:?} from [{:?}, {}] by apply {}", expected, p, d, cmd);
                    assert_eq!(d, rover.direction, "Apply {} should not change direction", cmd);
                }
        }
    }

    mod rover_controller {
        use super::*;

        #[test]
        fn should_send_forward_command() {
            let rover = Lander::new().coord(10, 3).direction(Directions::W).land();
            let mut controller: RoverCharConsole = rover.into();

            controller.send("f".chars().collect::<Vec<_>>());

            assert_eq!(&Coordinates(9, 3), controller.rover().coord())
        }

        #[test]
        fn should_send_backward_command() {
            let rover = Lander::new().coord(7, 12).direction(Directions::S).land();
            let mut controller: RoverCharConsole = rover.into();

            controller.send("b".chars().collect::<Vec<_>>());

            assert_eq!(&Coordinates(7, 13), controller.rover().coord())
        }
    }
}
