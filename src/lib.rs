#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Directions{
    N,
    S,
    E,
    W
}

impl Default for Directions {
    fn default() -> Self {
        Directions::N
    }
}

pub type Position = i32;

#[derive(Default, Debug, Clone, Eq, PartialEq)]
pub struct Coordinates(pub Position, pub Position);

pub struct Rover {
    coord: Coordinates,
    direction: Directions
}

impl Rover {
    pub fn coord(&self) -> &Coordinates {
        &self.coord
    }

    pub fn direction(&self) -> &Directions {
        &self.direction
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
pub struct Lander{
    coord: Coordinates,
    direction: Directions
}

impl Lander {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn land(&self) -> Rover {
        Rover {coord: self.coord.clone(), direction: self.direction.clone()}
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
    impl<'a> PartialEq<&'a Directions> for Directions {
        fn eq(&self, other: &&'a Directions) -> bool {
            self == *other
        }
    }

    impl<'a> PartialEq<Directions> for &'a Directions {
        fn eq(&self, other: &Directions) -> bool {
            *self == other
        }
    }

    impl<'a> PartialEq<&'a Coordinates> for Coordinates {
        fn eq(&self, other: &&'a Coordinates) -> bool {
            self == *other
        }
    }

    impl<'a> PartialEq<Coordinates> for &'a Coordinates {
        fn eq(&self, other: &Coordinates) -> bool {
            *self == other
        }
    }
}
