# Mars Rover

You’re part of the team that explores Mars by sending remotely controlled vehicles to the surface of the planet. Develop an API that translates the commands sent from earth to instructions that are understood by the rover.

Requirements

- [ ] You are given the initial starting point (x,y) of a rover and the direction (N,S,E,W) it is facing.
- [ ] The rover receives a character array of commands.
- [ ] Implement commands that move the rover forward/backward (f,b).
- [ ] Implement commands that turn the rover left/right (l,r).
- [ ] Implement wrapping from one edge of the grid to another. (planets are spheres after all)
- [ ] Implement obstacle detection before each move to a new square. If a given sequence of commands encounters an obstacle, the rover moves up to the last possible point, aborts the sequence and reports the obstacle.


# About Rust

To compile and run project's tests you should have a Rust toolchain installed. Use [rustup](https://www.rustup.rs/) to
install toolchain and then `cargo` to compile, run test and produce docs (very limited).


By the toolchain installed use

* `cargo test` to run both unit test and doctest (thats akts also as integration tests)
* `cargo docs` to create the (limited) Documentation at `target/doc/mars_rover/index.html`

Let me known if you have any issue...
