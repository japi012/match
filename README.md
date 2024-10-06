### `match`
An esoteric programming language based on functional programming, leaveraging ideas like *pattern matching* and *pairs*.

## Installation
The `match` interpreter is just a Rust crate, so download the Rust compiler. Then you can clone this repo and do `cargo run` to execute it.
```
cargo run -- <filename>
```
The standalone executable can be found at `target/debug/match-lang` after running `cargo build` or `cargo run`.

## Language
# Function syntax
A `match` program is made up of *functions*. All functions take one argument, and return one value.
Functions in `match` is defined by their name followed by a colon and then by a series of pattern branches.
Each branch consists of a pattern, an `->`, an expression to evaluate, and a `;`. The whole function is terminated by another `;`.

For example, here's a function that adds one to the argument given to it.
```
add_one:
    number -> number + 1;;
```
The `number` variable in the pattern binds to whatever is passed into the function and returns the value plus one.

Here's a `truth-machine`:
```
truth:
    0 -> println(0);
    1 -> print(1) then truth(1);;
```
The `then` operator throws away the result of the left hand side, and returns the right hand side.
As you can see, this function is recursive. This also showcases the `print` and `println` functions.
