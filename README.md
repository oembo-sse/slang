# Slang

## Dependencies

- [Rust](https://www.rust-lang.org/tools/install)
- _Optional:_ [cargo-watch](https://github.com/watchexec/cargo-watch)
    - Can be installed with `cargo install cargo-watch`
- _Optional:_ [Node.js](https://nodejs.org/en/download/)
- _Optional:_ [Just](https://github.com/casey/just?tab=readme-ov-file#packages)
    - Can be installed with `cargo install just`

## Running

You should create a new repo with `slang-ui` as a dependency, or use one of our templates.

But if you just want to play around a bit, you can check out our examples:

```sh
# Run the basic-lint example
cargo run --bin basic-lint

# Run the complex example
cargo run --bin complex

# Or run it with cargo-watch which will recompile on file changes
cargo watch -cx "--bin complex"
```
