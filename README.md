# optra

**Op**erational **Tra**nsformation library for collaborative editing, built with Haskell!

### To-Do's
This project is currently work-in-progress, and have many rooms for improvement. In particular, here's a list of To-Do's ordered by priority:
- Set up benchmarks and tests for performance and correctness
- Explore different serialization schemes and find the one that works best (Currently, Oprta serializes its operations to JSON via `aeason`).
- Documentation!


### Acknowledgements
This code follows and was tested against the Rust implementation of Operational Transformation in [spebern/operational-transform-rs](https://github.com/spebern/operational-transform-rs), which is ported from the Javascript implementation [Operational-Transform/ot.js](https://github.com/Operational-Transformation/ot.js/). 
