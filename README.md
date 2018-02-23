# Delay Differential Equations (DDE)

## Features

* Autonomous DDEs with multiple dynamical variables and a single delay (pull requests are welcome)
* Driven systems (i.e. with external input)
* Non-autonomous DDEs (using driven systems with time as external input)
* Second and fourth order integration methods
* Example models:
   * [Mackey-Glass](https://github.com/masterdezign/dde/blob/master/examples/MackeyGlass/Main.hs) with no external input
   * [Driven system](https://github.com/masterdezign/dde/blob/d7f636372b537b948d00097ecd09e689854b9392/dde/Numeric/DDE/Model.hs#L59)
* Pure Haskell

Note, only the first dynamical variable x(t) is recorded. This may be extended to multiple variables,
[see](https://github.com/masterdezign/dde/blob/9c5321cbd5788ebc96bd8920cb6dd460d3035521/dde/Numeric/DDE.hs#L164).
