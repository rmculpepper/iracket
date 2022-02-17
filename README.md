# IRacket

IRacket is a Racket kernel for [Jupyter](http://jupyter.org/). IRacket enables
interactive notebook-style programming with Racket.


# Requirements

* [Racket](http://racket-lang.org)
* [Jupyter](https://jupyter.org/)
* [ZeroMQ](http://zeromq.org)
  - on Debian/Ubuntu Linux: install the `libzmq5` package
  - on RedHat/Fedora (Linux): install the `zeromq` package
  - on MacOS with Homebrew: 
    - run `brew install zmq`
    - run `zsh -c 'a=(/usr/local/Cellar/zeromq/*/lib/libzmq.5.dylib(om)) ; for d in /Applications/Racket*/lib/ ; do ; gcp -v ${a[1]} "$d" ; done'`
  - on Windows, automatically installed by Racket's `zeromq-r-lib` package
  - for other systems, see http://zeromq.org


# Installation

First install the iracket package:
```bash
raco pkg install iracket
```
Then register the iracket kernel with Jupyter:
```bash
raco iracket install
```


# Using Jupyter with Racket

Run the Jupyter notebook server as you usually do, e.g.
```bash
jupyter notebook
```
and create a new notebook with the Racket kernel, or open
`examples/getting-started.ipynb` in the iracket source directory.


# Examples

See the `examples` subdirectory for example notebooks.

# Acknowledgments

The first version of IRacket was by Theo Giannakopoulos (then at BAE
Systems), for the PPAML program.
