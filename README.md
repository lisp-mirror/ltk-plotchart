# ltk-plotchart

A wrapper around tklib's
[plotchart](https://core.tcl-lang.org/tklib/doc/trunk/embedded/www/tklib/files/modules/plotchart/plotchart.html)
library to work with LTk.

For complete documentation see <https://peterlane.netlify.app/ltk-plotchart>

# Usage

Plotchart is part of Tklib, which is available from the
[tklib](https://www.tcl.tk/software/tklib/) webpage. 

On Linux, it can often be installed through the package manager, e.g. Ubuntu:

    > sudo apt install tklib

To install ltk-plotchart,
[download](https://notabug.org/peterlane/ltk-plotchart/releases) the latest
version, and make the downloaded folder available to the asdf system (by
copying or linking) - for example (depending on your local settings): 

* on Linux, in the folder "~/common-lisp/"
* on Windows, in the folder "~/AppData/Local/common-lisp/source"

To test the internal functions:

    * (asdf:test-system :ltk-plotchart)

or directly:

    * (require 'ltk-plotchart/tests)
    * (ltk-plotchart::run-tests)

To run an example, cd to the "ltk-plotchart/examples" folder:

    > sbcl --script plotdemos1.lisp

Examples tested on Linux using [sbcl](http://www.sbcl.org) version 2.1.3 and
[ltk](https://github.com/herth/ltk/releases) version 0.992 with 
[tklib](https://core.tcl-lang.org/tklib/event/545a7ee378a0cc271fcf58ebf83684fb4257094e)
version 0.7.

# MIT License

Copyright (c) 2021, Peter Lane <peterlane@gmx.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

