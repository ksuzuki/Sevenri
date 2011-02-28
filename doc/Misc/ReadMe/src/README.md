## About Sevenri

Sevenri is a Swing-based, interactive Clojure environment written in
Clojure.

In Sevenri you can run application called *slix*. Sevenri comes with bunch
of slixes, such as *REPL*, Clojure editor *Ced*, and Incanter front-end
*Incantea*. With help of these slixes and powerful Clojure libraries you can
learn and study Clojure, try out your ideas and run your projects, and even
develop your own slixes, all in Clojure interactively.

## Current Status

This version of Sevenri is an early experimental, proof-of-concept version
and runs on Mac OS X 10.6 only. Lots of features that are supposed to be in
this kind of system are missing; menus, functions and slixes for file system
and project management, customization and refactoring tools, slix UI design
and development facilities, etc. Small part is still written in Java.

## Getting Started

prerequisites: `git` and `leiningen` are required.

>~$ git clone git://github.com/ksuzuki/Sevenri.git  
>~$ cd Sevenri  
>~$ lein deps  
>~$ sevenri  

Sevenri is designed to start without building it. Please read the on-line users manual
(doc/Manuals/Sevenri\_Users\_Manual/out/Table\_of\_Conents.html) for more details.

## Contributing

Please report issus on the [GitHub issue tracker](https://github.com/ksuzuki/Sevenri/issues)
or the [mailing list](http://groups.google.com/group/sevenri).

## License

Copyright (C) 2011 Kei Suzuki  All rights reserved.

This document is part of Sevenri, a Clojure environment ("This Software").

The use and distribution terms for this software are covered by
the [Eclipse Public License version 1.0](http://opensource.org/licenses/eclipse-1.0.php)
which can be found in the COPYING at the root of this distribution.
By using this software in any fashion, you are agreeing to be bound by the
terms of this license. You must not remove this notice, or any other, from
this software.
