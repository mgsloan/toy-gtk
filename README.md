toy-gtk
=======

This is a GTK backend for the
[toy-interface](https://github.com/mgsloan/toy-interface) library.  It provides
a simple interface for creating applications which draw things and interact with
the mouse and keyboard.  It handles the minutiae of setting up the gtk window
and canvas, and processes the input events.

Why "toy"?
----------

The name \"toy\" comes from the \"toy framework\", a part of the
[lib2geom](http://lib2geom.sourceforge.net) library. It's used in building
"toys" demonstrating the features of the library.  This is a different variety
of "TDD" - but instead of tests, it's toys! We found that building little demos
to be a nice way to drive initial design / development.

Installation
------------

If you want to build the latest repository versions of the toy packages, use
[toy-sources](https://github.com/mgsloan/toy-sources), or manually fetch /
build the repositories.
