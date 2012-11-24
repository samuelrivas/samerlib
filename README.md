# Samerlib: Some Addictive Modules, Erlang Library

The acronym is not fixed, if a better one comes up, we will update it.

# What is this?

A collection of common functionality that can be useful for a lot of different
projects. It is the kind of application that wants to be named "stuff" or
"misc," or even "misc stuff."

The (vague) criteria that dictates whether a module is addictive could be
"something that is useful in several different contexts, but has not enough
entity to form an application."

The (as vague as before) way of telling if a module is not addictive could be
"something that implements any kind of business logic or will never be used in
more than one specific context."

Yet another imprecise way of thinking about Samerlib is that it compares to the
purpose of the [glib](http://developer.gnome.org/glib/), but for Erlang.

# Branching

 * [![Build
   Status](https://secure.travis-ci.org/samuelrivas/samerlib.png?branch=master)](http://travis-ci.org/samuelrivas/samerlib)
   the `master` branch points to the latest released tag.
 * [![Build
   Status](https://secure.travis-ci.org/samuelrivas/samerlib.png?branch=develop)](http://travis-ci.org/samuelrivas/samerlib)
   the `develop` branch points to the latest merged change. This branch always
   contains `master`.

Both `master` and `develop` are safe to branch off and rebase on as they are
guaranteed not to change their history or be ever deleted. Any other branch is
considered temporary and may be rebased or deleted without notice.

Also, `master` and `develop` are not allowed to contain merge commits. Any merge
from another branch is done rebasing it first to the HEAD of `develop`.

# Compiling

Just typing `make` should do the trick. You need Internet access to download the
dependencies.

# Testing

`make test` will run all the tests and produce a coverage report.

`make check` will run static checks (xref currently).
