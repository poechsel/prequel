![Prequel](http://amaia.at/prequel.png)

*A small SQL engine for CSV files.*


# About this project.

This project is an OCaml implementation of a SQL engine for CSV files.

It was written by [Pierre Oechsel](https://github.com/poechsel) and [Romain Liautaud](https://github.com/liautaud) for the [DBDM course](https://perso.liris.cnrs.fr/emmanuel.coquery/dbdm/DBDM-2018-project.html) of the ENS de Lyon.


# Installation guide.

This project depends on a few packages, which can be installed using [OPAM](https://opam.ocaml.org/doc/Install.html):
```
opam switch 4.05.0
opam install ocamlbuild
opam install ppx_deriving
opam install menhir
opam install fpath
opam install csv
```

Then, building the project is as simple as running `make`.

_For a better experience using the REPL, we recommand that you install [rlwrap](https://github.com/hanslub42/rlwrap)._


## Running unit tests.

To run unit tests, you have to install [OUnit2](http://ounit.forge.ocamlcore.org/api-ounit/OUnit2.html) using `opam install ounit2`, and then run `make test`.


# Usage.

To play around with the engine, just run `./prequel` to get a SQLite-like REPL.

Here are a few examples of queries:

- `SELECT * FROM "tests/sources/projets.csv" p;`
- `SELECT * FROM "tests/sources/projets.csv" p WHERE p.idp > p.responsable;`

You can also use the engine from the command line:
```
Prequel version 1.1.
Usage: ./prequel [path]
When path is not specified, runs in REPL mode.
  --output A file in which to write the output.
  --graph A file in which to save a graph of the term.
  -help  Display this list of options
  --help  Display this list of options
```


# Features.

### What it is capable now:

- parsing of a SQL query
- compilation to linear algebra
- physical interpretation using feeds
- `in` and `not in` subqueries
- queries of the form `Select ... FROM (SELECT ... FROM ... WHERE ...), ... WHERE ...`
- arithmetic expressions inside a `where` clause

### What remains to be done:

- implemented an external sort algorithm (or a bloom filter) in order to update `minus` and `union` operator to operate without loading everything to memory
- group by
- optimization

### Known issues:

- sometimes, (as with `not in` expressions or queries with a `in` subquery inside an other `in` subquery), the returned result will contain duplicated entries. This is probably due to the products
