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
  --use-caching Enable caching optimization.
  --no-select-push-down Disable push down of selections
  --no-projection-opti Disable optimisations of projections
  --no-joins Disable joins creations
  --big-data Use algorithms suited for large datasets
  -help  Display this list of options
  --help  Display this list of options
```


# Features.

## What it is capable now:

- parsing of a SQL query
- compilation to linear algebra
- physical interpretation using feeds
- `in` and `not in` subqueries
- queries of the form `Select ... FROM (SELECT ... FROM ... WHERE ...), ... WHERE ...`
- arithmetic expressions inside a `where` clause


## Known issues:

- common subqueries caching is in highly experimental state and you can assume it's (nearly) not working. Thus, it is deactivated by default.

## What can be done:

- Further debugging for subqueries caching
- A simple cost model. Some work have been done on it theorically, but the time was missing to implement it
- A join sort algorithm scaling to big tables. For now the joinSort algorithm keeps a lot of thing in memory. It could be easily modify in order to store intermediary results on the harddrive: in the same way done in the external sort, we can split the sorted results into "chunks" that can fit into memory. Then, we can identify in which chunk is the first occurence of the sibling's row (using a dichotomy on the first row of each chunk). Finally we iterate from this first row to the next row until we have a row that can't be joined.
- implement `JoinProjectRename` and `ReadSelectProjectRename`. It can be easily done inside the method `feed_from_query` by using pattern matching. We didn't think this optimisation was usefull as our algorithms are already optimised for memory thanks to a feed approach. Time was lacking to add placeholders for these two "meta" relations.
- a distinct operator. We didn't implemented it, but it is easy to implemenet it either by using a groupby or a sort.

# Choices of implementations

## Feed approach

Since the start of the project we've decided to use feeds (or iterators) in order to execute the physical plan. They allow us to have efficient algorithms both in speed and in memory.

One bottleneck to this approach was sorting. Sorting is a cornerstones of our algorithms, but it is impossible to sort a feed without storing it. We designed an external sort which allow us to sort big files with a low memory footprints. We also took care of optimizing it until we couldn't think of ways to optimize it anymore. As a result, we achieve to sort a 400mo CSV files in ~70/75s using less than 100mo of RAM.

We also designed to versions of union, minus and join. One if the file could fit in memory, which uses hashmaps. The other one if the file can't be stored into RAM. In this case we're using external sorting extensively.

## How compilations happens.

Our compilation process go through several passes:

- We transform the SQL AST in order to rename tables to uids, to make sure the attributes we try to access exists and to transform the query in a disjunctive form
- Now that we have transformed the SQL Ast in a "normalized form", we compile it. During compilations we can insert joins in a way that try to maximize the number of joins.
- Then, we apply several optimizations steps: one pushing select down, an other trying to optimize the placement of projections (removing and adding some if needed), and an other deducing other joins. We call by joins expressions of the form  `SELECT_{a.foo = b.bar}(PRODUCT(... containing a ; ... containing b))`, or in SQL request of the form `SELECT * from a, b WHERE a.foo = b.bar`
- finally we convert or relationnal algebra tree to a physical plan made of feeds.

## Cost model

In order to choose which implementations of the algorithms to choose (unionHash or unionSort for exemple), we thought of adding a cost model which will estimate the number of elements outputted by every subtrees. To do so, we would have computed for each input csv:
- the number of rows
- for each attribute the maximum number of same values

Using these informations we can overapproximate the number of elements outputted by a select, a projection, a product...
Then, if this estimation is greater than a offset we can pick an algorithm which is able to perform on huge datasets.

## Caching

Because of our preprocessing pass, subqueries caching is not trivial to do in our impementation. In fact, a subtree `a` and a subtree `b` represents the same 'expression' if the naming of the tables is "coherent", that is if we can rename the tables of `a` to obtain `b`.

Caching is not well tested and quite buggy on some case. For now, we mostly cache nodes of the form `Input("foo.csv")`, which is quite dumb. It is deactivated by default.
