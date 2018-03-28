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


## Running tests.

Prequel is covered by 38 unit tests and 5 integration tests.

To run them, you have to install [OUnit2](http://ounit.forge.ocamlcore.org/api-ounit/OUnit2.html) using `opam install ounit2`, and then run `make test`.


# Usage.

To play around with the engine, just run `./prequel` to get a SQLite-like REPL.

Here are a few examples of queries:

- `SELECT * FROM "tests/sources/projets.csv" p;`
- `SELECT * FROM "tests/sources/projets.csv" p WHERE p.idp > p.responsable;`
- `SELECT * FROM "tests/sources/projets.csv" p ORDER BY p.idp DESC;`
- `SELECT * FROM "tests/sources/projets.csv" p GROUP BY p.responsable;`

You can also use the engine from the command line:
```
Prequel version 1.1.
Usage: ./prequel [path]
When path is not specified, runs in REPL mode.
  --output A file in which to write the output.
  --graph A file in which to save a graph of the term.
  --use-caching Enable caching optimization.
  --no-select-push-down Disable push down of selections.
  --no-projection-opti Disable optimisations of projections.
  --no-joins Disable the use of joins.
  --big-data Use algorithms suited for large datasets.
  -help  Display this list of options
  --help  Display this list of options
  ```


# Features.

Prequel supports the following constructs:
- Using the `UNION` and `MINUS` set operators.
- Projecting on some or all columns (using `SELECT * FROM ◽`).
- Computating of arbitraty arithmetic expressions (e.g. `SELECT 1 + 1 FROM ◽`).
- Renaming of columns using `AS` (e.g. `SELECT foo.bar AS baz FROM ◽`).
- Filtering rows using the `WHERE` clause (e.g. `SELECT * FROM ◽ WHERE foo.bar + foo.baz < 10`).
- Using subqueries:
	- In the `FROM` clause (e.g. `SELECT table.foo FROM (SELECT * FROM ◽) AS table`).
	- In the `WHERE` clause, using `WHERE IN` and `WHERE NOT IN`.
- Ordering rows by arbitrary expressions (e.g. `SELECT * FROM ◽ ORDER BY foo.bar ASC, foo.baz + foo.bam DESC`).
- Grouping rows by arbitrary expressions (e.g. `SELECT * FROM ◽ GROUP BY foo.baz + foo.bam`).
- Using the `MAX`, `MIN`, `AVG` and `COUNT` aggregate functions:
	- In the `SELECT` clause (e.g. `SELECT MAX(foo.bar) FROM ◽ GROUP BY foo.baz`).
	- In the `HAVING` clause (e.g. `SELECT * FROM ◽ GROUP BY foo.baz HAVING MAX(foo.bar) > 5`).

Query examples for all those features can be found in the `tests/` folder.


# How it works.

Under the hood: 

- Prequel parses the SQL query into an instance of `Ast.query`.

- It then applies several checking and optimization phases to that query (see `AstChecker` and `AstTransform`):
	- It gives each table a unique name to avoid attribute shadowing.
	- It normalizes the query, putting all the conditions in disjunctive form.

- It compiles that query into a relational term.

  During compilation, it tries to replace expresions of the form `SELECT * from a, b WHERE a.foo = b.bar` with joins.

- This term is then further optimized, by trying to:
	- push `Select`s down;
	- optimize the placement of projections (removing and adding some if needed);
	- substituting expressions of the form `Select_{a.foo = b.bar}(Product(α, β))`, where `α` contains `a` and `β` contains `b`, with a `Join`.

- Finally, it converts the term to a physical plan, and evaluates it lazily.


## About lazy evaluation.

Since the start of the project we've decided to use lazy evaluation (in the form of iterators, or `feeds` as we call them) when executing the physical plan. This allows us to have efficient algorithms both in speed and in memory.

One bottleneck to this approach was sorting. Sorting is a cornerstones of our algorithms, but it is impossible to sort a feed without storing it. We designed an external sort which allow us to sort big files with a low memory footprints. We also took care of optimizing it until we couldn't think of ways to optimize it anymore. As a result, we achieve to sort a 400mo CSV files in ~70/75s using less than 100mo of RAM.

We also designed two versions of union, minus and join. One if the entire file fits in memory, which uses Hashmaps. The other if the file can't be stored into RAM. In this case we're using external sorting extensively.


## About `GROUP BY` and aggregate functions.

The computation of aggregate functions was a little tricky.

Because the `Group` relational operator only outputs one row per group, operators that would come after it (e.g. a `Projection` when using `SELECT` with an expression containing an aggregate function, or a `Select` when using a `HAVING` clause) would lose necessary information about the inside of the group.

To work around this, Prequel first extracts all the calls to aggregates functions in `AstChecker.extract_aggregates` in the `SELECT` and `HAVING` clauses, and replaces them with a reference to an attribute with a uniquely generated name. Then, when compiling the query, the `Group` operator receives a list of `(name, aggregate function call)` couples to compute for each group. This way, the `Group` operator is able to output extra columns for each function call, which can be reused by operators that come after it.


## About caching.

Because of our preprocessing pass, subqueries caching is not trivial to do in our impementation. In fact, a subtree `a` and a subtree `b` represents the same "expression" if the naming of the tables is "coherent", that is if we can rename the tables of `a` to obtain `b`.

So, at the moment, caching is not well tested and even buggy in some case -- and we mostly cache nodes of the form `Input("foo.csv")`, which is quite dumb. Because of this, it is disabled by default, and can be enabled using the `--use-caching` option.


# What could still be done.

- Further debugging for subqueries caching.
- A simple cost model.

  In order to choose which implementation of the algorithms to choose (UnionHash or UnionSort for example), we thought of adding a cost model which would estimate the number of elements produced by every subtree.

  To do so, we would have computed, for each input CSV file:
  - the number of rows;
  - for each attribute the maximum number of same values.

  Using this data, we can over-approximate the number of elements produced by a `Select`, a `Projection`, a `Product`, etc.
  Then, if this estimation is greater than a fixed offset, we pick an algorithm which is able to perform on huge datasets.

- A join sort algorithm scaling to big tables.

  For now the JoinSort algorithm keeps a lot of thing in memory. This could easily be modified in order to store intermediate results on the hard drive: in the same way as with the external sort, we can split the sorted results into "chunks" that can fit into memory.

  Then, we can identify in which chunk the first occurence of the sibling's row is (using a dichotomy on the first row of each chunk). Finally, we can iterate from this first row to the next row until we have a row that can't be joined.

- Implementing `JoinProjectRename` and `ReadSelectProjectRename`.

  It can be easily done inside the method `feed_from_query` by using pattern matching. We didn't think this optimisation was *really* useful as our algorithms are already optimised for memory thanks to a feed approach. Time was lacking to add placeholders for these two "meta" relations.

- A `DISTINCT` operator. We didn't have time to implement it, but it is really easy to do using either a `GroupBy` or a `Sort`.
