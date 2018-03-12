# MiniSQL

## How to use and build

### Build

Before begin able to build minisql, several dependencies must be installed with `opam`:

- `ocamlbuild`
- `csv`
- `ounit2` (optional): for the unit tests, but unfortunately none are implemented right now

Then, to build minisql execute

```
make
```



## Features:

### What it is capable now:

- parsing of a SQL query
- compilation to linear algebra
- physical interpretation using feeds
- `in` and `not in` subqueries
- queries of the form `Select ... FROM (SELECT ... FROM ... WHERE ...), ... WHERE ...`
- arithmetic expressions inside a `where` clause

## What remains to be done:

- implemented an external sort algorithm (or a bloom filter) in order to update `minus` and `union` operator to operate without loading everything to memory
- group by
- optimization

## Known issues:

- sometimes, (as with `not in` expressions or queries with a `in` subquery inside an other `in` subquery), the returned result will contain duplicated entries
