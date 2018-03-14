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


### How to use

Minisql comes with a simple command line interface:

After compiling, to execute minisql you have to use the command `./main.native`.
It takes the following parameters

- `-repl` use the repl mode
- `-o <file>` to pass the file in which we want to output the result of the query
- `-graphviz <file>` generate a graphviz plot of the relationnal algebra tree. The graphviz file is saved to `<file>` It can be compiled afterward with `dot -Tpdf <file> -o <file.pdf>`

An other parameter, which is a file containing a query to be executed can be given. Example:

```
./main.native test.sql -o test.out -graphviz test.dot
```

This will:

- execute the query present in test.sql
- save the result to test.out
- generate a graphviz plot to test.dot

**Remarks:**

- When using repl mode, you can use `rlwrap ./main.native` instead of `./main.native` in order to use arrow keys for easier manipulation.
- When using repl mode, requests must end by semi-colon **;**. This semi-colon is facultative if the query comes from a file.


## Features:

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
