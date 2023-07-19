# Lisp Cozodb

## About Cozodb

This is a Lisp wrapper for embedding [Cozodb] (https://www.cozodb.org)

Cozodb is a really nice database with some interesting features:

- Graph traversals
- Vector search
- Whole graph algorithms
- Time travelling
- Deduplication
- Full-text search
- Hi performance (up to 100K QPS)
- Transactions

Cozodb is based upon Datalog and is really easy to do pretty advanced logic with some easy queries. Syntax is really nice, readable and more powerfull than SQL - as far as I can see :)

Documentation on usage is [here] (https://docs.cozodb.org/en/latest/)

## Lisp api

### Installation

(ql:quickload "cozodb")

First you need the binary file. You can get it two ways:

- Download Cozodb and follow instructions for [building](https://github.com/cozodb/cozo/tree/main/cozo-lib-c)
- Or you can use the Linux version which is in the library folder in this project. If you have a Mac/Windows feel free to compile and make a pull request.

Make sure the .so file is in your path. Easy way on Linux is to do a export LD_LIBRARY_PATH=/path_to_dir_with_so_file

### Usage

First move to the package

```
(in-package cozodb)
```

Then open a database with:

```
(open-db "/tmp/mydir")
```

This returns a integer which will be needed when doing a query. Query can be done two ways:

1) Raw query. Following line creates a table and load it with two rows

```
(run-query db-id"?[l1, l2] <- [['a', 1], ['b',2]] :create stored {l1, l2}")
```

2) Interpolated query. The same query, but with interpolation. The percentage sign is the interpolation char.

```
(query db-id "?[l1, l2] <- % :create stored {l1, l2}" '(("a" 1) ("b" 2))
```

To query the data in the newly created table we can du this:
```
(run-query db-id "?[a, b] := *stored[a, b]")
```

The results of the query is in json format.

To close database:
```
(close-db db-id)
```

There are two optional params when doing a query. They are extra-params and immutable. Have a look in the Cozodb for an exlpanation of these if you need them.

Running examples are in the test.lisp file

## Backup and restore

```
(backup db-id "filename)
(restore db-id "filename)
```

See in the test-file for example on how to backup/restore.

## Viewing queries in a tabular format

The json can be tricky to read if you get more than a couple of rows.

The function

```
(show-as-table (query...))
```

can be used to show results in a tabular, nice format. It will also show tame taken to run the query.

### License

Lisp part is BSD or Apache, your choice.

