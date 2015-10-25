# orgmode-sql

A [Haskell](https://www.haskell.org/) library that takes [org-mode](http://orgmode.org/)
documents parsed with [orgmode-parse](https://hackage.haskell.org/package/orgmode-parse),
converts the data into a relational model and insert it into a SQL database
([SQLite](https://www.sqlite.org/), [PostgreSQL](http://www.postgresql.org/)
or [MySQL](https://www.mysql.com/))

The idea is to create a foundation that can be used to build interesting
applications that use org-mode data. SQL was chosen because it's ideal for
this kind of data and because it leaves the door open for use in other
languages and tools.

Initially only `SQLite` will be tested and supported, but the plan is to be able
to also use `PostgreSQL` and `MySQL`.

## Specification

Uses the [Stackage LTS 3.7 package set](https://www.stackage.org/lts-3.7) to
provide a stable dependecy tree.

Uses the [persistent](https://hackage.haskell.org/package/persistent) package
for database operations.

Basically this library is not very impressive, it only serves as glue between
other libraries that are impressive.
