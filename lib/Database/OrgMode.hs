{-|
= Introduction

<<https://api.travis-ci.org/rzetterberg/orgmode-sql.svg Travis CI status>>

A library that parses org-mode documents, imports the data into an SQL database,
provides the user with common queries on the data and functionality to export
the data to different data formats.

= Goals

Provide a solid foundation to build interesting applications/services that
revolves around using org-mode data.

This library should be usable with MySQL, PostgreSQL and SQLite.
However, currently testing is only performed using SQLite (in memory) as backend.

The priorities of this library are:

1. Stability
2. User friendly / Well documented
3. Features
4. Performance

= Under the hood

The foundation of this library revolves around using
<https://hackage.haskell.org/package/orgmode-parse orgmode-parse> and
<https://hackage.haskell.org/package/persistent persistent>. Those two libraries
takes care of the heavy lifting and solves the real problems. This library is
just the glue between them!

If you are familiar with orgmode-parse you know that it has data types for all
the different data in an org-mode document. This library has it's own data types
that is structured similarly but adapted for storage in SQL-databases. This
library aims to not expose the user to the internal data types and instead the
user should use the data types in orgmode-parse.

In other words, you put in orgmode-parse data types and get orgmode-parse
data types back.

= How to use the library

-}

module Database.OrgMode
       ( importDocument
       , textImportDocument
       , exportDocument
       , textExportDocument
       ) where

import           Database.OrgMode.Import (importDocument, textImportDocument)
import           Database.OrgMode.Export (exportDocument, textExportDocument)
