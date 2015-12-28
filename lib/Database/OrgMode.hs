-- | = Introduction
--
-- <<https://api.travis-ci.org/rzetterberg/orgmode-sql.svg Travis CI status>>
--
-- A library that facilitates import\/export orgmode data into\/out of SQL
-- databases, provide common queries and specialized summaries of the data.
--
-- Supports using MySQL, PostgreSQL or SQLite as storage backend.
--
-- == Importing, manipulating and exporting
--
-- Using this library you can grab one of your orgmode files and this library
-- will parse the file, create the database schema and import the data into the
-- database. You can then proceed to manipulate the data in the database and
-- when you are done you can use this library to export the data back to orgmode
-- plain text.
--
-- Since all data exists in a SQL database you could write custom SQL queries
-- that manipulate the data, or you could access the data in any other
-- programming language that has support for the chosen SQL database.
--
-- == Presenting data
--
-- This library also supplies different types of summaries that can be created
-- from parsed data.
--
-- For example if you want to show a summary of what time is
-- spent on, you can generate a 'ClockTable'.
--
-- Another example would be that you want to build a web site that can display
-- data as graphs using Javascript. Then you can use this library to produce
-- JSON output.
--
-- Maybe you want to create your own summaries and reports, then this library
-- can help you do that too.

module Database.OrgMode
       ( textImportDocument
       , textExportDocument
       ) where

import           Database.OrgMode.Import.Text (textImportDocument)
import           Database.OrgMode.Export.Text (textExportDocument)
