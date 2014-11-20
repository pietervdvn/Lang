module Languate.TypeTable.BuildTypeTable where

{--
This module builds the type table for the given module.

First, a simple set is build of what types are defined in the module.
Then, using the local sets from each module, we check which ones are visible where.
Using this info, we build the actual type table.
--}
