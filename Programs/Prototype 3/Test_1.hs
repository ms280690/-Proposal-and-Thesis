
-- Test_1.hs

-- Mehul Solanki.

-- Prolog for Hugs 98.


import Prolog

--import Main


add_clause :: Database -> Database
add_clause x = addClause x ((:-) (Struct "hello" []) [])

