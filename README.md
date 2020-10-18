# toydb

A toy database in Haskell.

A [Recurse Center](https://www.recurse.com/) FP study group project.


## Specifications 

* REPL for database interaction
* In-memory representation as B-Tree
* Serialization / Deserialization to and from disk
* Table interactions: simple `CREATE` and `DROP` only 
* Record interactions: simple `SELECT`, `INSERT` and `DELETE` only
* Record data types: support for `Int`, `Text`, and `Bool` types only 
* Single-index tables, where index column of type `Int must be specified explicitly on table creation 


## References

* [Creating a REPL in Haskell](https://blogg.bekk.no/creating-a-repl-in-haskell-efcdef1deec2)
* [B-Trees and B+ Trees](https://www.youtube.com/watch?v=aZjYr87r1b8)
* [Let's Build a Simple Database](https://cstack.github.io/db_tutorial/)
