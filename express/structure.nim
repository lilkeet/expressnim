##[This module encapsulates the abstract syntax tree of EXPRESS.]##


### Schemas

##[All EXPRESS files consist of a collection of "schema"'s. A schema contains
the data structures, functions, and procedures that make up the described
model.
These schemas may be referenced by other schema's (possibly in other files).
For example, a schema describing Nim's system module may reference another
schema that describes C's stdlib in order to define a `cstring`.

Schemas may be nested within another schema. In fact, all schemas are a child
of an imaginary "universal" schema. The built-in data structures, functions,
etc all exist within it.]##



{.experimental: "strictFuncs".}
