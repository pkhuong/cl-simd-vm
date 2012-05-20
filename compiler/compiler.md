Compiler Design
===============

The compiler takes as input a set of return values, a sequence of
element-wise operations, and a sequence of reduce operations.  The
output is a basic block suitable for the parallel VM.

Inputs
------

### Return values

These are vector values that must be fully computed; they must thus be
fully allocated.  They also determine whether values are live or not.

### Element-wise operations

This is the main input to the compiler and determines live values. a
sequence of operations, where an operation is defined by:

 - function to call
 - argument vector (some of which are vector values)
 - vector of variables used as masks
 - vector of destination variables

### Reduce operations

Reduce operations also determine live values.  Each reduce operation
is defined by:

 - vector reducing function
 - vector aggregating function
 - final reduce function
 - argument vector
 - vector of variables used as masks
 - vector of destination variables
 - vector of accumulation variables, and how to initialise them

Output
------

A basic block for the VM

 - sequence of variables, denoting:
   * whether they are local or global one
   * how to initialise them
   * what attributes to initialise them with:
     - all same value
     - used as mask
     - unused
 - a sequence of ops
 - a sequence of reduce ops

In arguments, variables are replaced with their register number
(location).  Thus, the functions only have to be passed the summary
vector and the register vector; registers point to the chunk start for
both local and global vectors.  

Passes
------

### Determine which op defines each vector variable

Simple linear walk over the op sequence, the relationship is stored in
a hash table.

### Determine which vector values are global

Walk over the return values.

### Determine which vector values and ops are live

Recursive traversal from the output and reduce vectors.  

### Emit code for ops

Walk over the live ops in order. Registers and constant values are
allocated on the fly.

### Emit code for reduce ops

Same.  

