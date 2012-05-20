Parallel VM Design
==================

Mini-BSP converts expression graphs into basic blocks for a simple
virtual machine that's meant to express predicated parallel-map
operations efficiently.  Each operation is easily implemented with
common short-vector SIMD (SSE, AVX, Altivec, etc) opcodes, but is also
structured to be executed in pipelines on L1D-sized subvector.  In
addition to improving locality and parallelism, the chunking also
makes it possible to insert conditional branches around predicated
operations without sensibly affecting performance negatively.

Basic semantics
---------------

Each basic block is defined by a common size, a set of variables
(homogeneous vectors), and a sequence of element-wise operations.  For
example,

    variables (size = 1000000):
     double a
     double b
     double d
     double r
    
    operations:
     d = a-b
     r = d*d

would have `r[i] = expt(a[i]-b[i], 2)` for each `0 <= i < 1000000`.

### Predicated operations

Each (nearly each) operation is predicated on a vector of masks (in
which each element is either 0 or -1).  The ith value of the result
vector is only defined if the ith mask is -1.  In order to simplify
common cases, these operations also receive an argument denoting
whether the meaning of the predicate mask should be reversed.

Chunked execution
-----------------

Each operation is a straight element-wise computation on the vectors,
making it trivial to fuse operations into pipelined loops.

The squared difference example in the previous section could be
naïvely executed by looping over `i < 1000000` to compute `d[i]` and
then `r[i]`.  With a native-code compiler, that would work fairly
well, particularly because intermediate values will be in first-level
cache, if not in registers.  However, this approach is difficult to
implement efficiently in an interpreter, and not always trivial to
optimise into SIMD code.

It would be better suited to an interpreter (and to SIMD code) if,
instead, each operation looped over the whole vectors.  In an
interpreter, each inner loop could be hand-written and compiled ahead
of time.  The disadvantage is that the operations fail to exploit
caches.  While accesses are in streaming order (i.e. memory latency
should not be an issue), newly-written data is likely to leave cached
memory before being reused.

We try to get the best of both worlds by executing the pipeline of
loops as a loop over reasonably-sized subsequences, or chunks,
(e.g. strides of 256 or 1024 elements), and then executing each
operation, in sequence, as inner loops over the subsequences.  The
chunks ought to be long enough (except for the very last one) to
render negligible the time spent on dynamic dispatch.  They're also
short enough to fit in level 2 caches, if not level 1.  Better, the
chunks' length and the multiple operations in the intermediate loop's
body means that parallel scheduling of each iteration is easily
amortised over large granularity work units.

Bad ascii art follows

    Loop per operation      Operation per loop
    
       aaaaaaaaaa             a
       bbbbbbbbbb             b
           |                  |
          a+b                a+b
           |                  |
           v                  v
       dddddddddd             d
           |                  |
          d*d                d*d
           |                  |
           v                  v
       rrrrrrrrrr             r
                                 a
                                 b
                                 |
                                a+b
                                 |
                                 v
                                 d
                                 |
                                d*d
                                 |
                                 v
                                 r
                                       ...

                 Chunked
    
               aaaaa
               bbbbb
                 |
                a+b
                 |
                 v
               ddddd
                 |
                d*d
                 |
                 v
               rrrrr
                     aaaaa
                     bbbbb
                       |
                      a+b
                       |
                       v
                     ddddd
                       |
                      d*d
                       |
                       v
                     rrrrr
          
### Local vectors

Some vectors are only used as intermediate values: they are neither
arguments nor results, like `d` in the example above.  Chunk-wise loop
body can reuse the storage for these vectors.  They only need to
allocate enough space for a single chunk's worth of data and adjust
the subsequence passed to each inner loop to always points to the
beginning of local vectors.

### Vector properties

It is useful to know when a vector consists of the same constant
replicated over all locations, or will not be used for masking
operations, or even not used at all.  A bitset of properties is
associated with each vector variable.

### Predicated operations

The chunking also introduces a reasonable granularity over which to
introduce conditional branches.  An operation that's predicated over a
mask chunk that is known to be all -1 or all 0 can be executed more
efficiently (or skipped); this is easily determined by branching on
the vector's constantness property.

Non-local (global) vectors may be used as masks.  In that case, a
result-less operation (`summary`) may be used to update its property
value with respect to the current chunk.

### Reduce operations

Vectors that are only passed to reduce (fold) operations can be
consumed incrementally chunk by chunk, avoiding the need to store full
vectors.  Local vector variables are initialised with a neutral value
and used as both destination and source vectors for reduce operations.

Once all chunks have been processed by worker threads, a sequence of
reducer operations describe how to reduce such vectors pairwise, and
then how to convert the final remaining vectors into scalar values.

Concrete implementation
-----------------------

The basic block is represented with a vector of variable structures,
a vector of operations, and a vector of reduction steps.

Each variable structure denotes whether it is a local or a global
vector, its element type and initial element, and its position in the
vector of variables, and the information necessary to initialise the
variable's property: constantness, liveness, whether it is used as a
mask.  The global flag is also a function: called with no argument, it
returns a pre-allocated vector, or `nil` if none; called with one
argument, it is used to update the vector corresponding to the
variable.

Each operation is a function to call, and a vector of arguments.
References to variables should simply be replaced with each variable's
location.

Before execution, a vector of homogeneous vectors is allocated, and
filled according to the variable structures.  Global vectors are
allocated for the whole basic block, while local ones are allocated
per worker.  A parallel vector of property values (`unsigned`) is also
allocated, per-worker.

For each chunk in the iteration range, the property vector is reset to
its initial value, and the operations are executed in sequence.  Each
function is called with a vector of addresses (the beginning of the
vector's subsequence), the property vector, the beginning of the
chunk, the size of the chunk, and the values in the argument vector.
The typical argument list for a vector-processing function is thus:
`(vectors properties start count mask-index flip-p dst-index ...)`.

Finally, once all operations have been executed, the `reduce-step` are
executed.  Each reduce-step is similar to an op, but contains two
functions: a 2arg-reducer and a 1arg-reducer.  The 2arg function
receives two vector and property vectors, and the rest of the
arguments; the destination vector is the first one.  For example,
`2arg-sum` could have this argument list: `(vectors1 properties1
vectors2 properties2 start count mask-index flip-p dst-index
src-index)`.  It would then take the partial sum in vectors2's
dst-index'th vector, and increment the dst vector in vector1 with it.
Once that is done, `1arg-sum` is called to compute a single scalar
value from the temporary vectors.  In this case, it is called with
only one pair of vectors and properties.
