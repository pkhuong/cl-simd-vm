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

### Predicated operations

The chunking also introduces a reasonable granularity over which to
introduce conditional branches.  An operation that's predicated over a
mask chunk that is known to be all -1 or all 0 can be executed more
efficiently (or skipped).  In order to exploit that, a summary is
associated with each vector, to denote whether the current chunk is
"all -1", "all 0", or neither ("unknown").

The default value for a summary is "unknown".  Operations may,
however, set it to a more precise value.  Each operation is passed an
argument to determine whether the computed values will be used as
masks.  If so, each value is defined (to 0 if the predicate mask isn't
-1), and it is recommended to determine a useful summary value.

Non-local (global) vectors may be used as masks.  In that case, a
result-less operation (`summary`) may be used to update its summary
value with respect to the current chunk.

### Reduce operations

Vectors that are only passed to reduce (fold) operations can be
consumed incrementally chunk by chunk, avoiding the need to store full
vectors.  Local vector variables may be marked as reduceable vectors.
Such vectors are meant to be initialised with a neutral value, and
will be used as both destination and argument by reduce functions.
Once all chunks have been processed by worker threads, their
reduceable vectors are further reduced, and the single remaining
vector reduced into a scalar.

Concrete implementation
-----------------------

The basic block is represented with a vector of variable structures,
and a vector of operations.

Each variable structure denotes whether it is a local or a global
vector, its element type and initial element, and its position in the
vector of variables.  Global vectors also point to a mutable cell
pointing to the whole vector (or filled with one as needed).

Each operation is a sequence in which the first element is the
function to call, and the rest arguments.  Arguments may be variable
structures, or arbitrary scalars values.

Before execution, a vector of homogeneous vectors is allocated, and
filled according to the variable structures.  Global vectors are
allocated for the whole basic block, while local ones are allocated
per worker.  A parallel vector of summary values (fixnums) is also
allocated, per-worker.

For each chunk in the iteration range, the operations are executed in
sequence.  The function is called with the vector of vectors, the
summary vector, the number of element to process in the chunk, and the
arguments in the remainder of the operation.  Each argument that's
actually a variable structure is replaced with two values: the
corresponding vector variable's index, and the index at which the
chunk begins (0 for local vectors).  The typical argument list for a
vector-processing function is thus: `(vectors summaries count
mask-index mask-start flip-p for-mask-p dst-index dst-start ...)`.

Finally, once all operations have been executed, all worker thread's
reduceable vectors are pair-wise reduced, and the remaining vector
reduced into a scalar.
