CL-SIMD-VM
==========

The goal of this project is four-folds:

 1. provide easy access to SIMD and thread-level parallelism in vector
 map and reduce operations;
 2. sprinkle bulk operations to handle common operations that are not
 readily expressible as map/reduce (e.g. scan or sort);
 3. exploit the latter to ensure predicated versions of the former are
 reasonably efficient;
 4. support large datasets via foreign (C) allocation, mmap-ed files,
 and a distributed master/slave architecture.

Currently, only 1. and, to a certain extent, 3., are mostly
implemented.

Using vectorised map and reduce
===============================

The first step is initialise a context, specifying the number of
elements the map and reduce operations will operate on:

    (v:with-context (count &optional chunk-size) &body body)

The default chunk-size is a reasonably small value, 1024, which seems
to strike a nice balance between small temporary vectors and
minimising dynamic dispatch overhead (more on both topics in the
Implementation section).  It should always be a multiple of 256, to
work safely on future platforms (e.g. AVX).

In the dynamic scope of `v:with-context`, we have access to map and
reduce operations like `v:*` and `v:/+`.  For example, we can define
the 2-norm as

    (defun 2-norm (x)
      (sqrt (v:/+ (v:* x x)))) ; map *, reduce with +, cl:sqrt the result

and then use that regular CL function in a context.

    CL-USER> (defparameter *x* (map-into (make-array (* 1024 1024)
                                                     :element-type 'double-float)
                                         (lambda ()
                                           (1- (random 2d0)))))
    *X*
    CL-USER> (v:with-context ((length *x*))
               (2-norm *x*))
    591.1369354073146d0

In addition to arithmetic (on doubles and 32-bit unsigned integers),
map operations also include comparisons and selection (if).  A
vectorised map could be implemented as

    (defun vmax (x y)
      (v:if (v:> x y) x y))

and used normally again:

    CL-USER> (defparameter *y* (map-into (make-array (* 1024 1024)
                                                     :element-type 'double-float)
                                         (lambda ()
                                           (1- (random 2d0)))))
    *Y*
    CL-USER> (v:with-context (10)        ; only work on the first 10 elements
               (v:value (vmax *x* *y*))) ; convert the value to a CL array 
    #(0.8990741502941364d0 0.6821787183278096d0 0.7014682167254209d0
      0.8188648203640212d0 0.5084609828939568d0 0.2426496760813448d0
      0.9335859447014592d0 0.4421339096423811d0 -0.16706906282819567d0
      -0.6610080801379175d0)

Now, `max` is already a vectorised function, so we can also

    CL-USER> (v:with-context (10)
               (v:value (v:max *x* *y*)))
    #(0.8990741502941364d0 0.6821787183278096d0 0.7014682167254209d0
      0.8188648203640212d0 0.5084609828939568d0 0.2426496760813448d0
      0.9335859447014592d0 0.4421339096423811d0 -0.16706906282819567d0
      -0.6610080801379175d0)

Non-vector values are automatically converted into replicated
vectors.  For example, the variance could be computed with

    (defun variance (x)
      (let* ((avg        (/ (v:/+ x) v:n))
             (normalised (v:- x avg)))
        (/ (v:/+ (v:* normalised normalised))
           v:n)))
    
    CL-USER> (v:with-context ((length *x*))
               (time (variance *x*)))
    Evaluation took:
      0.007 seconds of real time
      0.007366 seconds of total run time (0.007283 user, 0.000083 system)
      100.00% CPU
      11,598,472 processor cycles
      32,768 bytes consed
      
    0.3332545175675462d0

Until now, we have only implemented computations with a single (scalar
or vector) result at each step.  This is not always the case.  A less
numerically stable way to compute the variance is:

    (defun quick-variance (x)
      (v:let ((sum-x   (v://+ x))
              (sum-x^2 (v://+ (v:* x x))))
        (- (/ (v:value sum-x^2) v:n)
           (expt (/ (v:value sum-x) v:n) 2))))
    
    CL-USER> (v:with-context ((length *x*))
               (time (quick-variance *x*)))
    Evaluation took:
      0.004 seconds of real time
      0.003947 seconds of total run time (0.003884 user, 0.000063 system)
      100.00% CPU
      6,190,480 processor cycles
      32,576 bytes consed
      
    0.3332545175675429d0

The double-slash versions of reduce operations return a placeholder on
which `v:value` must be called, like map operations.  `v:let` allows
the runtime to keep track of (potentially) live map and reduce
placeholders.  The first call to `v:value` triggers the computation of
the value for all live placeholders.  Batching operations enables
significantly more efficient execution strategies, particularly on
large datasets.

Operations
==========

Three element types are supported: double floats (`bsp:double`),
32-bit unsigneds (`bsp:u32`) and booleans (`bsp:bool`) represented as
32 bit masks.  All of these operations are dynamically typed (at the
front-end) on homogeneous vectors.  Thus, type-dispatch happens only
while the expression graph is created, not during vectorised
execution.

Map operations
--------------

In all cases, scalar values are upgraded to vectors: double floats to
`bsp:double`, integers to `bsp:u32` and booleans to `bsp:bool`.  These
operations always return placeholders on which `v:value` must be
called to obtain a Lisp vector.

 * `v:+`, `v:-`, `v:*` work on both doubles and unsigneds; unary `v:-`
   negates the value, unary `v:*` and `v:+` are identity.  Unary `v:/`
   only exists for doubles.
   
 * `v:%` a modulo for unsigned integers.

 * `v:max`, `v:min` work on all three types. For booleans, `true >
   false`.
   
 * `v:or`, `v:and`, `v:xor` and `v:~` implementing bitwise or, and,
   xor and complement on unsigneds, and the logical equivalent on
   booleans.
   
 * `v:=`, `v:/=`, `v:<`, `v:<=`, `v:>` and `v:>=` are predicates that
   work on all three types.  Again, for booleans `true > false`.

Reduce operations
-----------------

Reduce operations have a regular single slash version (e.g. `v:/+`)
that returns a value, and double slash (`v://+`) that return a
placeholder.

 * `v:/+`: sum of any of the three types.  Boolean `true` is
   interpreted as 1, `false` as 0.
   
 * `v:/*`: product of unsigneds or doubles.
 
 * `v:/min`, `v:/max`: max of any of the three types.  For booleans,
   `true > false`.
   
 * `v:/or`, `v:/and`, `v:/xor`: bitwise on unsigneds, or logical on
   booleans.

Miscellaneous operations
------------------------

* `v:value`: forces placeholders (from map or double-slash reduces)
  into CL values.

* `v:let`: the variables introduced via `v:let` are visible to the
  runtime; placeholders bound to such variables are considered as
  live.

* `v:n`: elements count in the current context.

* `v:barrier`: force the computation of all live placeholders.

* `v:with-context`: create a new map/reduce context with the specified
  element count.

Execution model
===============

Map and reduce operations have the property that they can easily be
pipelined.  For example, it's always possible to perform all the
operations for the 1024 first elements, then the next 1024, etc.  This
way, intermediate values are much more likely to remain in cache, and
temporary vectors are of small, constant size.

Each call to a map or reduce function adds an operation in the
context's operation log.  When execution is triggered, all the
operations necessary to compute a live (either via `v:let` or as the
argument to `v:value`) placeholder will be executed.  Placeholders and
operations are decoupled, allowing a single operation to have multiple
results.

Conditionals are executed as predicated (masked) operations followed
by an element-wise merge.  The front-end keeps track of the currently
active stack of masks and ensures that operations are only performed
on arguments with compatible masks.  There's an interesting benefit to
the partial pipelining: operations predicated on constant masks (or
runs thereof) can go through a fast path.

Once that is done, full CL vectors are allocated for live map values,
and temporary ones are `malloc`ed for constant values.  Then, each
worker thread (currently, only one) `malloc`s thread-local temporary
vectors, and the sequence of operations executed on each chunk
(subsequence).

Finally, the results for reduce operations are aggregated between
threads, and converted to CL values.  Once that is done, `malloc`ed
values are `free`d.
