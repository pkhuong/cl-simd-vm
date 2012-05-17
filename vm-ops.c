#include <assert.h>
#include <string.h>
#include <strings.h>
#include <math.h>

typedef unsigned long v2ul __attribute__ ((ext_vector_type (2)));
typedef unsigned v4u __attribute__ ((ext_vector_type (4)));
typedef double   v2d __attribute__ ((ext_vector_type (2)));

#define WITH_SUMMARY                                    \
        v4u summary_por  = {0, 0, 0, 0};                \
        v4u summary_pand = {-1U, -1U, -1U, -1U};        \
        do {} while (0)

#define PSUMMARISE(X)                           \
        do {                                    \
                summary_por  |= (X);            \
                summary_pand &= (X);            \
        } while (0)

inline unsigned scalarify_por (v4u x)
{
        v4u y = __builtin_shufflevector(x, x, 2, 3, 2, 3)
                | x;
        return y.x | y.y;
}

inline unsigned scalarify_pand (v4u x)
{
        v4u y = __builtin_shufflevector(x, x, 2, 3, 2, 3)
                & x;
        return y.x & y.y;
}

#define SCALAR_SUMMARY                                          \
        unsigned summary_or  = scalarify_por(summary_por);      \
        unsigned summary_and = scalarify_pand(summary_pand);    \
        do {} while (0)

#define SUMMARISE(X)                            \
        do {                                    \
                summary_or  |= (X);             \
                summary_and &= (X);             \
        } while (0)

#define RETURN_SUMMARY do {                             \
                if (summary_or == 0) return -1;         \
                if (summary_and == -1U) return 1;       \
                return 0;                               \
        } while (0)
        

int summarise (unsigned size, unsigned * restrict data)
{
        WITH_SUMMARY;

        {
                unsigned packed_size = size/4;
                v4u * restrict packed = (v4u*)data;
                for (unsigned i = 0; i < packed_size; i++) {
                        v4u x = packed[i];
                        PSUMMARISE(x);
                }
        }

        SCALAR_SUMMARY;

        for (unsigned i = size&(-4UL); i < size; i++) {
                unsigned x = data[i];
                SUMMARISE(x);
        }

        RETURN_SUMMARY;
}

int canonicalise_mask (unsigned size, unsigned * restrict mask, int mask_summary,
                       int flipped, int for_mask,
                       unsigned * restrict dst, unsigned * restrict src)
{
        (void)mask_summary;
        WITH_SUMMARY;

        {
                unsigned packed_size = size/4;
                v4u * restrict pmask = (v4u*)mask;
                v4u * restrict pdst  = (v4u*)dst;
                v4u * restrict psrc  = (v4u*)src;
                v4u pflip = {0, 0, 0, 0};
                if (flipped) pflip = ~pflip;
                for (unsigned i = 0; i < packed_size; i++) {
                        v4u value = psrc[i] & (pmask[i] ^ pflip);
                        PSUMMARISE(value);
                        pdst[i] = value;
                }
        }
        SCALAR_SUMMARY;

        unsigned flip = flipped?-1U:0;
        for (unsigned i = size&(-4U); i < size; i++) {
                unsigned x = mask[i] & (src[i] ^ flip);
                SUMMARISE(x);
                dst[i] = x;
        }

        RETURN_SUMMARY;
}

#include <stdio.h>

int merge64 (unsigned size, unsigned * restrict mask, int mask_summary,
             int flipped, int for_mask,
             double * restrict dst,
             unsigned * restrict select,
             double * restrict x, double * restrict y)
{
        {
                unsigned packed_size = size/4;
                v4u * restrict pmask = (v4u*)mask;
                v4u * restrict psel  = (v4u*)select;
                v4u * restrict pdst  = (v4u*)dst;
                v4u * restrict px    = (v4u*)x;
                v4u * restrict py    = (v4u*)y;
                v4u pflip = {0,0,0,0};
                if (flipped) pflip = ~pflip;
                for (unsigned i = 0; i < packed_size; i++) {
                        v4u mask = pmask[i]^pflip;
                        v4u mask_lo = __builtin_shufflevector(mask, mask, 0, 0, 1, 1);
                        v4u mask_hi = __builtin_shufflevector(mask, mask, 2, 2, 3, 3);
                        v4u sel  = psel[i];
                        v4u sel_lo  = __builtin_shufflevector(sel, sel, 0, 0, 1, 1);
                        v4u sel_hi  = __builtin_shufflevector(sel, sel, 2, 2, 3, 3);
                        {
                                v4u x = (v4u)(px[i*2]), y = (v4u)(py[i*2]);
                                x &= sel_lo;
                                y &= ~sel_lo;
                                pdst[i*2] = (x|y)&mask_lo;
                        }
                        {
                                v4u x = (v4u)(px[i*2+1]), y = (v4u)(py[i*2+1]);
                                x &= sel_hi;
                                y &= ~sel_hi;
                                pdst[i*2+1] = (x|y)&mask_hi;
                        }
                }
        }
        for (unsigned i = size&(-4U); i < size; i++) {
                double r = 0;
                if (mask[i] ? !flipped:flipped)
                        r = select[i]?x[i]:y[i];
                dst[i] = r;
        }

        return 0;
}

int merge32 (unsigned size, unsigned * restrict mask, int mask_summary,
             int flipped, int for_mask,
             unsigned * restrict dst,
             unsigned * restrict select,
             unsigned * restrict x, unsigned * restrict y)
{
        WITH_SUMMARY;

        {
                unsigned packed_size = size/4;
                v4u * restrict pmask = (v4u*)mask;
                v4u * restrict psel  = (v4u*)mask;
                v4u * restrict pdst  = (v4u*)dst;
                v4u * restrict px    = (v4u*)x;
                v4u * restrict py    = (v4u*)y;
                v4u pflip = {0,0,0,0};
                if (flipped) pflip = ~pflip;
                for (unsigned i = 0; i < packed_size; i++) {
                        v4u mask = pmask[i]^pflip;
                        v4u sel  = psel[i];
                        v4u x    = px[i];
                        v4u y    = py[i];

                        x &= sel;
                        y &= ~sel;
                        v4u r = mask&(x|y);
                        PSUMMARISE(r);
                        pdst[i] = r;
                }
        }

        SCALAR_SUMMARY;
        for (unsigned i = size&(-4U); i < size; i++) {
                unsigned r = 0;
                if (mask[i] ? !flipped:flipped)
                        r = select[i]?x[i]:y[i];
                SUMMARISE(r);
                dst[i] = r;
        }

        RETURN_SUMMARY;
}

int complement_mask (unsigned size, unsigned * restrict mask, int mask_summary,
                     int flipped, int for_mask,
                     unsigned * restrict dst, unsigned * restrict src)
{
        if (flipped)
                mask_summary = -mask_summary;

        if (mask_summary == -1) {
                if (for_mask)
                        bzero(dst, size*sizeof(double));
                return -1;
        }

        WITH_SUMMARY;

        if (mask_summary == 1) {
                unsigned packed_size = size/4;
                v4u * restrict pdst = (v4u*)dst;
                v4u * restrict psrc = (v4u*)src;
                for (unsigned i = 0; i < packed_size; i++) {
                        v4u x = psrc[i];
                        v4u r = ~x;
                        PSUMMARISE(r);
                        pdst[i] = r;
                }
        } else {
                unsigned packed_size = size/4;
                v4u * restrict pmask = (v4u*)mask;
                v4u * restrict pdst = (v4u*)dst;
                v4u * restrict psrc = (v4u*)src;
                v4u flip = {0,0,0,0};
                if (flipped)
                        flip = ~flip;

                for (unsigned i = 0; i < packed_size; i++) {
                        v4u mask = pmask[i]^flip;
                        v4u src = psrc[i];
                        v4u r = src^mask;
                        PSUMMARISE(r);
                        pdst[i] = r;
                }
        }

        SCALAR_SUMMARY;

        unsigned flip = flipped? -1U : 0;
        for (unsigned i = size&(-4UL); i < size; i++) {
                if (!(mask[i]^flip)) {
                        dst[i] = 0;
                        continue;
                }
                unsigned x = src[i];
                unsigned r = (~x)?-1U:0;
                SUMMARISE(r);
                dst[i] = r;
        }

        RETURN_SUMMARY;
}

#define NAME neg_double
#define OP(X) (-(X))
#include "unary-double-op.inc"
#undef OP
#undef NAME

#define NAME inv_double
#define OP(X) (1.0/(X))
#include "unary-double-op.inc"
#undef OP
#undef NAME

#define NAME add_double
#define OP(X, Y) ((X)+(Y))
#include "binary-double-op.inc"
#undef OP
#undef NAME

#define NAME sub_double
#define OP(X, Y) ((X)-(Y))
#include "binary-double-op.inc"
#undef OP
#undef NAME

#define NAME mul_double
#define OP(X, Y) ((X)*(Y))
#include "binary-double-op.inc"
#undef OP
#undef NAME

#define NAME div_double
#define OP(X, Y) ((X)/(Y))
#include "binary-double-op.inc"
#undef OP
#undef NAME

#define NAME eq_double
#define OP(X, Y) ((X) == (Y))
#include "binary-double-predicate.inc"
#undef OP
#undef NAME

#define NAME neq_double
#define OP(X, Y) ((X) != (Y))
#include "binary-double-predicate.inc"
#undef OP
#undef NAME

#define NAME lt_double
#define OP(X, Y) ((X) < (Y))
#include "binary-double-predicate.inc"
#undef OP
#undef NAME

#define NAME lte_double
#define OP(X, Y) ((X) <= (Y))
#include "binary-double-predicate.inc"
#undef OP
#undef NAME

#define NAME gt_double
#define OP(X, Y) ((X) > (Y))
#include "binary-double-predicate.inc"
#undef OP
#undef NAME

#define NAME gte_double
#define OP(X, Y) ((X) >= (Y))
#include "binary-double-predicate.inc"
#undef OP
#undef NAME

#define NAME neg_unsigned
#define OP(X) (-(X))
#include "unary-unsigned-op.inc"
#undef OP
#undef NAME

#define NAME complement_unsigned
#define OP(X) (~(X))
#define PREDICATE
#include "unary-unsigned-op.inc"
#undef PREDICATE
#undef OP
#undef NAME

#define NAME add_unsigned
#define OP(X, Y) ((X)+(Y))
#include "binary-unsigned-op.inc"
#undef OP
#undef NAME

#define NAME sub_unsigned
#define OP(X, Y) ((X)-(Y))
#include "binary-unsigned-op.inc"
#undef OP
#undef NAME

#define NAME mul_unsigned
#define OP(X, Y) ((X)*(Y))
#include "binary-unsigned-op.inc"
#undef OP
#undef NAME

#define NAME div_unsigned
#define OP(X, Y) ((X)/(Y))
#include "binary-unsigned-op.inc"
#undef OP
#undef NAME

#define NAME mod_unsigned
#define OP(X, Y) ((X)%(Y))
#include "binary-unsigned-op.inc"
#undef OP
#undef NAME

#define NAME eq_unsigned
#define OP(X, Y) ((X)==(Y))
#define PREDICATE
#include "binary-unsigned-op.inc"
#undef  PREDICATE
#undef OP
#undef NAME

#define NAME neq_unsigned
#define OP(X, Y) ((X)!=(Y))
#define PREDICATE
#include "binary-unsigned-op.inc"
#undef  PREDICATE
#undef OP
#undef NAME

#define NAME lt_unsigned
#define OP(X, Y) ((X)<(Y))
#define PREDICATE
#include "binary-unsigned-op.inc"
#undef  PREDICATE
#undef OP
#undef NAME

#define NAME lte_unsigned
#define OP(X, Y) ((X)<=(Y))
#define PREDICATE
#include "binary-unsigned-op.inc"
#undef  PREDICATE
#undef OP
#undef NAME

#define NAME gt_unsigned
#define OP(X, Y) ((X)>(Y))
#define PREDICATE
#include "binary-unsigned-op.inc"
#undef  PREDICATE
#undef OP
#undef NAME

#define NAME gte_unsigned
#define OP(X, Y) ((X)>=(Y))
#define PREDICATE
#include "binary-unsigned-op.inc"
#undef  PREDICATE
#undef OP
#undef NAME

#define NAME and_unsigned
#define OP(X, Y) ((X)&(Y))
#define PREDICATE
#include "binary-unsigned-op.inc"
#undef  PREDICATE
#undef OP
#undef NAME

#define NAME or_unsigned
#define OP(X, Y) ((X)|(Y))
#define PREDICATE
#include "binary-unsigned-op.inc"
#undef  PREDICATE
#undef OP
#undef NAME

#define NAME xor_unsigned
#define OP(X, Y) ((X)^(Y))
#define PREDICATE
#include "binary-unsigned-op.inc"
#undef  PREDICATE
#undef OP
#undef NAME

#define NAME reduce_add_double
#define OP(X, Y) ((X)+(Y))
#define NEUTRAL 0.0
#include "unary-double-reduce.inc"
#undef NEUTRAL
#undef OP
#undef NAME

#define NAME reduce_mul_double
#define OP(X, Y) ((X)*(Y))
#define NEUTRAL 1.0
#include "unary-double-reduce.inc"
#undef NEUTRAL
#undef OP
#undef NAME

#define NAME reduce_max_double
#define OP(X, Y) ({ v2ul mask = (v2ul)((X)>(Y));                \
                        (v2d)(((v2ul)(X)&mask)|((v2ul)(Y)&(~mask)));})
#define SCALAR_OP(X, Y) ((X)>(Y)? (X) : (Y))
#define NEUTRAL -HUGE_VAL
#include "unary-double-reduce.inc"
#undef NEUTRAL
#undef SCALAR_OP
#undef OP
#undef NAME

#define NAME reduce_min_double
#define OP(X, Y) ({ v2ul mask = (v2ul)((X)<(Y));                \
                        (v2d)(((v2ul)(X)&mask)|((v2ul)(Y)&(~mask)));})
#define SCALAR_OP(X, Y) ((X)<(Y)? (X) : (Y))
#define NEUTRAL -HUGE_VAL
#include "unary-double-reduce.inc"
#undef NEUTRAL
#undef SCALAR_OP
#undef OP
#undef NAME

#define NAME reduce_add_unsigned
#define OP(X, Y) ((X)+(Y))
#define NEUTRAL 0
#include "unary-unsigned-reduce.inc"
#undef NEUTRAL
#undef OP
#undef NAME

#define NAME reduce_mul_unsigned
#define OP(X, Y) ((X)*(Y))
#define NEUTRAL 1
#include "unary-unsigned-reduce.inc"
#undef NEUTRAL
#undef OP
#undef NAME

#define NAME reduce_or_unsigned
#define OP(X, Y) ((X)|(Y))
#define NEUTRAL 0
#include "unary-unsigned-reduce.inc"
#undef NEUTRAL
#undef OP
#undef NAME

#define NAME reduce_and_unsigned
#define OP(X, Y) ((X)&(Y))
#define NEUTRAL -1U
#include "unary-unsigned-reduce.inc"
#undef NEUTRAL
#undef OP
#undef NAME

#define NAME reduce_max_unsigned
#define OP(X, Y) ({ v4u mask = (v4u)((X) > (Y));        \
                        ((X)&mask)|((Y)&(~mask));})
#define SCALAR_OP(X, Y) ((X)>(Y)? (X) : (Y))
#define NEUTRAL -HUGE_VAL
#include "unary-unsigned-reduce.inc"
#undef NEUTRAL
#undef SCALAR_OP
#undef OP
#undef NAME

#define NAME reduce_min_unsigned
#define OP(X, Y) ({ v4u mask = (v4u)((X) < (Y));        \
                        ((X)&mask)|((Y)&(~mask));})
#define SCALAR_OP(X, Y) ((X)<(Y)? (X) : (Y))
#define NEUTRAL -HUGE_VAL
#include "unary-unsigned-reduce.inc"
#undef NEUTRAL
#undef SCALAR_OP
#undef OP
#undef NAME
