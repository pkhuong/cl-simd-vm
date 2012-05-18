int NAME (unsigned size, unsigned * restrict mask, int mask_summary,
          int flipped, int for_mask,
          unsigned * restrict dst, unsigned * restrict x, unsigned * restrict y)
{
        if (flipped)
                mask_summary = -mask_summary;

        if (mask_summary == -1) {
                if (for_mask)
                        bzero(dst, size*sizeof(double));
                return -1;
        }

#ifdef PREDICATE
        WITH_SUMMARY;
#else
        assert(!for_mask);
#endif
        if ((mask_summary == 1) || !for_mask) {
                unsigned packed_size = size/4;
                v4u * restrict pdst = (v4u*)dst;
                v4u * restrict px = (v4u*)x;
                v4u * restrict py = (v4u*)y;
#ifdef PREDICATE
#define BODY(I) do {                                    \
                        unsigned i = (I);               \
                        v4u x = px[i], y = py[i];       \
                        v4u r = (v4u)OP(x, y);          \
                        PSUMMARISE(r);                  \
                        pdst[i] = r;                    \
                } while (0)
#else
#define BODY(I) do {                            \
                        unsigned i = (I);       \
                        v4u x = px[i], y = py[i];       \
                        v4u r = (v4u)OP(x, y);          \
                        pdst[i] = r;                    \
                } while (0)
#endif
                for (unsigned ii = 0; ii < packed_size; ii+=4)
                        for (unsigned j = 0; j < 4; j++)
                                BODY(ii+j);

                for (unsigned ii = packed_size&(-4u); ii < packed_size; ii++)
                        BODY(ii);
#undef BODY
        } else {
                unsigned packed_size = size/4;
                v4u * restrict pmask = (v4u*)mask;
                v4u * restrict pdst = (v4u*)dst;
                v4u * restrict px = (v4u*)x;
                v4u * restrict py = (v4u*)y;
                v4u flip = {0,0,0,0};
                if (flipped)
                        flip = ~flip;

#ifdef PREDICATE
#define BODY(I) do {                                    \
                        unsigned i = (I);               \
                        v4u mask = pmask[i]^flip;       \
                        v4u x = px[i], y = py[i];       \
                        v4u r = (v4u)OP(x, y)&mask;     \
                        PSUMMARISE(r);                  \
                        pdst[i] = r;                    \
                } while (0)
#else
#define BODY(I) do {                                    \
                        unsigned i = (I);               \
                        v4u mask = pmask[i]^flip;       \
                        v4u x = px[i], y = py[i];       \
                        v4u r = (v4u)OP(x, y)&mask;     \
                        pdst[i] = r;                    \
                } while (0)
#endif
                for (unsigned ii = 0; ii < packed_size; ii+=4)
                        for (unsigned j = 0; j < 4; j++)
                                BODY(ii+j);

                for (unsigned ii = packed_size&(-4u); ii < packed_size; ii++)
                        BODY(ii);
#undef BODY
        }

#ifdef PREDICATE
        SCALAR_SUMMARY;
#endif

        unsigned flip = flipped? -1U : 0;
        for (unsigned i = size&(-4UL); i < size; i++) {
                unsigned r = 0;
                if (mask[i]^flip) {
                        unsigned vx = x[i], vy = y[i];
#ifndef SCALAR_OP
                        r = OP(vx, vy);
#else
                        r = SCALAR_OP(vx, vy);
#endif
#ifdef PREDICATE
                        r = r?-1U:0;
#endif
                }
#ifdef PREDICATE
                SUMMARISE(r);
#endif
                dst[i] = r;
        }

#ifdef PREDICATE
        RETURN_SUMMARY;
#else
        return 0;
#endif
}
