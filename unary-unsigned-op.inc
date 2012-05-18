int NAME (unsigned size, unsigned * restrict mask, int mask_summary,
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

#ifdef PREDICATE
        WITH_SUMMARY;
#else
        assert(!for_mask);
#endif
        if ((mask_summary == 1) || !for_mask) {
                unsigned packed_size = size/4;
                v4u * restrict pdst = (v4u*)dst;
                v4u * restrict psrc = (v4u*)src;
#ifdef PREDICATE
#define BODY(I) do {                            \
                        unsigned i = (I);       \
                        v4u x = psrc[i];        \
                        v4u r = OP(x);          \
                        PSUMMARISE(r);          \
                        pdst[i] = r;            \
                } while (0)
#else
#define BODY(I) do {                            \
                        unsigned i = (I);       \
                        v4u x = psrc[i];        \
                        v4u r = OP(x);          \
                        pdst[i] = r;            \
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
                v4u * restrict psrc = (v4u*)src;
                v4u flip = {0,0,0,0};
                if (flipped)
                        flip = ~flip;

#ifdef PREDICATE
#define BODY(I) do {                                    \
                        unsigned i = (I);               \
                        v4u mask = pmask[i]^flip;       \
                        v4u src = psrc[i];              \
                        v4u r = OP(src)&mask;           \
                        PSUMMARISE(r);                  \
                        pdst[i] = r;                    \
                } while (0)
#else
#define BODY(I) do {                                    \
                        unsigned i = (I);               \
                        v4u mask = pmask[i]^flip;       \
                        v4u src = psrc[i];              \
                        v4u r = OP(src)&mask;           \
                        pdst[i] = r;                    \
                } while (0)
#endif

                for (unsigned ii = 0; ii < packed_size; ii+=4)
                        for (unsigned j = 0; j < 4; j++)
                                BODY(ii+j);
                for (unsigned ii = packed_size&(-4U); ii < packed_size; ii++)
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
                        unsigned x = src[i];
#ifdef PREDICATE
                        r = OP(x)?-1U:0;
#else
                        r = OP(x);
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
