int NAME (unsigned size, unsigned * restrict mask, int mask_summary,
          int flipped, int for_mask,
          unsigned * restrict dst, double * restrict x, double * restrict y)
{
        if (flipped)
                mask_summary = -mask_summary;

        if (mask_summary == -1) {
                if (for_mask)
                        bzero(dst, size*sizeof(double));
                return -1;
        }

        WITH_SUMMARY;

        if ((mask_summary == 1) || !for_mask) {
                unsigned packed_size = size/4;
                v4u * restrict pdst = (v4u*)dst;
                v2d * restrict px = (v2d*)x;
                v2d * restrict py = (v2d*)y;

#define BODY(I) do {                                                    \
                        unsigned i = (I);                               \
                        v2d x = px[i*2], y = py[i*2];                   \
                        v4u lo = (v4u)OP(x, y);                         \
                        x = px[i*2+1]; y = py[i*2+1];                   \
                        v4u hi = (v4u)OP(x, y);                         \
                        v4u mixed = __builtin_shufflevector(lo, hi,     \
                                                            0, 2, 4, 6); \
                        PSUMMARISE(mixed);                              \
                        pdst[i] = mixed;                                \
                } while (0)

                for (unsigned ii = 0; ii < packed_size; ii+=4)
                        for (unsigned j = 0; j < 4; j++)
                                BODY(i+j);
                for (unsigned ii = packed_size&(-4U); ii < packed_size; ii++)
                        BODY(ii);
#undef BODY
        } else {
                unsigned packed_size = size/4;
                v4u * restrict pmask = (v4u*)mask;
                v4u * restrict pdst = (v4u*)dst;
                v2d * restrict px = (v2d*)x;
                v2d * restrict py = (v2d*)y;
                v4u flip = {0,0,0,0};
                if (flipped)
                        flip = ~flip;

#define BODY(I) do {                                                    \
                        unsigned i = (I);                               \
                        v4u mask = pmask[i]^flip;                       \
                        v4u hi, lo;                                     \
                        {                                               \
                                v2d x = px[i*2], y = py[i*2];           \
                                lo = (v4u)OP(x, y);                     \
                        }                                               \
                        {                                               \
                                v2d x = px[i*2+1], y = py[i*2+1];       \
                                hi = (v4u)OP(x, y);                     \
                        }                                               \
                        v4u mixed = __builtin_shufflevector(lo, hi,     \
                                                            0, 2, 4, 6)&mask; \
                        PSUMMARISE(mixed);                              \
                        pdst[i] = mixed;                                \
                } while (0)

                for (unsigned ii = 0; ii < packed_size; ii+=4)
                        for (unsigned j = 0; j < 4; j++)
                                BODY(ii+j);
                for (unsigned ii = packed_size&(-4U); ii < packed_size; ii++)
                        BODY(ii);
#undef BODY
        }

        SCALAR_SUMMARY;

        unsigned flip = flipped? -1U : 0;
        for (unsigned i = size&(-4UL); i < size; i++) {
                unsigned r = 0;
                if (mask[i]^flip) {
                        double vx = x[i], vy = y[i];
                        r = OP(vx, vy)? -1U: 0;
                }
                SUMMARISE(r);
                dst[i] = r;
        }

        RETURN_SUMMARY;
}
