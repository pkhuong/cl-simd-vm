void NAME (void ** values, unsigned * properties,
           unsigned long start, unsigned size,
           unsigned mask_idx, int flipped,
           unsigned dst_idx,
           unsigned x_idx, unsigned y_idx)
{
        (void)start;
        unsigned * restrict mask = values[mask_idx];
        unsigned * restrict dst = values[dst_idx];
        double   * restrict x = values[x_idx];
        double   * restrict y = values[y_idx];
        int mask_summary = 0;
        int for_mask = properties[dst_idx] & MASK_FLAG;

        if (properties[mask_idx] & CONSTANT_FLAG) {
                if(*mask == 0) mask_summary = -1;
                if(*mask == -1U) mask_summary = 1;
        }

        if (flipped)
                mask_summary = -mask_summary;

        if (mask_summary == -1) {
                if (for_mask) {
                        bzero(dst, size*sizeof(double));
                        properties[dst_idx] |= CONSTANT_FLAG;
                }
                return;
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

                for (unsigned ii = 0; ii+4 <= packed_size; ii+=4)
                        for (unsigned j = 0; j < 4; j++)
                                BODY(ii+j);
                for (unsigned ii = (packed_size&(-4U)); ii < packed_size; ii++)
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

                for (unsigned ii = 0; ii+4 <= packed_size; ii+=4)
                        for (unsigned j = 0; j < 4; j++)
                                BODY(ii+j);
                for (unsigned ii = (packed_size&(-4U)); ii < packed_size; ii++)
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

        SUMMARY_VALUE(dst_idx);
}
