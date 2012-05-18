int NAME (unsigned size, unsigned * restrict mask, int mask_summary,
          int flipped, int for_mask,
          double * restrict dst, double * restrict x, double * restrict y)
{
        if (flipped)
                mask_summary = -mask_summary;

        if (mask_summary == -1) {
                if (for_mask)
                        bzero(dst, size*sizeof(double));
                return -1;
        }

        if ((mask_summary == 1) || !for_mask) {
                unsigned packed_size = size/2;
                v2d * restrict pdst = (v2d*)dst;
                v2d * restrict px = (v2d*)x;
                v2d * restrict py = (v2d*)y;

#define BODY(I) do {                                    \
                        unsigned i = (I);               \
                        v2d x = px[i], y = py[i];       \
                        pdst[i] = OP(x, y);             \
                } while (0)
                        
                for (unsigned ii = 0; ii < packed_size; ii+=4)
                        for (unsigned j = 0; j < 4; j++)
                                BODY(ii+j);
                for (unsigned ii = packed_size&(-4U); ii < packed_size; ii++)
                        BODY(ii);
#undef BODY
        } else {
                unsigned packed_size = size/4;
                v4u * restrict pmask = (v4u*)mask;
                v2d * restrict pdst = (v2d*)dst;
                v2d * restrict px = (v2d*)x;
                v2d * restrict py = (v2d*)y;
                v4u flip = {0,0,0,0};
                if (flipped)
                        flip = ~flip;

#define BODY(I) do {                                                    \
                        unsigned i = (I);                               \
                        v4u mask = pmask[i]^flip;                       \
                        v4u mask_lo = __builtin_shufflevector(mask, mask, \
                                                              0, 0, 1, 1); \
                        v4u mask_hi = __builtin_shufflevector(mask, mask, \
                                                              2, 2, 3, 3); \
                        {                                               \
                                v2d x = px[i*2], y = py[i*2];           \
                                v2d r = OP(x, y);                       \
                                v2ul masked = (v2ul)r & (v2ul)mask_lo;  \
                                pdst[i*2] = (v2d)masked;                \
                        }                                               \
                        {                                               \
                                v2d x = px[i*2+1], y = py[i*2+1];       \
                                v2d r = OP(x, y);                       \
                                v2ul masked = (v2ul)r & (v2ul)mask_hi;  \
                                pdst[i*2+1] = (v2d)masked;              \
                        }                                               \
                } while (0)

                for (unsigned ii = 0; ii < packed_size; ii+=4)
                        for (unsigned j = 0; j < 4; j++)
                                BODY(ii+j);
                for (unsigned ii = packed_size&(-4U); ii < packed_size; ii++)
                        BODY(ii);
#undef BODY
        }

        unsigned flip = flipped? -1U : 0;
        for (unsigned i = size&(-4UL); i < size; i++) {
                if (!(mask[i]^flip)) continue;
                double vx = x[i], vy = y[i];
#ifndef SCALAR_OP
                dst[i] = OP(vx, vy);
#else
                dst[i] = SCALAR_OP(vx, vy);
#endif
        }

        return 0;
}
