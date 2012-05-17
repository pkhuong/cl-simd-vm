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
                for (unsigned i = 0; i < packed_size; i++) {
                        v4u x = px[i], y = py[i];
                        v4u r = (v4u)OP(x, y);
#ifdef PREDICATE
                        PSUMMARISE(r);
#endif
                        pdst[i] = r;
                }
        } else {
                unsigned packed_size = size/4;
                v4u * restrict pmask = (v4u*)mask;
                v4u * restrict pdst = (v4u*)dst;
                v4u * restrict px = (v4u*)x;
                v4u * restrict py = (v4u*)y;
                v4u flip = {0,0,0,0};
                if (flipped)
                        flip = ~flip;

                for (unsigned i = 0; i < packed_size; i++) {
                        v4u mask = pmask[i]^flip;
                        v4u x = px[i], y = py[i];
                        v4u r = (v4u)OP(x, y)&mask;
#ifdef PREDICATE
                        PSUMMARISE(r);
#endif
                        pdst[i] = r;
                }
        }

#ifdef PREDICATE
        SCALAR_SUMMARY;
#endif

        unsigned flip = flipped? -1U : 0;
        for (unsigned i = size&(-4UL); i < size; i++) {
                unsigned r = 0;
                if (mask[i]^flip) {
                        unsigned vx = x[i], vy = y[i];
#ifdef PREDICATE
                        r = OP(vx, vy)?-1U:0;
#else
                        r = OP(vx, vy);
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
