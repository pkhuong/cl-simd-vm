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

        if (mask_summary == 1) {
                unsigned packed_size = size/4;
                v4u acc = *(v4u*)dst;
                v4u * restrict psrc = (v4u*)src;
                for (unsigned i = 0; i < packed_size; i++) {
                        v4u x = psrc[i];
                        acc = OP(acc, x);
                }
                *(v4u*)dst = acc;
        } else {
                unsigned packed_size = size/4;
                v4u * restrict pmask = (v4u*)mask;
                v4u acc = *(v4u*)dst;
                v4u * restrict psrc = (v4u*)src;
                v4u flip = {0,0,0,0};
                v4u neutral = {NEUTRAL, NEUTRAL, NEUTRAL, NEUTRAL};
                if (flipped)
                        flip = ~flip;

                for (unsigned i = 0; i < packed_size; i++) {
                        v4u mask = pmask[i]^flip;
                        v4u src = (psrc[i]&mask)|(neutral&(~mask));
                        acc = OP(acc, src);
                }
                *(v4u*)dst = acc;
        }

        unsigned flip = flipped? -1U : 0;
        unsigned acc = *dst;
        for (unsigned i = size&(-4UL); i < size; i++) {
                if (mask[i]^flip) {
                        unsigned x = src[i];
                        acc = OP(acc, x);
                }
        }
        *dst = acc;

        return 0;
}
