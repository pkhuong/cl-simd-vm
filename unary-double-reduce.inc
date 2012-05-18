int NAME (unsigned size, unsigned * restrict mask, int mask_summary,
          int flipped, int for_mask,
          double * restrict dst, double * restrict src)
{
        if (flipped)
                mask_summary = -mask_summary;

        if (mask_summary == -1)
                return 0;

        if (mask_summary == 1) {
                unsigned packed_size = size/2;
                v2d acc0 = *(v2d*)dst,
                    acc1 = {NEUTRAL, NEUTRAL},
                    acc2 = acc1,
                    acc3 = acc1;
                v2d * restrict psrc = (v2d*)src;
                for (unsigned i = 0; i < packed_size; i+=4) {
                        {
                                v2d x = psrc[i];
                                acc0 = OP(acc0, x);
                        }
                        {
                                v2d x = psrc[i];
                                acc1 = OP(acc1, x);
                        }
                        {
                                v2d x = psrc[i];
                                acc2 = OP(acc2, x);
                        }
                        {
                                v2d x = psrc[i];
                                acc3 = OP(acc3, x);
                        }
                }
                for (unsigned i = packed_size&(-4U); i < packed_size; i++) {
                                v2d x = psrc[i];
                                acc0 = OP(acc0, x);                        
                }
                acc0 = OP(acc0, acc1);
                acc2 = OP(acc2, acc3);
                *(v2d*)dst = OP(acc0, acc2);

                {
                        unsigned flip = flipped? -1U : 0;
                        double acc = *dst;
                        for (unsigned i = size&(-2UL); i < size; i++) {
                                if (!(mask[i]^flip)) continue;
                                double x = src[i];
#ifndef SCALAR_OP
                                acc = OP(acc, x);
#else
                                acc = SCALAR_OP(acc, x);
#endif
                        }
                        *dst = acc;
                }
                return 0;
        }

        unsigned packed_size = size/4;
        v4u * restrict pmask = (v4u*)mask;
        v2d acc0 = *(v2d*)dst,
            acc1 = *(v2d*)(dst+2);
        v2d * restrict psrc = (v2d*)src;
        v2d neutral = {NEUTRAL, NEUTRAL};
        v4u flip = {0,0,0,0};
        if (flipped)
                flip = ~flip;
        
        for (unsigned i = 0; i < packed_size; i++) {
                v4u mask = pmask[i]^flip;
                v4u mask_lo = __builtin_shufflevector(mask, mask, 0, 0, 1, 1);
                v4u mask_hi = __builtin_shufflevector(mask, mask, 2, 2, 3, 3);
                {
                        v4u src = (v4u)psrc[i*2] & mask_lo;
                        src |= (v4u)neutral&(~mask_lo);
                        acc0 = OP(acc0, (v2d)src);
                }
                {
                        v4u src = (v4u)psrc[i*2+1] & mask_hi;
                        src |= (v4u)neutral&(~mask_hi);
                        acc1= OP(acc1, (v2d)src);
                }
        }
        *(v2d*)dst = acc0;
        *(v2d*)(dst+2) = acc1;
        {
                unsigned flip = flipped? -1U : 0;
                double acc = *dst;
                for (unsigned i = size&(-3UL); i < size; i++) {
                        if (!(mask[i]^flip)) continue;
                        double x = src[i];
#ifndef SCALAR_OP
                        acc = OP(acc, x);
#else
                        acc = SCALAR_OP(acc, x);
#endif
                }
                *dst = acc;
        }

        return 0;
}
