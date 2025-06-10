# Updated 2.12.25 to change p to 10.4% (for MDD instead of MDE)

library(powerMediation)

## Largest comparison: White
minEffect.VSMc.logistic(n = 19664,
                        power = 0.8,
                        sigma.m = 1,
                        p = 0.104,
                        corr.xm = 0.1,
                        alpha = 0.05,
                        verbose = TRUE)
minEffect.VSMc.logistic(n = 19664,
                        power = 0.8,
                        sigma.m = 1,
                        p = 0.104,
                        corr.xm = 0.5,
                        alpha = 0.05,
                        verbose = TRUE)

## Smallest comparison: NHOPI
minEffect.VSMc.logistic(n = 589,
                        power = 0.8,
                        sigma.m = 1,
                        p = 0.104,
                        corr.xm = 0.1,
                        alpha = 0.05,
                        verbose = TRUE)
minEffect.VSMc.logistic(n = 589,
                        power = 0.8,
                        sigma.m = 1,
                        p = 0.104,
                        corr.xm = 0.5,
                        alpha = 0.05,
                        verbose = TRUE)
