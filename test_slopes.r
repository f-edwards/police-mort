# varying slopes
blk.stan = stan_glmer(d.black ~ ur.code + (1 + ur.code|division),
                      prior_intercept=normal((log(0.94)-log(100000)), 10), #for prior intercept, based on krieger estimates
                      prior = normal(0, 2.5), #weakly informative, no difference from big urban
                      prior_covariance = decov(1, 1, 1, 1), #default
                      data = tmp2, offset=I(log(black+1)), 
                      family="neg_binomial_2", iter=2000, chains=4)
                                                                              

wht.stan = stan_glmer(d.white ~ ur.code + (1|division),
                      prior_intercept=normal((log(0.37)-log(100000)), 10), #for prior intercept, based on krieger estimates
                      prior = normal(0, 2.5), #weakly informative, no difference from big urban
                      prior_covariance = decov(1, 1, 1, 1), #default
                      data = tmp2, offset=log(white), family="neg_binomial_2", iter=2000, chains=4)

lat.stan = stan_glmer(d.latino ~ ur.code + (1|division),
                      prior_intercept=normal((log(0.37)-log(100000)), 10), #for prior intercept, based on krieger estimates
                      prior = normal(0, 2.5), #weakly informative, no difference from big urban
                      prior_covariance = decov(1, 1, 1, 1), #default
                      data = tmp2, offset=I(log(latino+1)), family="neg_binomial_2", iter=2000, chains=4)

save.image("slopes.RData")