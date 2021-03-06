
#--- physical properties facies
# Huggenberger (1993)
# Jussel et al (1994)
# Huber and Huggenberger (2016) doi:10.5194/hess-20-2035-2016
# p = porosity
# de = dielectric number saturated zone
#
faciesProp <- list(gp = c(p      = 0.201,
                          Kmean  = 1.5e-3,
                          Klogsd = 0.5,
                          Kvani  = 6,
                          de     = 12.1),
                   bm = c(p      = 0.25,
                          Kmean  = 1.5e-3,
                          Klogsd = 0.1,
                          Kvani  = 1,
                          de     = 9.2),
                   ow = c(p      = 0.35,
                          Kmean  = 1e-1,
                          Klogsd = 0.1,
                          Kvani  = 1,
                          de     = 26.9))