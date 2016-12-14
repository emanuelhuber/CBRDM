
#--- physical properties facies
# Huggenberger (1993)
# Jussel et al (1994)
# Huber and Huggenberger (2016) doi:10.5194/hess-20-2035-2016
# p = porosity
# de = dielectric number saturated zone
#
faciesProp <- list(gp = c(p      = 0.201,
                       K      = 1.5e-3,
                       sdlogK = 0.5,
                       vaniK  = 6,
                       de     = 12.1),
                bm = c(p      = 0.25,
                       K      = 1.5e-3,
                       sdlogK = 0.1,
                       vaniK  = 1,
                       de     = 9.2),
                ow = c(p      = 0.35,
                       K      = 1e-1,
                       sdlogK = 0.1,
                       vaniK  = 1,
                       de     = 26.9))