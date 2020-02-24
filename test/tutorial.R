
library(devtools)
devtools::install_github("emanuelhuber/CBRDM")
library(CBRDM)

TF <- trough(pos   = matrix(runif(9, 10,90), nrow=3, ncol=3),
             size  = cbind(rnorm(3, 40, 5), rnorm(3, 20,2), 
                          rnorm(3, 2, 0.5)),
             theta = runif(3,0,3.14),
             rH    = rep(6, 3))


plotTopView(TF, border = "blue", col = "grey", asp = 1)


plotTopView(TF[[2]], border = "blue", col = "green", asp = 1, add = TRUE)

para <- list(nF   = 10,      # nF cross-beds
             rpos = 0.75,    # 0 <= rpos <=1
             phi  = 2.2)     # orientation angle
              
TF <- crossBedding(TF, para)

plotTopView(TF, border = "blue", col = "grey", asp = 1)


para <- list(nF   = c(10, 3, 30),      # nF cross-beds
             rpos = c(0, 0.75, 1),     # 0 <= rpos <=1
             phi  = c(0, -1, 2.2))     # orientation angle
TF <- crossBedding(TF, para)
plotTopView(TF, border = "blue", col = "grey", asp = 1)



