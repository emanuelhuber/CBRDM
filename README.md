# CBRDM
Coarse Braided River Deposit Model

## How to install/load

```r
library(devtools)
devtools::install_github("emanuelhuber/CBRDM")
library(CBRDM)
```

## Notes
This is an ongoing project. If you have any questions, don't hesitate to contact me:

emanuel.huber@stanford.edu

Thank you!

## Short tutorial
### Trough fill simulation
This model simulates trough fills with cross-bedding and horizontal layers. The trough fills are approximated by a series of truncated ellipsoids.
We create an instance of the class `Trough` using the constructor function `trough()`. The objects properties and positions are drawn from random distribution:


```r
# Trough Fills
TF <- trough(pos   = matrix(runif(9, 10,90), nrow=3, ncol=3),
             size  = cbind(rnorm(3, 40, 5), rnorm(3, 20,2), 
                          rnorm(3, 2, 0.5)),
             theta = runif(3,0,3.14),
             rH    = rep(6, 3))
```

To visualise this trough fills, use the function
