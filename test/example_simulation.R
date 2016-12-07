
## TODO: PIXELISE 
##            > Deposits
##            > Trough2D
##            > Deposits2D
## TODO: STRAUSS MODEL
## TODO: SUBSURFACE FLOW SIMULATION



rm(list=ls(all=TRUE))

library(devtools)
devtools::install_github("emanuelhuber/CBRDM")
library(CBRDM)


modbox <- list("x" = c(0,200),    # 0, 700    # before: 0, 500
             "y" = c(0,200),    # 0, 500    # before: 100, 400
             "z" = c(0,10)      # for computation range
             )
             
prior <- list("L"   = list(type = "runif", min = 40, max = 70),
              "rLW" = list(type = "runif", min = 3, max = 4),
              "rLH" = list(type = "runif", min = 45, max = 66),
              "theta" = list(type = "runif", min = -20 * pi / 180, 
                                             max = 20 * pi / 180),
              "rH" = 2,
              "ag" = 0.05,
              "lambda" = 0.001,
              "alpha" = -3,
              "beta" = 20,
              "r" = 70,
              "fd" = c(2,1),
              "nF" = list(type = "runif", min = 2, max = 5),
              "rpos" = list(type = "runif", min = 0.65, max = 1), 
              "phi" = list(type = "runif", min = -1.5, max = 1.5)
              )

mod <- sim(modbox, "poisson", prior)

plotTopView(mod, border = "red", col = "grey", asp = 1)

pts <- locator(type="p",n=2)
l_pts <- joinLine(pts)  # line joining the two points
RConics::addLine(l_pts, col="blue")

smod <- section(mod, l_pts)
plotSection(smod, border = "red", col = "grey", asp = 2, ylim = c(0, 10))
plotSection(smod@troughs, border = "red", col = "grey", ylim = c(5,15))


mod2 <- crossBedding(mod)
smod2 <- section(mod2, l_pts)
plotSection(smod2, border = "red", col = "grey", asp = 5, 
            ylim = c(5,15), lwd = 0.5, lay = list(lw = 0.1))

            
# pixeling
grid <- c(mod@bbox, list(dx = 5, dy = 5, dz = 0.1))

x <- mod


Pix <- pixelise(mod, grid)
# horizontal xy
plot3D::image2D(z = Pix$XYZ[,,5], x = Pix$x, y = Pix$y)
# vertical xz
plot3D::image2D(z = Pix$XYZ[,5,], x = Pix$x, y = Pix$z)
# vertical yz
plot3D::image2D(z = Pix$XYZ[5,,], x = Pix$y, y = Pix$z)
# volume
Pix$vol

range(as.vector(Pix$XYZ))

test <- function(x, a = 1, ...){
  list(...)
}
test(1, 2,3)

## cuboid - line intersection
  section(bbox(mod), l_pts)

## pixel section
x <- smod
grid <- c(mod@bbox, list(dx = 5, dy = 5, dz = 0.1))
            
x <- smod2@troughs
y <- mod2@troughs

i <- 0

i<- i + 1
par(mfrow = c(2,1))
plotSection(x[[i]],border = "red", col = "grey", asp = 5, 
            ylim = c(5,15), lwd = 0.5, main = x[[i]]@id)
plotTopView(y[[x[[i]]@id]],border = "red", col = "grey", asp = 5, 
            ylim = c(5,15), lwd = 0.5, main = x[[i]]@id)
RConics::addLine(l_pts, col="blue")
            

i <- 0

i<- i + 1
i<- 302

plotTopView(y[[i]],border = "red", col = "grey", asp = 5, 
            ylim = c(5,15), lwd = 0.5, main = y[[i]]@id)
RConics::addLine(l_pts, col="blue")

do.call(prior$L$type, list(10, prior$L$min, prior$L$max))



# ax + by + c
l_pts

l <- l_pts

b <- bbox(mod)

# corners
crns <- .rect(b@pos[1:2], b@L, b@W, b@theta)

ls <- list()
ls$bot <- RConics::join(c(crns[4, ], 1), c(crns[1, ], 1))
ls$lef <- RConics::join(c(crns[1, ], 1), c(crns[2, ], 1))
ls$top <- RConics::join(c(crns[2, ], 1), c(crns[3, ], 1))
ls$rig <- RConics::join(c(crns[3, ], 1), c(crns[4, ], 1))

ls$top <- RConics::join(c(mod@bbox$x[1], mod@bbox$y[2], 1),
                      c(mod@bbox$x[2], mod@bbox$y[2], 1))
ls$bot <- RConics::join(c(mod@bbox$x[1], mod@bbox$y[1], 1),
                      c(mod@bbox$x[2], mod@bbox$y[1], 1))
ls$lef <- RConics::join(c(mod@bbox$x[1], mod@bbox$y[1], 1),
                      c(mod@bbox$x[1], mod@bbox$y[2], 1))
ls$rig <- RConics::join(c(mod@bbox$x[2], mod@bbox$y[1], 1),
                      c(mod@bbox$x[2], mod@bbox$y[2], 1))

plot(0, type = "n", ylim = c(-50, 250), xlim = c(-50, 250))                  
invisible(sapply(ls, RConics::addLine))
RConics::addLine(l, col = "red")

fJoin <- function(x, l){
  RConics::join(x, l)
}
pts <- lapply(ls, RConics::join,  l)
invisible(sapply(pts, function(x, ...) points(t(x), ...)))

fSel <- function(p, xmin, xmax, ymin, ymax){
    (p[1] >= xmin & p[1] <= xmax) & (p[2] >= ymin & p[2] <= ymax)
}

test <- sapply(pts, fSel, xmin = mod@bbox$x[1], xmax = mod@bbox$x[2],
                  ymin = mod@bbox$y[1], ymax = mod@bbox$y[2])


                  




