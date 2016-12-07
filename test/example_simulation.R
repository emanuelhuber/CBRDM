
## TODO: PIXELISE > DEPOSITS
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
             
prior <- list("L"   = list(type = "runif", min = 20, max = 50),
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
              "phi" = list(type = "runif", min = -1.0, max = 1.0)
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
grid <- c(mod@bbox, list(dx = 1, dy = 1, dz = 0.1))

Pix <- pixelise(mod, grid)
# horizontal xy
plot3D::image2D(z = Pix$XYZ[,,5], x = Pix$x, y = Pix$y)
# vertical xz
plot3D::image2D(z = Pix$XYZ[,5,], x = Pix$x, y = Pix$z)
# vertical yz
plot3D::image2D(z = Pix$XYZ[5,,], x = Pix$y, y = Pix$z)
# volume
Pix$vol

test <- function(x, a = 1, ...){
  list(...)
}
test(1, 2,3)

            
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


