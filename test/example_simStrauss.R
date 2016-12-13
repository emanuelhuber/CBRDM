

## TODO: STRAUSS MODEL
## TODO: SUBSURFACE FLOW SIMULATION



rm(list=ls(all=TRUE))

library(devtools)
devtools::install_github("emanuelhuber/CBRDM")
library(CBRDM)
library(spatstat)



prior <- list("L"      = list(type = "runif", min = 40, max = 70),
              "rLW"    = list(type = "runif", min = 3, max = 4),
              "rLH"    = list(type = "runif", min = 45, max = 66),
              "theta"  = list(type = "runif", min = -20 * pi / 180, 
                                             max = 20 * pi / 180),
              "rH"     = 2,
              "ag"     = 0.5,
              "lambda" = 0.001,
              "bet"    = 50,
              "gam"    = 0.1,
              "d"      = 100,
              "nit"    = 1e5,
              "n0"     = 1,
              "fd"     = c(2,1),
              "nF"     = list(type = "runif", min = 2, max = 5),
              "rpos"   = list(type = "runif", min = 0.65, max = 1), 
              "phi"    = list(type = "runif", min = -1.5, max = 1.5)
              )

modbox2 <- list("x" = c(0, 900),    # 0, 700    # before: 0, 500
                "y" = c(0, 1800),    # 0, 500    # before: 100, 400
                "z" = c(0, 5)      # for computation range
               )



f <- max(modbox2$x, modbox2$y)
f2 <- 1
area <- (modbox2$x[2]/f*f2 *  modbox2$y[2]/f*f2)
mod01 <- list(cif = "strauss", par = list(beta = prior$bet, #*area, 
              gamma = prior$gam, r = prior$d/f), 
              w = c(modbox2$x/f*f2, modbox2$y/f*f2))
X0 <- rmh(model = mod01, start=list( n.start = 2),
                  control = list(nrep = 1e6))
plot(X0, asp = 1)   
X0$n
area
X0$n*area

X1 <- rStrauss(beta = prior$bet, gamma = prior$gam, R = prior$d/f, 
                W = owin(modbox2$x/f, modbox2$y/f))
plot(X1)
plot(X1$x*f, X1$y*f, xlim = modbox2$x, ylim = modbox2$y, asp = 1)
X1$n

A <- dist(cbind(X1$x*f, X1$y*f))
hist(A)


prior$bet
[1] 100
prior$gam
[1] 0.1
prior$d/f
[1] 0.05555556
owin(modbox2$x/f, modbox2$y/f)
window: rectangle = [0, 0.5] x [0, 1] units


Xx <- rStrauss(beta  = prior$bet, 
               gamma = prior$gam, 
               R     = prior$d/f, 
               W     = owin(modbox2$x/f, modbox2$y/f))
plot(Xx)















modbox <- list("x" = c(0,100),    # 0, 700    # before: 0, 500
             "y" = c(0,100),    # 0, 500    # before: 100, 400
             "z" = c(0,10)      # for computation range
             )

prior$ag <- 0.5
prior$ag <- 0.05    # 5 cm

mod <- sim(modbox, hmodel = "poisson", prior)
mod <- sim(modbox, hmodel = "strauss", prior)


plotTopView(mod, border = "red", col = "grey", asp = 1)

lv <- c(1, 0, -50)
lh <- c(0, 1, -50)
RConics::addLine(lv, col = "blue", lwd = 3)
RConics::addLine(lh, col = "black", lwd = 4)



l <- lv       # perpendicular
l <- lh       # paralell
smod <- section(mod, l)
plotSection(smod, border = "red", col = "grey", asp = 2, ylim = c(0, 10),
            xlim = c(0,100))
         
mbox <- list(x = c(0, 100), z = c(0,5), dx = 1, dy = 1, dz = 0.01)
FAC <- pixelise(smod, mbox)
plot3D::image2D(z = FAC$z, x = FAC$x, y = FAC$y)

B <- setProp(FAC$z, type = c("K"), depprop)
B2 <- setProp(FAC$z, type = c("facies"))

par(oma = c(0,0,0,0), xpd = TRUE)
plot3D::image2D(z = B2, x = FAC$x, y = FAC$y, asp = 2, colkey = FALSE,
                col = c("lightsalmon4", "lightcyan4", "gold"))
legend("top", inset = c(-0,-0.1), 
        cex = 1.5, 
        bty = "n", 
        #xpd = TRUE,
        legend = c("gp", "ow", "bm"), 
        horiz = TRUE,
        fill = c("lightsalmon4", "lightcyan4", "gold"))
#         pch = c(15))
par(xpd = NA)
plot3D::image2D(z = B,  x = FAC$x, y = FAC$y, asp = 2, 
                main ="hydraulic conductivity (m/s)")

















pts <- locator(type="p",n=2)
l_pts <- joinLine(pts)  # line joining the two points
RConics::addLine(l_pts, col="blue")

lv <- c(1, 0, -100)
lh <- c(0, 1, -100)
RConics::addLine(lv, col = "blue", lwd = 3)
RConics::addLine(lh, col = "black", lwd = 4)


smod <- section(mod, l_pts)
plotSection(smod, border = "red", col = "grey", asp = 2, ylim = c(0, 10))
plotSection(smod@troughs, border = "red", col = "grey", ylim = c(5,15))

# perpendicular
smod <- section(mod, lv)
plotSection(smod, border = "red", col = "grey", asp = 2, ylim = c(0, 10),
            xlim = c(0,100))
            
# paralell
smod <- section(mod, lv)
plotSection(smod, border = "red", col = "grey", asp = 2, ylim = c(0, 10),
            xlim = c(0,100))
            
mbox <- list(x = c(0, 100), z = c(0,10), dx = 1, dy = 1, dz = 0.01)
FAC <- pixelise(smod, mbox)
plot3D::image2D(z = FAC$z, x = FAC$x, y = FAC$y)

B <- setProp(FAC$z, type = c("K"), depprop)
B2 <- setProp(FAC$z, type = c("facies"))

par(oma = c(0,0,0,0), xpd = TRUE)
plot3D::image2D(z = B2, x = FAC$x, y = FAC$y, asp = 2, colkey = FALSE,
                col = c("lightsalmon4", "lightcyan4", "gold"))
legend("top", inset = c(-0,-0.25), 
        cex = 1.5, 
        bty = "n", 
        #xpd = TRUE,
        legend = c("gp", "ow", "bm"), 
        horiz = TRUE,
        fill = c("lightsalmon4", "lightcyan4", "gold"))
#         pch = c(15))

plot3D::image2D(z = B,  x = FAC$x, y = FAC$y, asp = 2, 
                main ="hydraulic conductivity (m/s)")

mod2 <- crossBedding(mod)
smod2 <- section(mod2, l_pts)
plotSection(smod2, border = "red", col = "grey", asp = 5, 
            ylim = c(5,15), lwd = 0.5, lay = list(lw = 0.1))


## pixelise model
mbox <- c(mod@bbox, list(dx = 1, dy = 1, dz = 0.05))
Pix <- pixelise(mod, mbox)
# horizontal xy
plot3D::image2D(z = Pix$XYZ[,,5], x = Pix$x, y = Pix$y)
# vertical xz
plot3D::image2D(z = Pix$XYZ[,5,], x = Pix$x, y = Pix$z)
# vertical yz
plot3D::image2D(z = Pix$XYZ[5,,], x = Pix$y, y = Pix$z)
# volume
# Pix$vol


A <- setProp(Pix$XYZ, type = c("facies"))
# vertical yz
plot3D::image2D(z = A[5,,], x = Pix$y, y = Pix$z)


## pixelise section
mbox <- c(mod@bbox, list(dx = 1, dy = 1, dz = 0.01))
FAC <- pixelise(smod, mbox)
plot3D::image2D(z = FAC$z, x = FAC$x, y = FAC$y)

B <- setProp(FAC$z, type = c("K"), depprop)
B2 <- setProp(FAC$z, type = c("facies"))

plot3D::image2D(z = B,  x = FAC$x, y = FAC$y, asp = 5)
plot3D::image2D(z = B2, x = FAC$x, y = FAC$y, asp = 5)




##--- STRAUSS PROCESS

X0 <- straussMH(nit = 50000)
plot(X0$X)
plot(X0$n, type = "l")
hist(X0$n, 50)

X1 <- straussMH(bet = 10, gam = 1, d = 0.05, nit = 50000, n0 = 100)
plot(X1$X)
plot(X1$n, type = "l")
hist(X1$n, 50)

X2 <- straussMH(bet = 50, gam = 0.5, d = 0.2, nit = 50000, n0 = 60, 
                count = TRUE)
plot(X2$X)
plot(X2$n, type = "l")
hist(X2$n)

# Strauss process.
bet <- 2
gam <- 0.2
r <- 0.7

mod01 <- list(cif = "strauss", par = list(beta = bet, gamma = gam, r = r),
              w = c(0, 10, 0, 10))
X0 <- rmh(model = mod01, start=list( n.start = 2),
                  control = list(nrep = 1e5, nverb = 10000))
plot(X0$x, X0$y, xlim = c(0, 10), ylim = c(0, 10))
X0$n

X1 <- rStrauss(beta = bet, gamma = gam, R = r, W = square(10))
plot(X1$x, X1$y, xlim = c(0, 10), ylim = c(0, 10))
X1$n

frStrauss <- function(...){
  rStrauss(...)$n
}

nX1 <- replicate(10000, frStrauss(beta = bet, gamma = gam, R = r, 
                                  W = square(10)))
hist(nX1)                                  


X2 <- straussMH(bet = bet/3, gam = 0.2, d = r, nit = 1e4, n0 = 1, 
                W = list(x = c(0, 10), y = c(0,10)), count = TRUE)
plot(X2$X)
plot(X2$n, type = "l")
hist(X2$n)


system.time({
A <- replicate(10000, distxtoX2(X, x_cand))
})

system.time({
A <- replicate(10000, distxtoX3(X, x_cand))
})


X1 <- rStrauss(beta = bet, gamma = gam, R = 8, W = square(10))
plot(X1$x, X1$y, xlim = c(0, 10), ylim = c(0, 10))
X1$n

modbox <- list("x" = c(0, 100),    # 0, 700    # before: 0, 500
               "y" = c(0, 100),    # 0, 500    # before: 100, 400
               "z" = c(0, 5)      # for computation range
             )

prior <- list("L"      = list(type = "runif", min = 40, max = 70),
              "rLW"    = list(type = "runif", min = 3, max = 4),
              "rLH"    = list(type = "runif", min = 45, max = 66),
              "theta"  = list(type = "runif", min = -20 * pi / 180, 
                                             max = 20 * pi / 180),
              "rH"     = 2,
              "ag"     = 0.01,
              "lambda" = 0.001,
              "bet"    = 0.01,
              "gam"    = 0.2,
              "d"      = 100,
              "nit"    = 1e5,
              "n0"     = 1,
              "fd"     = c(2,1),
              "nF"     = list(type = "runif", min = 2, max = 5),
              "rpos"   = list(type = "runif", min = 0.65, max = 1), 
              "phi"    = list(type = "runif", min = -1.5, max = 1.5)
              )

# saveRDS(list(prior = prior, modbox = modbox, mod = mod), 
#         file = "sim01_strauss_veryhighAg.rds")

modbox2 <- list("x" = c(0, 900),    # 0, 700    # before: 0, 500
                "y" = c(0, 900),    # 0, 500    # before: 100, 400
                "z" = c(0, 5)      # for computation range
               )
               
# X <- straussMH(bet = prior$bet, gam = prior$gam, d = prior$d, 
#                nit = prior$nit, n0  = prior$n0,  W = modbox2, fd = prior$fd,
#                count = TRUE)

mod01 <- list(cif = "strauss", par = list(beta = prior$bet, gamma = prior$gam, 
              r = prior$d), w = c(modbox2$x, modbox2$y))
X0 <- rmh(model = mod01, start=list( n.start = 2),
                  control = list(nrep = 1e7))
plot(X0$x, X0$y, xlim = modbox2$x, ylim = modbox2$y)   
X0$n
X0
names(X0)

X1 <- rStrauss(beta = prior$bet, gamma = prior$gam, R = prior$d, 
              W = owin(modbox2$x, modbox2$y))
plot(X1$x, X1$y, xlim = c(0, 10), ylim = c(0, 10))
X1$n


plot(X$X, xlim = modbox2$x, ylim = modbox2$y)
plot(X$n, type = "l", ylim = c(0, 50))
hist(X$n)



mod <- sim(modbox, hmodel = "strauss", prior, crossbeds = TRUE)

mod2 <- crossBedding(mod, prior)


plotTopView(mod, border = "red", col = "grey", asp = 1)
plotTopView(mod2, border = "red", col = "grey", asp = 1)

mod3 <- extract(mod, modbox)
mod3 <- extract(mod, modbox = list(x = c(50, 400), 
                                   y = c(100, 500), 
                                   z = c(-1,     6)))

plotTopView(mod3, border = "black", col = "green", asp = 1, add = TRUE)
rect(modbox$x[1], modbox$y[1], modbox$x[2], modbox$y[2])



plotTopView(mod, border = "red", col = "grey", asp = 1)
lv <- c(1, 0, -50)
lh <- c(0, 1, -50)
RConics::addLine(lv, col = "blue", lwd = 3)
RConics::addLine(lh, col = "black", lwd = 4)



l <- lv       # perpendicular
l <- lh       # paralell
smod <- section(mod, l)
plotSection(smod, border = "red", col = "grey", asp = 2, ylim = c(0, 10),
            xlim = c(0,100))
         
mbox <- list(x = c(0, 100), z = c(0,5), dx = 1, dy = 1, dz = 0.01)
FAC <- pixelise(smod, mbox)
plot3D::image2D(z = FAC$z, x = FAC$x, y = FAC$y)

B <- setProp(FAC$z, type = c("K"), depprop)
B2 <- setProp(FAC$z, type = c("facies"))

par(oma = c(0,0,0,0), xpd = TRUE)
plot3D::image2D(z = B2, x = FAC$x, y = FAC$y, asp = 2, colkey = FALSE,
                col = c("lightsalmon4", "lightcyan4", "gold"))
legend("top", inset = c(-0,-0.1), 
        cex = 1.5, 
        bty = "n", 
        #xpd = TRUE,
        legend = c("gp", "ow", "bm"), 
        horiz = TRUE,
        fill = c("lightsalmon4", "lightcyan4", "gold"))
#         pch = c(15))
par(xpd = NA)
plot3D::image2D(z = B,  x = FAC$x, y = FAC$y, asp = 2, 
                main ="hydraulic conductivity (m/s)")





plot(XL[[1]])
plot(XL[[2]], type = "l")













# 
# F <- FAC
# F$z[] <- NA
# F$z[gp] <- 0
# F$z[bm] <- 1
# F$z[ow] <- 2
# plot3D::image2D(gp)
# plot3D::image2D(bm)
# plot3D::image2D(ow)
# 
# F <- FAC
# F$z[] <- NA
# ugp <- unique(FAC$z[gp])
# ngp <- length(ugp)
# gpK <- runif(ngp, 0, 1)
# gpK <- rlognorm(ngp, mean = depprop$gp["K"], sdlog = depprop$gp["K"])
# for(k in seq_len(ngp)){
#   F$z[FAC$z == ugp[k]] <- gpK[k] 
#   sum(FAC$z == ugp[k])
# }
# 
# F <- FAC
# F$z[] <- NA
# ugp <- unique(FAC$z[bm])
# ngp <- length(ugp)
# gpK <- runif(ngp, 0, 1)
# gpK <- rlognorm(ngp, mean = depprop$bm["K"], sdlog = depprop$bm["K"])
# for(k in seq_len(ngp)){
#   F$z[FAC$z == ugp[k]] <- gpK[k] 
#   sum(FAC$z == ugp[k])
# }
# 
# 
# GP <- FAC$z
# GP[] <- NA
# GP[gp]
# plot3D::image2D(z = F$z, x = F$x, y = F$y)
# 
# plotSection(smod, border = "red", col = "grey", asp = NA, ylim = c(0, 10),
#             ylim = c(0, 250))
# 
# 
# vl <- "gp"            
# ugp <- unique(FAC$z[bm])
# ngp <- length(ugp)
# gpK <- runif(ngp, 0, 1)
# gpK <- rlognorm(ngp, mean = depprop[[vl]]["K"], sdlog = depprop[[vl]]["K"])   
  
#        
#             
# range(as.vector(Pix$XYZ))
# 
# test <- function(x, a = 1, ...){
#   list(...)
# }
# test(1, 2,3)
# 
# smod <- section(mod, l)
# 
# ## cuboid - line intersection
# plot(0, type = "n", ylim = c(-50, 250), xlim = c(-50, 250))                  
# pts <- locator(type="p",n=2)
# l <- joinLine(pts)  # line joining the two points
# RConics::addLine(l, col="blue")
# pp <- section(bbox(mod), l)
# smod <- section(mod, l)
# xy <- smod@troughs@pos
# range(xy[,1])
# 
# plotSection(smod)
# 
# -sign(l[1])*sign(l[2]) * sqrt(sum((xy[1,] - pp[[1]][1:2] )^2))
# ref <- -sign(l[1])*sign(l[2]) * sqrt(sum((pp[[1]][1:2] - c(-l[3]/l[1],0) )^2))
# 
# xy0 <- xy[,1] - ref
# range(xy0)
# 
#  myloc <- ifelse(l[1] != 0 && l[2] != 0, 
#                       -sign(l[1])*sign(l[2]) *
#                       sqrt(sum((xy[1,] - pp[[1]][1:2] )^2)),
#                       newLoc[l == 0][1])
# 
# ## pixel section
# x <- smod
# mbox <- c(mod@bbox, list(dx = 5, dy = 5, dz = 0.1))
#             
# x <- smod2@troughs
# y <- mod2@troughs
# 
# i <- 0
# 
# i<- i + 1
# par(mfrow = c(2,1))
# plotSection(x[[i]],border = "red", col = "grey", asp = 5, 
#             ylim = c(5,15), lwd = 0.5, main = x[[i]]@id)
# plotTopView(y[[x[[i]]@id]],border = "red", col = "grey", asp = 5, 
#             ylim = c(5,15), lwd = 0.5, main = x[[i]]@id)
# RConics::addLine(l_pts, col="blue")
#             
# 
# i <- 0
# 
# i<- i + 1
# i<- 302
# 
# plotTopView(y[[i]],border = "red", col = "grey", asp = 5, 
#             ylim = c(5,15), lwd = 0.5, main = y[[i]]@id)
# RConics::addLine(l_pts, col="blue")
# 
# do.call(prior$L$type, list(10, prior$L$min, prior$L$max))
# 
# 
# 
# # ax + by + c
# l_pts
# 
# l <- l_pts
# 
# b <- bbox(mod)
# 
# # corners
# crns <- .rect(b@pos[1:2], b@L, b@W, b@theta)
# 
# ls <- list()
# ls$bot <- RConics::join(c(crns[4, ], 1), c(crns[1, ], 1))
# ls$lef <- RConics::join(c(crns[1, ], 1), c(crns[2, ], 1))
# ls$top <- RConics::join(c(crns[2, ], 1), c(crns[3, ], 1))
# ls$rig <- RConics::join(c(crns[3, ], 1), c(crns[4, ], 1))
# 
# ls$top <- RConics::join(c(mod@bbox$x[1], mod@bbox$y[2], 1),
#                       c(mod@bbox$x[2], mod@bbox$y[2], 1))
# ls$bot <- RConics::join(c(mod@bbox$x[1], mod@bbox$y[1], 1),
#                       c(mod@bbox$x[2], mod@bbox$y[1], 1))
# ls$lef <- RConics::join(c(mod@bbox$x[1], mod@bbox$y[1], 1),
#                       c(mod@bbox$x[1], mod@bbox$y[2], 1))
# ls$rig <- RConics::join(c(mod@bbox$x[2], mod@bbox$y[1], 1),
#                       c(mod@bbox$x[2], mod@bbox$y[2], 1))
# 
# plot(0, type = "n", ylim = c(-50, 250), xlim = c(-50, 250))                  
# invisible(sapply(ls, RConics::addLine))
# RConics::addLine(l, col = "red")
# 
# fJoin <- function(x, l){
#   RConics::join(x, l)
# }
# pts <- lapply(ls, RConics::join,  l)
# invisible(sapply(pts, function(x, ...) points(t(x), ...)))
# 
# fSel <- function(p, xmin, xmax, ymin, ymax){
#     (p[1] >= xmin & p[1] <= xmax) & (p[2] >= ymin & p[2] <= ymax)
# }
# 
# test <- sapply(pts, fSel, xmin = mod@bbox$x[1], xmax = mod@bbox$x[2],
#                   ymin = mod@bbox$y[1], ymax = mod@bbox$y[2])
# 
# 
#                   




