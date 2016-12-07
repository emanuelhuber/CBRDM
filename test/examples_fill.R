

rm(list=ls(all=TRUE))

ROOT   <- "/home/huber/WORK/UNIBAS/"
DIR0   <- "RESEARCH/coarse_braided_deposits/Auswertung/OBJECT_BASED/"
DIR1   <- "09_package/CBRDM/"
DIR   <- file.path(ROOT,DIR0, DIR1)
DIR   <- file.path("/media/data/huber/Documents/WORK_NEWNEW/CBRDM")
# DIR   <- file.path("/home/huber/GWMODR", sep="")
setwd(DIR)
getwd()

source("main.R")


o <- trough(pos   = matrix(runif(9, 10,90), nrow=3, ncol=3),
            size  = cbind(rnorm(3, 40, 5), rnorm(3, 20,2), rnorm(3, 2, 0.5)),
            theta = runif(3,0,3.14),
            rH    = rep(6, 3))

o2 <- crossBedding(o)


plotTopView(o2[[1]], border = "red", asp = 1, col = "grey")
plotTopView(o[[1]], border = "blue", asp = 1, add = TRUE)

plotTopView(o2, border = "red", asp = 1, col = "grey")

pts <- locator(type="p",n=2)
l_pts <- joinLine(pts)  # line joining the two points
RConics::addLine(l_pts, col="blue")

so2 <- section(o2, l_pts)
plotSection(so2, border = "red")


##----------------------------- SPOON -------------------------##
o <- spoon(pos   = matrix(runif(9, -10,10), nrow=3, ncol=3),
            size  = cbind(rnorm(3, 60, 5), rnorm(3, 20,2), rnorm(3, 2, 0.15)),
            theta = runif(3,0,3.14),
            rH    = rep(6, 3),
            rL    = c(0.4, 0.1, 0.35))

# return a list of two elements
to <- spoon2trough(o)

plotTopView(o[[1]], asp = 1)

plotTopView(to[[1]], asp = 1, border = "red")
plotTopView(to[[2]], asp = 1, add = TRUE, border = "blue")

plotTopView(bbox(to[[1]]), border = "red", add = TRUE)
plotTopView(bbox(to[[2]]), border = "blue", add = TRUE)

xy0 <- c(l * cos(o@theta), 

b <- bbox(o[[1]])
plotTopView(b, add = TRUE, lwd = 1)


L1 <- x@L * (1 - x@rL)
L2 <- x@L * x@rL
l <- L1 - x@L/2
pos[,1:2] <- matrix(l*c(cos(-x@theta), sin(x@theta)),
                    ncol = 2, byrow = FALSE) + x@pos[,1:2]
pos[,1] <- l
rot <- matrix(c( cos(x@theta), sin(x@theta), 
                              -sin(x@theta), cos(x@theta)),
                              byrow = TRUE, nrow = 2,ncol = 2)
scl <- obj@W / obj@L
xyFRot <- oF@pos[,1:2] %*% t(rot)


plotTopView(b, add = TRUE, lwd = 2)
points( x@pos[,1:2], pch = 20)
points( pos[,1:2], pch = 20, col = "red")


points(b@pos[,1:2])

obj <- trough(pos   = c(0, 0, 0),
              size  = c(50, 15, 3),
              theta = 0.5,
              rH    = 6)
bbox(obj)
plotTopView(obj)
points(0,0, pch = 4)
boundary(obj)
              
              
objInit <- function(n, pos = NULL,  para = NULL, pre = 2){
  E <- matrix(0, nrow=n,ncol=13)
  colnames(E) <- c("ID", "x", "y", "z","theta", 
                      "a","b","c","zmax","L","W","H","rH" )
  # constant for object into ellipse
  # a = cstO2E * L
  # b = cstO2E * C
  cstO2E <- para$rH/(2*sqrt(2*para$rH -1))
  if(is.null(pos)){
    pos <- list("x" = c(0), "y" = c(0), "z" = c(0))
  }
  E[, "ID"] <- c(1:n)
  E[, "x"] <- pos$x
  E[, "y"] <- pos$y
  E[, "theta"] <- para$theta * pi/180
  E[, "L"] <- para$L
  E[, "W"] <- para$W
  E[, "H"] <- para$H
  E[, "rH"] <- para$rH
  E[, "a"] <-  para$L * cstO2E
  E[, "b"] <-  para$W * cstO2E
  E[, "c"] <-   para$rH * para$H
  E[, "zmax"] <- pos$z
  E[, "z"] <- E[, "zmax"] + E[, "c"] - para$H
  test1 <- E[, "zmax"] < E[, "z"]               # test zmax < z
  test2 <- E[, "zmax"] > E[, "z"] -   E[, "c"]  # test zmax > z - c
  return( round(E[test1 & test2, , drop=FALSE], pre) )
}


obj <- trough(pos   = c(5, -9, 0),
              size  = c(50, 15, 3),
              theta = pi/7,
              rH    = 6)
              
o <- as(obj, "Sphere")
boundary(o)
plotTopView(o, asp = 1)


oF <- fillingReg(obj, nF = 6, phi = 1.05, rpos = 1)

plotTopView(obj, add = FALSE, border = "green")
plotTopView(oF, add = TRUE, border = "blue")

# number of foresets
nF <- 6
xyzr <- c(0.5, 1, 0.5)

# position first thing
rL <- 0.75
phi <- 2.5

# compute Dr = radius smallest fill
Dr <- obj@L/2/(nF + 1)


# fillings length
rF <- cumsum(rep(2 * Dr, nF))/2

# available length
oL2 <- (o@r - rF[1])

# fillings positions
xy0 <- o@pos[1:2] + oL2 * rL * c(cos(phi), sin(phi))
xF <- seq(xy0[1], pos[1], length = nF + 1)[-(nF + 1)]
yF <- seq(xy0[2], pos[2], length = nF + 1)[-(nF + 1)]


oF <- new("Sphere",
          id = seq_along(rF),
          pos = cbind(xF, yF, 0),
          r = rF
        )

plotTopView(o, asp = 1)
plotTopView(oF, add = TRUE, border = "blue")
plotTopView(obj, add = TRUE, border = "green")


rot <- matrix(c( cos(obj@theta), sin(obj@theta), 
                              -sin(obj@theta), cos(obj@theta)),
                              byrow = TRUE, nrow = 2,ncol = 2)
scl <- obj@W / obj@L
xyFRot <- oF@pos[,1:2] %*% t(rot)
xyFScl <- xyFRot
xyFScl[,2] <- xyFScl[,2] * scl
xyFSclRot <- xyFScl %*% (rot)

points(oF@pos[,1:2])
points(xyFRot)
points(xyFScl)
points(xyFSclRot)


objF <-  new("Trough",
            version="0.1",
            id = oF@id,
            pos = cbind(xyFSclRot, 0) + 
                    matrix(obj@pos, nrow=length(oF@id), ncol = 3, byrow = TRUE),
            L = oF@r * 2,
            W = oF@r * 2 * obj@W / obj@L,
            H = obj@H,
            theta = obj@theta,  # depth position
            rH = obj@rH
        )
plotTopView(objF, add = TRUE)
        
        
# fillings positions
rF0 <- r2 / 2 * rL

xyF0 <- r2[1:2] / 2 * xyzr[1:2] * sign(xyzr[1:2])

oF <- new("Sphere",
          id = seq_along(rF),
          pos = matrix(0, nrow = length(rF), ncol = 3),
          r = rF
        )

plotTopView(o, asp = 1)

plotTopView(oF, add = TRUE, border = "blue")
        
        
obj1 <- new("Ellipse",
            id    = 1,
            pos   = matrix(c(0, 0), nrow =1, ncol=2),
            a     = obj@L/2, 
            b     = obj@L/2,
            theta = 0)
            
plotObj(obj1)


x <- obj1
if(add == FALSE){
  b <- boundary(x)
  plot(0,0, type = "n", xlab = xlab, ylab = ylab, main = main,
        xlim = b[c("xmin", "xmax")], ylim = b[c("ymin", "ymax")],
        asp = asp)
}
E <- as.matrix(x)
if(length(E) > 0 ){
  invisible(apply(E, 1, .plotEllipse, ...) )
}else{
  cat("no objects to plot!\n")
}

obj <- trough(pos   = c(0, 0, 0),
              size  = c(50, 15, 3),
              theta = pi/7,
              rH    = 6)

# relative position between -1 and 1
#       0 = middle
xyzr <- c(0.5, 1, 0.5)

# number of foresets
nF <- 6

x1 <- obj@L * obj@W / sqrt(obj@W^2 + obj@L^2 * tan(obj@theta)^2)
y1 <- obj@W * sqrt( 1 - (x1 / obj@L)^2 )


# compute available length, width, height
LWH <- c(obj@L, obj@W, obj@H)
DLWH <- LWH/2/(nF + 1)
LWH2 <- LWH - 2 * DLWH

# compute length, width, height
LF <- cumsum(rep(2 * DLWH[1], nF))
WF <- cumsum(rep(2 * DLWH[2], nF))
HF <- cumsum(rep(2 * DLWH[3], nF))

# compute position
pos <- obj@pos
xyF0 <- LWH2[1:2] / 2 * xyzr[1:2] * sign(xyzr[1:2])
theta <- obj@theta
xyF0 <- xyF0 %*% matrix(c( cos(theta), sin(theta), -sin(theta), cos(theta)),
                  byrow = TRUE, nrow = 2,ncol = 2)

xF <- seq(xyF0[1], pos[1], length = nF + 1)[-(nF + 1)]
yF <- seq(xyF0[2], pos[2], length = nF + 1)[-(nF + 1)]

fill <- trough(pos   = cbind(xF,yF,obj@pos[3]),
               size  = cbind(LF, WF, HF),
               theta = rep(obj@theta, nF),
               rH    = rep(obj@rH, nF))

plotTopView(obj, border = "red")
plotTopView(fill, border = "green", add = TRUE)


points(cbind(xF,yF))
points(xyF0, col = "red", pch = 20)
points(PFrot)

W <- obj@W
DW <- W/2/(nF + 1)
W2 <- W - 2*DW

H <- obj@H
DH <- H/2/(nF + 1)
H2 <- H - 2*DH

phi <- 2 * pi * c(0, 0.25)
P <- matrix(nrow = 2, ncol = 2)
P[,1] <- obj@L * cos(phi) / 2
P[,2] <- obj@W * sin(phi) / 2
theta <- obj@theta
P2 <- (P) %*% matrix(c( cos(theta), sin(theta), -sin(theta), cos(theta)),
                  byrow = TRUE, nrow = 2,ncol = 2)
P2
b <- boundary(obj)
plot(0,0, type = "n", xlim = b[c("xmin", "xmax")], ylim = b[c("ymin", "ymax")],
            asp = 1)
E <- as.matrix(obj)
polygon(RConics::ellipse(saxes = E[, c("L", "W")]/2, 
                         loc   = E[, c("x", "y")], 
                         theta = E[, c("theta")]))
plotTopView(obj, add = TRUE, border = "red")
points(0,0, pch = 4)
points((P2))
grid()

obj@fill <- list(pos = "type1",
                 
phi <- 2*pi*seq(0,1, len = n)
    P <- matrix(nrow=n,ncol=2)
    P[,1] <- saxes[1] * cos(phi)
P[,2] <- saxes[2] * sin(phi)
              
o <- trough(pos   = matrix(runif(9, -10,10), nrow=3, ncol=3),
            size  = cbind(rnorm(3, 40, 5), rnorm(3, 20,2), rnorm(3, 2, 0.5)),
            theta = runif(3),
            rH    = rep(6, 3))

E <- as(o, "TrEllipsoid")
E
as.matrix(E) 

o2 <- as(E, "Trough")
as.matrix(o) - as.matrix(o2)


Cubo <- as(o, "Cuboid")
Cubo
CubE <- as(E, "Cuboid")
CubE

as.matrix(Cubo) - as.matrix(CubE)

Spho <- as(o, "Sphere")
Spho
SphE <- as(E, "Sphere")
SphE

as.matrix(SphE) - as.matrix(Spho)


bbox(o)
boundary(o)




as(o, "matrix")
as.matrix(o)


bbo <- boundary(o)
plotTopView(o, main = "test")
rect(bbo["xmin"], bbo["ymin"], bbo["xmax"], bbo["ymax"], 
     border = "blue", lwd = 2)

grid <- list(x = round(bbo[1:2]),
             y = round(bbo[3:4]),
             z = round(bbo[5:6]),
             dx = 1, dy = 1, dz = 0.1)
Pix <- pixelise(o, grid)
# horizontal xy
plot3D::image2D(z = Pix$XYZ[,,5], x = Pix$x, y = Pix$y)
# vertical xz
plot3D::image2D(z = Pix$XYZ[,5,], x = Pix$x, y = Pix$z)
# vertical yz
plot3D::image2D(z = Pix$XYZ[5,,], x = Pix$y, y = Pix$z)
# volume
Pix$vol



plot(0,0, xlim=c(-50,50),ylim=c(-50,50))


plotTopView(o, main = "test", lwd = 3, asp = 1)
pts <- locator(type="p",n=2)
l_pts <- joinLine(pts)  # line joining the two points
RConics::addLine(l_pts, col="blue")

plotTopView(as(o, "TrEllipsoid"), add = TRUE, border = "red")
plotTopView(as(o, "Sphere"), add = TRUE, border = "red")
plotTopView(bbox(o), add = TRUE, border = "blue")
plotTopView(bbox(as(o, "TrEllipsoid")), add = TRUE, border = "blue")
plotTopView(as(o, "Cuboid"), add = TRUE, border = "blue")
plotTopView(bbox(as(o, "Cuboid")), add = TRUE, border = "green")



measureDistance()


sO <-section(o, l_pts)
sO
E <- as(o, "TrEllipsoid")
sE <- section(E, l_pts)
sE
as.matrix(sO) - as.matrix(as(sE, "Trough2D"))
as.matrix(as(sO, "TrEllipse")) - as.matrix(sE)

sT <- as(sE, "Trough2D")
sE@zmax - sT@pos[,2]
sE2 <- as(sT, "TrEllipse")
as.matrix(sE2) - as.matrix(sE)
sE2@pos[,2] - sE@pos[,2]

b <- sT@rH * sT@H
sE@b - b
sT@pos[,2] - sT@H + b


E@pos[,3]
plotSection(sE, lwd = 6)
plotSection(sO, add = TRUE, border = "red")

bbox(sE)
as(sE, "Trough2D")
boundary(bbox(sE))
plotObj(bbox(sE), add = TRUE, lwd = 4)
plotObj(bbox(as(sE, "Trough2D")), add = TRUE, border = "green")

sO <- section(o, l_pts)
sO
doIntersect(as(o, "Sphere"), l_pts)
doIntersect(o, l_pts)
