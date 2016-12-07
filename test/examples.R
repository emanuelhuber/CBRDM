

rm(list=ls(all=TRUE))

library(devtools)
devtools::install_github("emanuelhuber/CBRDM")
library(CBRDM)



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
