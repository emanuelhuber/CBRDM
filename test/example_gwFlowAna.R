
rm(list=ls(all=TRUE))


ROOT  <- "/home/huber/WORK/UNIBAS/"
DIR0  <- file.path("RESEARCH/coarse_braided_deposits/Auswertung/OBJECT_BASED",
                   "09_package")
DIR     <- file.path(ROOT,DIR0)
setwd(DIR)
getwd()

source(paste(ROOT,"softwares/codeR/MODFLOW/RMODFLOW.R",sep=""), chdir=TRUE)

library(devtools)
devtools::install_github("emanuelhuber/CBRDM")
library(CBRDM)
library(plot3D)
library(Cairo)


data(faciesProp)

##---- id
id <- "highAg_saved" # model run identifier
id <- "lowAg"
id <- "highAg"

dirproj <- file.path(getwd(), "gwflw_simulation")
dirrun <- file.path(dirproj, id)

# readRDS(particles, file=file.path(dirrun,"particles.rds"))
gwMod <- readRDS(file=file.path(dirrun,"gwMod.rds"))
partE <- readRDS(file=file.path(dirrun,"partE.rds"))
partP <- readRDS(file=file.path(dirrun,"partP.rds"))
rhead <- readRDS(file=file.path(dirrun,"rhead.rds"))
mod   <- readRDS(file=file.path(dirrun,"mod.rds"))
# saveRDS(mod, file=file.path(dirrun,"mod.rds"))
prior <- readRDS(file=file.path(dirrun,"prior.rds"))


myAsp <-2
##----------------- PARTICLES 2D -------------------##
Cairo(file      = file.path(dirproj, paste0(id, "_part2D_xz.png")), 
      type      = "png",
      units     = "in", 
      width     = 14, 
      height    = 8, 
      pointsize = 14, 
      dpi       = 300)
par(mfrow = c(1,2))
scatter2D(partE[,"x"], max(partP[,"z"]) -partE[,"z"], pch = 20, 
          colvar = partE[,"x0"], 
          asp = myAsp, xlab = "y (m)", ylab = "z (m)",
          cex = 1, xaxs = "i", yaxs = "i", colkey = list(plot = FALSE),
          xlim = extent3D(gwMod)[1:2], ylim = extent3D(gwMod)[5:6] )
scatter2D(partE[,"x"], max(partP[,"z"]) -partE[,"z"], pch = 20, 
          colvar = partE[,"z0"], 
          asp = myAsp, xlab = "y (m)", ylab = "z (m)",
          cex = 1, xaxs = "i", yaxs = "i", colkey = list(plot = FALSE),
          xlim = extent3D(gwMod)[1:2], ylim = extent3D(gwMod)[5:6] )
dev.off()


##----------------- MODEL 3D - part -------------------##

library(Cairo)
Cairo(file      = file.path(dirproj, paste0(id, "_mod3D_x.png")), 
      type      = "png",
      units     = "in", 
      width     = 8, 
      height    = 8, 
      pointsize = 14, 
      dpi       = 300)
      
      
par(mfrow = c(1,1))
points3D(partE[,"x0"], partE[,"y0"], partE[,"z0"], 
        colvar = partE[,"z0"], 
         bty = "f", cex = 0.5, 
         pch = 20, clab = "inflow z-position (m)", ticktype = "detailed",
         theta = 40 , expand = 5, scale = FALSE, xlim = extent3D(gwMod)[1:2], 
         ylim = extent3D(gwMod)[3:4], zlim = extent3D(gwMod)[5:6],
         xlab="y",ylab="x",shade=TRUE,border="black",
         colkey = list(width = 0.5, length = 0.5,cex.axis = 0.8, side = 1),
         col.axis = "black", col.panel = "white", col.grid = "grey", 
         lwd.panel = 1, lwd.grid = 2, box = TRUE)
dev.off()
         

##----------------- PARTICLES 3D -------------------##

library(Cairo)
Cairo(file      = file.path(dirproj, paste0(id, "_part3D_x.png")), 
      type      = "png",
      units     = "in", 
      width     = 8, 
      height    = 8, 
      pointsize = 14, 
      dpi       = 300)
par(mfrow = c(1,1))
# points3D(partP[,"x"], partP[,"y"], partP[,"z"], 
#         colvar = partE[partP[,"id"],"x0"], 
#          bty = "f", cex = 0.01, 
#          pch = 20, clab = "inflow y-position (m)", ticktype = "detailed",
#          theta = 40 , expand = 5, scale = FALSE, xlim = extent3D(gwMod)[1:2],
#          ylim = extent3D(gwMod)[3:4], zlim = extent3D(gwMod)[5:6],
#          xlab="y",ylab="x",shade=TRUE,border="black",
#          colkey = list(width = 0.5, length = 0.5,cex.axis = 0.8, side = 1),
#          col.axis = "black", col.panel = "white", col.grid = "grey", 
#          lwd.panel = 1, lwd.grid = 2, box = TRUE)
points3D(partP[,"x"], partP[,"y"], partP[,"z"], 
        colvar = partE[partP[,"id"],"z0"], 
         bty = "f", cex = 0.01, 
         pch = 20, clab = "inflow z-position (m)", ticktype = "detailed",
         theta = 40 , expand = 5, scale = FALSE, xlim = extent3D(gwMod)[1:2], 
         ylim = extent3D(gwMod)[3:4], zlim = extent3D(gwMod)[5:6],
         xlab="y",ylab="x",shade=TRUE,border="black",
         colkey = list(width = 0.5, length = 0.5,cex.axis = 0.8, side = 1),
         col.axis = "black", col.panel = "white", col.grid = "grey", 
         lwd.panel = 1, lwd.grid = 2, box = TRUE)
dev.off()
         
dim(gwMod)

##----------------- SECTION -------------------##
modbox <- list("x" = c(0,100),    # 0, 700    # before: 0, 500
               "y" = c(0,100),    # 0, 500    # before: 100, 400
               "z" = c(0,10)      # for computation range
              )
modgrid <- list(L = c(min = modbox$x[1], max = modbox$x[2]),
                W = c(min = modbox$y[1], max = modbox$y[2]),
                H = diff(modbox$z),
                nx = 200,      # number of cells (x axis)
                ny = 200,      # number of cells (y axis)
                nz = 200)      # number of cells (z axis)
mbox <- list(x = modbox$x, y = modbox$y, z = modbox$z, 
            dx = diff(modbox$x)/modgrid$nx, 
            dy = diff(modbox$y)/modgrid$ny, 
            dz = diff(modbox$z)/modgrid$nz)
FAC <- pixelise(mod, mbox)


HK <- setProp(FAC$XYZ, type = c("K"), fprop = faciesProp)
# plot3D::image2D(HK[,,200])
# plot3D::image2D(HK[,,1])
# plot3D::image2D(HK[200,,])
# plot3D::image2D(HK[1,,])
# plot3D::image2D(HK[,1,])
# plot3D::image2D(HK[,200,])

Cairo(file      = file.path(dirproj, paste0(id,"_HK.png")), 
      type      = "png",
      units     = "in", 
      width     = 8, 
      height    = 8, 
      pointsize = 14, 
      dpi       = 300)
      
par(mfrow = c(1,1))
vy <- FAC$x
vx <- rep(100, length(FAC$x))
vz <- matrix(rep(FAC$z, each = length(vx)), ncol = length(vx), 
             nrow = length(vy), byrow = TRUE)
M1 <- mesh(vx, vy)

surf3D(M1$x, M1$y, vz, colvar = t(HK[,200,]), 
        col = jet2.col(201),
        bty = "f", cex = 0.01, pch = 20,  clim = range(HK),
        clab = "hydraulic conductivity (m/s)", ticktype = "detailed",
        theta = 40 , expand = 5, scale = FALSE, resfac = 0, clog = TRUE,
        xlim = modbox$x, ylim = modbox$y, 
        zlim = modbox$z, #extent3D(gwMod)[5:6],
        xlab = "x", ylab = "y", shade = TRUE, ltheta = -125, lphi = 45,
        colkey = list(plot = TRUE, width = 0.5, length = 0.5,
        cex.axis = 0.8, side = 1),
        col.axis = "black", col.panel = "white", col.grid = "grey",
        lwd.panel = 1, lwd.grid = 2, box = TRUE)

#image2D(HK[,200,], col = jet2.col(201) )       
        
vy <- (FAC$x)
vx <- rep(0, length(FAC$x))
vz <- matrix(rep(FAC$z, each = length(vx)), ncol = length(vx), 
             nrow = length(vy), byrow = TRUE)
M1 <- mesh(vx, vy)

surf3D(M1$y, M1$x, vz, colvar = t(HK[1,,]), 
        col = jet2.col(201), add = TRUE, expand = 5, scale = FALSE, 
        resfac = 0, clog = TRUE,  clim = range(HK),
        colkey = list(plot = FALSE))


vx <- rev(FAC$x)
vy <- FAC$y
vz <- matrix(rep(rep(max(FAC$z), length(FAC$z)), each = length(vx)), 
              ncol = length(vx), nrow = length(vy), byrow = TRUE)
M1 <- mesh(vx, vy)

surf3D(M1$y, M1$x, vz, colvar = (HK[,,199]), 
        col = jet2.col(201), add = TRUE, expand = 5, scale = FALSE, 
        resfac = 0, clog = TRUE,  clim = range(HK), 
        colkey = list(plot = FALSE))
        
dev.off()

##----------------- SECTION -------------------##
# plotTopView(mod, border = "red", col = "grey", asp = 1)
# 
lv <- c(1, 0, -50)
lh <- c(0, 1, -50)
# RConics::addLine(lv, col = "blue", lwd = 3)
# RConics::addLine(lh, col = "black", lwd = 4)


Cairo(file      = file.path(dirproj, paste0(id, "_section.png")), 
      type      = "png",
      units     = "in", 
      width     = 14, 
      height    = 8, 
      pointsize = 14, 
      dpi       = 300)
par(mfrow = c(2,1))

lpar <- lv       # perpendicular
lper <- lh       # paralell
smodper <- section(mod, lper)
smodpar <- section(mod, lpar)
plotSection(smodper, border = "red", col = "grey", asp = 2, ylim = c(0, 10),
            xlim = c(0,100), ylab = "z (m)", xlab = "x (m)")
title("Across valley orientation")
plotSection(smodpar, border = "red", col = "grey", asp = 2, ylim = c(0, 10),
            xlim = c(0,100), ylab = "z (m)", xlab = "x (m)")
title("Along valley orientation")
dev.off()


Cairo(file      = file.path(dirproj, paste0(id, "_section_prop.png")), 
      type      = "png",
      units     = "in", 
      width     = 14, 
      height    = 6.6, 
      pointsize = 14, 
      dpi       = 300)
par(mfrow = c(2,1))

mbox <- list(x = c(0, 100), z = c(0,5), dx = 1, dy = 1, dz = 0.01)
FACsec <- pixelise(smodpar, mbox)
#plot3D::image2D(z = FAC$z, x = FAC$x, y = FAC$y)

B <- setProp(FACsec$z, type = c("K"), fprop = faciesProp)
B2 <- setProp(FACsec$z, type = c("facies"))

par(xpd = TRUE)
plot3D::image2D(z = B2, x = FACsec$x, y = FACsec$y, asp = 2,
                col = c("lightsalmon4", "lightcyan4", "gold"),
                colkey = list(plot = FALSE))
legend("top", inset = c(-0,-0.7), 
        cex = 1.5, 
        bty = "n", 
        #xpd = TRUE,
        legend = c("gp", "ow", "bm"), 
        horiz = TRUE,
        fill = c("lightsalmon4", "lightcyan4", "gold"))
#         pch = c(15))
plot3D::image2D(z = B,  x = FACsec$x, y = FACsec$y, asp = 2, 
                main ="hydraulic conductivity (m/s)")
par(xpd = NA)

dev.off()

