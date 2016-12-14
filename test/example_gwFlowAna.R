
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


# readRDS(particles, file=file.path(dirrun,"particles.rds"))
gwMod <- readRDS(file=file.path(dirrun,"gwMod.rds"))
partE <- readRDS(file=file.path(dirrun,"partE.rds"))
partP <- readRDS(file=file.path(dirrun,"partP.rds"))
rhead <- readRDS(file=file.path(dirrun,"rhead.rds"))
# saveRDS(mod, file=file.path(dirrun,"mod.rds"))
# saveRDS(prior, file=file.path(dirrun,"prior.rds"))


myAsp <- 5
##----------------- PARTICLES 2D -------------------##
Cairo(file      = file.path(dirrun, "part2D_xz.png"), 
      type      = "png",
      units     = "in", 
      width     = 14, 
      height    = 8, 
      pointsize = 14, 
      dpi       = 300)
par(mfrow = c(1,2))
scatter2D(partE[,"x"], partE[,"z"], pch = 20, colvar = partE[,"x0"], 
          asp = myAsp, xlab = "y (m)", ylab = "z (m)",
          cex = 1, xaxs = "i", yaxs = "i", colkey = list(plot = FALSE),
          xlim = extent3D(gwMod)[1:2], ylim = extent3D(gwMod)[5:6] )
scatter2D(partE[,"x"], partE[,"z"], pch = 20, colvar = partE[,"z0"], 
          asp = myAsp, xlab = "y (m)", ylab = "z (m)",
          cex = 1, xaxs = "i", yaxs = "i", colkey = list(plot = FALSE),
          xlim = extent3D(gwMod)[1:2], ylim = extent3D(gwMod)[5:6] )
dev.off()
##----------------- PARTICLES 3D -------------------##

library(Cairo)
Cairo(file      = file.path(dirrun, "part3D_x.png"), 
      type      = "png",
      units     = "in", 
      width     = 8, 
      height    = 8, 
      pointsize = 14, 
      dpi       = 300)
points3D(partP[,"x"], partP[,"y"], partP[,"z"], 
        colvar = partE[partP[,"id"],"x0"], 
         bty = "f", cex = 0.01, 
         pch = 20, clab = "inflow y-position (m)", ticktype = "detailed",
         theta = 40 , expand = 5, scale = FALSE, xlim = extent3D(gwMod)[1:2], 
         ylim = extent3D(gwMod)[3:4], zlim = extent3D(gwMod)[5:6],
         xlab="y",ylab="x",shade=TRUE,border="black",
         colkey = list(width = 0.5, length = 0.5,cex.axis = 0.8, side = 1),
         col.axis = "black", col.panel = "white", col.grid = "grey", 
         lwd.panel = 1, lwd.grid = 2, box = TRUE)
dev.off()
         