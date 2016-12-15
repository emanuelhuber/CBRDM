


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


prior <- list("L"      = list(type = "runif", min = 40, max = 70),
              "rLW"    = list(type = "runif", min = 3, max = 4),
              "rLH"    = list(type = "runif", min = 45, max = 66),
              "theta"  = list(type = "runif", min = 90-20 * pi / 180, 
                                             max = 90+20 * pi / 180),
              "rH"     = 2,
              "ag"     = 0.5,
              "lambda" = 0.001,
              "bet"    = 1e-4,
              "gam"    = 0.2,
              "d"      = 100,
              "nit"    = 1e5,
              "n0"     = 1,
              "fd"     = c(2,1),
              "nF"     = list(type = "runif", min = 2, max = 5),
              "rpos"   = list(type = "runif", min = 0.65, max = 1), 
              "phi"    = list(type = "runif", min = -1.5, max = 1.5)
              )
              

##---- high aggradation
# id <- "highAg" # model run identifier
# prior$ag <- 0.005    # 5 cm

##---- low aggradation
id <- "lowAg"
prior$ag <- 0.05

# 10 cm vertical auf lösung!
# braucht es "layers" oder only 3D array?
modbox <- list("x" = c(0,100),    # 0, 700    # before: 0, 500
               "y" = c(0,100),    # 0, 500    # before: 100, 400
               "z" = c(0,10)      # for computation range
              )


##------------ PARAMETERS ----------------##
modgrid <- list(L = c(min = modbox$x[1], max = modbox$x[2]),
                W = c(min = modbox$y[1], max = modbox$y[2]),
                H = diff(modbox$z),
                nx = 200,      # number of cells (x axis)
                ny = 200,      # number of cells (y axis)
                nz = 200)      # number of cells (z axis)
grad_hyd <- 0.01
##----------------------------------------##

##------------ GEOMETRIC MODEL -----------##
mod <- sim(modbox, hmodel = "strauss", prior)
##----------------------------------------##


##-------- SPATIAL DISCRETISATION --------##
valleyFloor <- function(x,y, a = 0.02, b = 10){
    z <- b - y * a
    return(z)
}

gwMod <- modGrid3D(modgrid, fun = valleyFloor, a = -grad_hyd, b = 10)
names(gwMod)
##----------------------------------------##

##--------- HYDRAULIC PROPERTIES ---------##
data(faciesProp)

mbox <- list(x = modbox$x, y = modbox$y, z = modbox$z, 
            dx = diff(modbox$x)/modgrid$nx, 
            dy = diff(modbox$y)/modgrid$ny, 
            dz = diff(modbox$z)/modgrid$nz)
FAC <- pixelise(mod, mbox)

A <- setProp(FAC$XYZ, type = c("facies"))
plot3D::image2D(A[,,30])
plot3D::image2D(A[,30,])
B <- FAC$XYZ[,30,]
B1 <- B
B1[B>= 0] <- 0
plot3D::image2D(B1)

HK <- setProp(FAC$XYZ, type = c("K"), fprop = faciesProp)
plot3D::image2D(HK[,,30])
plot3D::image2D(HK[30,,])
plot3D::image2D(HK[,30,])

VANI <- setProp(FAC$XYZ, type = c("Kvani"), fprop = faciesProp)
plot3D::image2D(VANI[,,30])
plot3D::image2D(VANI[30,,])
plot3D::image2D(VANI[,30,])

PORO <- setProp(FAC$XYZ, type = c("p"), fprop = faciesProp)
plot3D::image2D(PORO[,,30])
plot3D::image2D(PORO[30,,])

#--- HK
for(i in 1:nlay(gwMod)){
  r <- gwMod[[paste0("lay",i,".bot")]]
  r[] <- HK[,,i]
  names(r) <- paste0("lay",i,".hk")
  gwMod <- stackRaster(gwMod, r)
}

#--- vani
for(i in 1:nlay(gwMod)){
  r <- gwMod[[paste0("lay",i,".bot")]]
  r[] <- VANI[,,i]
  names(r) <- paste0("lay",i,".vani")
  gwMod <- stackRaster(gwMod, r)
}

#--- porosity
for(i in 1:nlay(gwMod)){
  r <- gwMod[[paste0("lay",i,".bot")]]
  r[] <- PORO[,,i]
  names(r) <- paste0("lay",i,".porosity")
  gwMod <- stackRaster(gwMod, r)
}
##----------------------------------------##


##------- SPECIFIED-HEAD CONDITIONS ------##
#---- specified-head locations
rCHD <- gwMod[[1]]
rCHD[] <- NA
ztopmax <- cellStats(gwMod[["lay1.top"]], max)
rCHD[1,] <- ztopmax
rCHD[nrow(rCHD),] <- ztopmax - grad_hyd*(yFromRow(rCHD, row = 1) - 
              yFromRow(rCHD, row = nrow(rCHD)))
plot(rCHD)
names(rCHD) <- "chd"
gwMod <- stackRaster(gwMod, rCHD)

plot(gwMod[[1]])

rcCHD   <- rowColFromCell(rCHD, which(!is.na(values(rCHD))))
val <- valueFromRowCol(rCHD, rcCHD)
CHDFrame <- corCHD(gwMod, rcCHD, val, "ss")
##----------------------------------------##



##-------------- INITIAL HEADS -----------##
r <- gwMod[[1]]
r[] <- ztopmax - grad_hyd*(yFromCell(rCHD, ncell(rCHD):1) - 
                           yFromRow(rCHD, row=nrow(rCHD)))
gwMod <- initialHeads(gwMod, r) 
cat("grad_hyd = ", grad_hyd, "\n")
plot(gwMod[["lay1.strt"]])
plot(gwMod[["lay1.strt"]] - gwMod[[1]])
##----------------------------------------##


##--------------- PARTICLES --------------##

vp <- seq(5, to = nlay(gwMod), by = 5)
vx <- seq(5, to = ncol(gwMod), by = 5)
PPP <- matrix(nrow = length(vp) * length(vx), ncol = 8)
names(PPP) <- c("Layer", "Row", "Column", "LocalX", "LocalY", "LocalZ", 
                "ReleaseTime", "Label")
                
for(i in seq_along(vp)){
  vr <- (i-1) * length(vx) + seq_along(vx)
  PPP[vr, 1] <- vp[i]
  PPP[vr, 2] <- 1
  PPP[vr, 3] <- vx
  PPP[vr, 4:6] <- 0.5
  PPP[vr, 7] <- 0
  PPP[vr, 8] <- i
}

particles <- list(as.data.frame(PPP))
##----------------------------------------##


##---------------- MODFLOW/MODPATH RUN -------------------##
dirproj <- file.path(getwd(), "gwflw_simulation")
dir.create(path = dirproj)
dirrun <- file.path(dirproj, id)




arguments <- list(rs.model       = gwMod, 
                  chd            = CHDFrame,
                  id             = id, 
                  dir.run        = dirrun, 
                  ss.perlen      = 1L, 
                  is.convertible = TRUE,
                  uni            = c("seconds","meters"),
                  timeFormat     = "%Y%m%d",
                  verbose        = TRUE)

do.call(WriteModflowInputFiles, arguments)

# Create and execute a batch ﬁle containing commands that run MODFLOW-USG.
output <- runModflowUsg(dirpath = dirrun, id = id, exe = "mfusg")

##--- Initial heads = output previous simulation
# read head files
fhds <- file.path(dirrun , paste0(id , ".hds"))
rhead <- get.heads(fhds,kper = 1, kstp = 1, r = gwMod[[1]])
# if(exists("rs.head")){
gwMod <- initialHeads(gwMod, rhead) 
arguments[["rs.model"]] <- gwMod
do.call(WriteModflowInputFiles, arguments)
output <- runModflowUsg(dirpath = dirrun, id = id, exe = "mfusg")



fhds <- file.path(dirrun , paste0(id , ".hds"))
rhead <- get.heads(fhds,kper = 1, kstp = 1, r = gwMod[[1]])

i <- 45
plot(rhead[[i]])
poly <- r2polygons(gwMod,n = which(t(A[,,i]) == 2))
plotPoly(poly, col=rgb(0.1,0.1,0.2,0.2))
contour(rhead[[i]], levels=seq(0,30,by=0.05), add=TRUE)

#plot(gwMod[[paste0("lay",i,".hk")]])
#contour(rhead[[i]], levels=seq(0,30,by=0.05), add=TRUE)
  

fbud <- file.path(dirrun , paste0(id , ".bud"))

writeModpathInputFiles(id          = id,
                       dir.run     = dirrun,
                       optionFlags = c("StopOption" = 2), 
                       fbud        = fbud,
                       rs.model    = gwMod,
                       particles   = particles,
                       unconfined  = TRUE,
                       verbose     = TRUE)

   
outputMP <- runModpath(dirpath = dirrun, id = id, exe = "mp6", 
                       batFile = "runModpath.bat")   
##--------------------------------------------------------##

fhds <- file.path(dirrun , paste0(id , ".hds"))
rhead <- get.heads(fhds,kper = 1, kstp = 1, r = gwMod[[1]])

partE <- readParticles(id, dirrun, type="end")
partP <- readParticles(id, dirrun, type="path", ext=extent3D(gwMod))   

saveRDS(particles, file=file.path(dirrun,"particles.rds"))
saveRDS(gwMod, file=file.path(dirrun,"gwMod.rds"))
saveRDS(partE, file=file.path(dirrun,"partE.rds"))
saveRDS(partP, file=file.path(dirrun,"partP.rds"))
saveRDS(rhead, file=file.path(dirrun,"rhead.rds"))
saveRDS(mod, file=file.path(dirrun,"mod.rds"))
saveRDS(prior, file=file.path(dirrun,"prior.rds"))

# saveRDS(XYZ, file=file.path(dirrun,"XYZ.rds"))
# saveRDS(IDE, file=file.path(dir.out,"IDE.rds"))
# saveRDS(IDE, file=file.path(getwd(),"IDE.rds"))


i <- 25
plot(rhead[[i]])
points(partE[,c("x","y")], pch = 20, col = "blue")  # end (final)
plotPathXY(partP)

hist(partP[,"z"])

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

         
plot(gwMod[["lay95.bot"]])       