


rm(list=ls(all=TRUE))

ROOT  <- "/home/huber/WORK/UNIBAS/"
source(paste(ROOT,"softwares/codeR/MODFLOW/RMODFLOW.R",sep=""), chdir=TRUE)

library(devtools)
devtools::install_github("emanuelhuber/CBRDM")
library(CBRDM)


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
              
# 10 cm vertical auf lösung!
# braucht es "layers" oder only 3D array?
modbox <- list("x" = c(0,100),    # 0, 700    # before: 0, 500
               "y" = c(0,100),    # 0, 500    # before: 100, 400
               "z" = c(0,10)      # for computation range
              )

prior$ag <- 0.5
prior$ag <- 0.005    # 5 cm
prior$bet <- 1e-4    # 5 cm
prior$gam <- 0.2    # 5 cm


mod <- sim(modbox, hmodel = "strauss", prior)



##------------ PARAMETERS ----------------##
modgrid <- list(L = c(min = modbox$x[1], max = modbox$x[2]),
                W = c(min = modbox$y[1], max = modbox$y[2]),
                H = diff(modbox$z),
                nx = 100,      # number of cells (x axis)
                ny = 100,      # number of cells (y axis)
                nz = 50)      # number of cells (z axis)
grad_hyd <- 0.01
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
mbox <- list(x = modbox$x, y = modbox$y, z = modbox$z, 
            dx = diff(modbox$x)/modgrid$nx, 
            dy = diff(modbox$y)/modgrid$ny, 
            dz = diff(modbox$z)/modgrid$nz)
FAC <- pixelise(mod, mbox)

A <- setProp(FAC$XYZ, type = c("facies"))
plot3D::image2D(A[,,30])
plot3D::image2D(A[,30,])
data(faciesProp)
HK <- setProp(FAC$XYZ, type = c("K"), fprop = faciesProp)

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
rCHD[1,] <- ztopmax + grad_hyd*(yFromRow(rCHD, row = 1) - 
              yFromRow(rCHD, row = nrow(rCHD)))
rCHD[nrow(rCHD),] <- ztopmax
plot(rCHD)
names(rCHD) <- "chd"
gwMod <- stackRaster(gwMod, rCHD)

rcCHD   <- rowColFromCell(rCHD, which(!is.na(values(rCHD))))
val <- valueFromRowCol(rCHD, rcCHD)
CHDFrame <- corCHD(gwMod, rcCHD, val, "ss")
##----------------------------------------##



##-------------- INITIAL HEADS -----------##
r <- gwMod[[1]]
r[] <- ztopmax + grad_hyd*(yFromCell(rCHD, 1:ncell(rCHD)) - yFromRow(rCHD, 
row=nrow(rCHD)))
gwMod <- initialHeads(gwMod, r) 
cat("grad_hyd = ", grad_hyd, "\n")
##----------------------------------------##


##--------------- PARTICLES --------------##
rnames <- c("lay1.top",paste0("lay",1:nlay(gwMod),".bot"))
PPP <- matrix(nrow=nlay(gwMod)*ncol(gwMod), ncol=8)
names(PPP) <- 
c("Layer","Row","Column","LocalX","LocalY","LocalZ","ReleaseTime","Label")
for(i in seq_len(nlay(gwMod))){
  PPP[(i-1)*ncol(gwMod) + 1:ncol(gwMod), 1] <- i
  PPP[(i-1)*ncol(gwMod) + 1:ncol(gwMod), 2] <- 1
  PPP[(i-1)*ncol(gwMod) + 1:ncol(gwMod), 3] <- 1:ncol(gwMod)
  PPP[(i-1)*ncol(gwMod) + 1:ncol(gwMod), 4:6] <- 0.5
  PPP[(i-1)*ncol(gwMod) + 1:ncol(gwMod), 7] <- 0
  PPP[(i-1)*ncol(gwMod) + 1:ncol(gwMod), 8] <- i
}

particles <- list(as.data.frame(PPP))
##----------------------------------------##


##---------------- MODFLOW/MODPATH RUN -------------------##
# id <- "threeFacies" # model run identifier
dir.out <- file.path(getwd(), id)
dir.create(path = dir.out)
dir.run <- file.path(dir.out, "Run")

saveRDS(particles, file=file.path(dir.out,"particles.rds"))
saveRDS(gwMod, file=file.path(dir.out,"gwMod.rds"))
saveRDS(CHDFrame, file=file.path(dir.out,"CHDFrame.rds"))
saveRDS(XYZ, file=file.path(dir.out,"XYZ.rds"))
saveRDS(XYZ, file=file.path(getwd(),"XYZ.rds"))
# saveRDS(IDE, file=file.path(dir.out,"IDE.rds"))
# saveRDS(IDE, file=file.path(getwd(),"IDE.rds"))


arguments <- list(rs.model = gwMod, 
             chd = CHDFrame,
                id = id, 
             dir.run = dir.run, 
           ss.perlen = 1L, 
      is.convertible = TRUE,
             uni = c("seconds","meters"),
              timeFormat = "%Y%m%d",
              verbose=TRUE)

do.call(WriteModflowInputFiles, arguments)

# Create and execute a batch ﬁle containing commands that run MODFLOW-USG.
output <- runModflowUsg(dirpath=dir.run, exe="mfusg")

##--- Initial heads = output previous simulation
# read head files
fhds <- file.path(dir.run , paste0(id , ".hds"))
rs.head <- get.heads(fhds,kper=1,kstp=1, r=gwMod[[1]])
# if(exists("rs.head")){
gwMod <- initialHeads(gwMod, rs.head) 
arguments[["rs.model"]] <- gwMod
do.call(WriteModflowInputFiles, arguments)
output <- runModflowUsg(dirpath=dir.run, exe="mfusg")



fbud <- file.path(dir.run , paste0(id , ".bud"))

writeModpathInputFiles(id=id,dir.run= dir.run,
            optionFlags = c("StopOption" = 2), 
            fbud=fbud,
            rs.model=gwMod,
            particles = particles,
            unconfined=TRUE,
            verbose = TRUE)

   
outputMP <- runModpath(dirpath=dir.run, exe="mp6", batFile="runModpath.bat")   
##--------------------------------------------------------##
