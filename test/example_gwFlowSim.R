

ROOT  <- "/home/huber/WORK/UNIBAS/"
source(paste(ROOT,"softwares/codeR/MODFLOW/RMODFLOW.R",sep=""), chdir=TRUE)
# source(paste(ROOT,"softwares/codeR/utils/binaryConnections.R",sep=""))



# 10 cm vertical auf lösung!
# braucht es "layers" oder only 3D array?


##------------ PARAMETERS ----------------##
modgrid <- list(L = c(min = modbox$x[1], max = modbox$x[2]),
             W = c(min = modbox$y[1], max = modbox$y[2]),
             H = diff(modbox$z),
             nx = 100,      # number of cells (x axis)
             ny = 100,      # number of cells (y axis)
             nz = 500)      # number of cells (z axis)
grad_hyd <- 0.01
##----------------------------------------##


##-------- SPATIAL DISCRETISATION --------##
gwMod <- modGrid3D(modgrid)
names(gwMod)
##----------------------------------------##

##--------- HYDRAULIC PROPERTIES ---------##
mbox <- list(x = c(0, 100), y = c(0, 100), z = c(0,5), 
            dx = 1, dy = 1, dz = 0.01)
FAC <- pixelise(mod, mbox)

A <- setProp(FAC$XYZ, type = c("facies"))


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
