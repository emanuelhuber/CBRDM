
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