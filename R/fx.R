################################################################################
########################### HELPER FUNCTIONS ###################################
################################################################################

# - initObject
# - mScour2mTrEll


#'@export
plotCDF <- function(x, add = FALSE, type = 's', xaxs = "i", yaxs = "i", 
                    ylab = "eCDF", ylim = c(0, 1), ...){
  if(add){
    lines(sort(x), seq_along(x)/length(x), type = type, ...)
  }else{
    plot(sort(x), seq_along(x)/length(x), type = type, 
     ylim = ylim, xaxs = xaxs, yaxs = yaxs, ylab = ylab, ...)
  }
}

#'@export
plotMultiHist <- function(x, sel, breaks = breaks, xlab = "", 
                          ylab = "counts", ...){
X <- hist(x, plot = FALSE, breaks = breaks)
ymax <- max(X$counts)
plot(0, type = "n", xlim = range(x), ylim = c(0,ymax), xlab = xlab,
  ylab = ylab)
linesHist(x, breaks = breaks, col = "black", lwd = 3)
linesHist(x[!sel], breaks = breaks, col = "red", lwd = 3)
linesHist(x[sel], breaks = breaks, col = "blue", lwd = 3)
}


linesHist <- function(x, breaks = 30, ...){
  X <- hist(x, plot = FALSE, breaks = breaks)
  xl <- rep(X$breaks, each = 2)
  yl <- c(0, rep(X$counts, each = 2), 0)
  lines(xl, yl, ...)
}

#'@export
plotMultiDensity <- function(x, sel, xlab = "", ylab = "density"){
X1 <- density(x, bw = "SJ", adjust = 1, from = min(x),
             to = max(x))
X2 <- density(x[!sel], bw = "SJ", adjust = 1, from = min(x),
             to = max(x))
X3 <- density(x[sel], bw = "SJ", adjust = 1, from = min(x),
             to = max(x))
n <- length(x)
plot(X1$x, X1$y, type = "l", ylim = c(0, max(X1$y)), xlab = xlab,
  ylab = ylab, lwd = 2)
lines(X2$x, X2$y*sum(!sel)/n, col = "red", lwd = 3)
lines(X3$x, X3$y*sum(sel)/n, col = "blue", lwd = 3)
}

#'@export
plotSectionMat <- function(x, add = FALSE, xlab = "x",
            ylab = "y", main = "", asp = NA, xaxs = "i", yaxs = "i", ...){
 if(add == FALSE){
  #b <- boundary(x)
  dots <- list(...)
  if(!is.null(dots$xlim)){
    xlim <- dots$xlim
    dots$xlim <- NULL
  }else{
    stop("sdfoiuwrsdf")
  }
  if(!is.null(dots$ylim)){
    ylim <- dots$ylim
    dots$ylim <- NULL
  }else{
    stop("sdfoiuwrsdfsdfsf")
  }
  plot(0, type = "n", xlab = xlab, ylab = ylab, main = main,
        xlim = xlim, ylim = ylim,
        asp = asp, xaxs = xaxs, yaxs = yaxs)
  }
  # plot troughs
  invisible(apply(x, 1, .plotTrEllipse2, ...))
  #plotSection(x@troughs, add = TRUE, ...)
}
    


.plotTrEllipse2 <- function(e, ...){
  polygon(.trEllipse(saxes = e[c("a", "b")], 
                     loc   = e[c("x", "z")],
                     theta = 0,
                     zmax  = e["zmax"],
                     alpha = c(0.5, 1)), ...)
}  
  
  

#'@export
equidistCurve <- function(x, y, d, withTail = TRUE, method = "linear", 
                          extrap = TRUE, ...){
  dL <- posLine(cbind(x, y))
  dLi <- seq(0, tail(dL, 1), by = d)
  if(withTail == TRUE && tail(dLi,1) != tail(dL,1)) dLi <- c(dLi, tail(dL,1))
  XY <- matrix(nrow = length(dLi), ncol = 2)
  XY[,1] <- signal::interp1(dL, x, xi = dLi, method = method, extrap = extrap, 
                            ...) 
  XY[,2] <- signal::interp1(dL, y, xi = dLi, method = method, extrap = extrap, 
                            ...) 
  return(XY)
}



####----------------------- MCMC ---------------------------------####

###----------- initialisation

#'@export
initMod <- function(SCOURS, l, para, Hmin, Hmax, rLHmax, 
                      eroBox, tol, tol2, n, ah){
  z0 <- sapply(SCOURS, function(x) max(x[,2]))
  zorder <- order(z0)
  z <- z0[zorder]
  names(z) <- seq_along(z)
  lays0 <- lapply(seq_along(SCOURS), initObject, SCOURS, l, para)
  lays <- lays0[zorder]
  return(new("Deposits",  
            version = "character",   # version of the class
            id = 1L,
            #z = z,
            layers = lays,
            bbox = modbox
            ))
}

initObject <- function(i, SCOURS, l, para){
  A <- SCOURS[[i]]
  l0 <- c(-l[3]/l[1],0)
  #L <- runif(1,min=para$L$min,max=para$L$max)
  xyz <- matrix(nrow=1, ncol = 3)
  xyz[1] <- sqrt( (A[which.min(A[,2]),1])^2/( 1 + 
                      (-l[1]/l[2])^2)) + l0[1]
  xyz[2] <- sqrt( ((-l[1]/l[2])^2 * (A[which.min(A[,2]),1])^2)/( 1 + 
                      (-l[1]/l[2])^2))  + l0[2]
  xyz[3] <- max(A[,2])
  
  W <- max(diff(range(A[,1])), 2*(A[which.min(A[,2]),1] - 
            A[which.max(A[,2]),1])) * 0.8
  H <- abs(diff(range(A[,2])))
  L <- ifelse(W * para$rLW$max < para$L$max & 
              W * para$rLW$max > para$L$min, 
              W * para$rLW$max, W * para$rLW$min)
  L[L < para$L$min] <- para$L$min
  L[L > para$L$max] <- para$L$max
  rLW <- L/W
  rLW[rLW > para$rLW$max] <- para$rLW$max
  rLW[rLW < para$rLW$min] <- para$rLW$min
  rLH <- L/H
  rLH[rLH > para$rLH$max] <- para$rLH$max
  rLH[rLH < para$rLH$min] <- para$rLH$min
  
  
  return(list("id" = i,
              "z"  = xyz[3],
              "obj" = new("Trough",
                          version = "0.1",
                          id      = as.integer(i),
                          pos     = xyz,
                          L       = unname(L),
                          W       = L/rLW, #L/mean(para$rLW$min, para$rLW$max),
                          H       = L/rLH, #L/mean(para$rLH$min, para$rLH$max),
                          #theta   = .rsim(para$theta, 1L),  # depth position
                          theta   = 0,  # depth position
                          rH      = para$rH
                        )))
}

#'@export
initSim <- function(X0, Y, l, para, pDMAX, probUpdates, min_it, max_it, 
                    Hmin, Hmax, rLHmax, eroBox,  tol, tol2, n, ah){
  if(max_it <= min_it) max_it <- min_it + 1L
  uptp <- integer(max_it)
  acc <- integer(max_it)
  pdv <- numeric(max_it)
  X <- X0
  sX <- section(X, l, lim = list(x = vSec$llim, z = modbox$z) )
  sXf <- filterSection(x = sX, Hmin = Hmin, Lmin = Lmin, 
                          rLHmax = rLHmax, eroBox = eroBox, 
                          tol = tol, tol2 = tol2, n = n)
  pD <- pseudoDist(x = sXf, y = Y, ah)
  types <- paste0(seq_along(probUpdates))
  i <- 0L
  XL <- list()
  sXL <- list()
  ESL <- list()
  while(i < min_it || (pD > pDMAX && i < max_it)){
    i <- i + 1L
    if((i %% round(max_it/10)) == 0) message(" * ",  appendLF = FALSE)
    type <-   sample(types, 1, prob = probUpdates)
    uptp[i] <- as.integer(type)
    Xstar <- switch(type,
          "1" = {updateLay(X, type = "n",    para = para)},
          "2" = {updateLay(X, type = "pos",  para = para)},
          "3" = {updateObj(X, type = "pos",  para = para)},
          "4" = {updateObj(X, type = "n",    para = para)},
          "5" = {updateObj(X, type = "prop", para = para)})
    sXstar <- section(Xstar, l)
    sXfstar <- filterSection(x = sXstar, Hmin = Hmin, Lmin = Lmin, 
                        rLHmax = rLHmax, eroBox = eroBox, 
                        tol = tol, tol2 = tol2, n = n)
    pDstar <- pseudoDist(x = sXfstar, y = Y, ah)
    if(!is.infinite(pDstar) && (pDstar <= pD || pDstar < pDMAX)){
      acc[i] <- 1L
      pD <- pDstar
      sX <- sXstar
      sXf <- sXfstar
      X <- Xstar
    }
    pdv[i] <- pD
    XL[[i]] <- X
    sXL[[i]] <- sX
    ESL[[i]] <- sXf$e
  }
  uptp <- uptp[seq_len(i)]
  acc <- acc[seq_len(i)]
  pdv <- pdv[seq_len(i)]
  return(list(X = X, sX = sX, sXf = sXf, uptp = uptp, acc = acc, pdv = pdv,
              XL = XL, sXL = sXL, ESL = ESL))
}
    

#'@export   
simMC <- function(X0, Y, l, para, pDMAX, probUpdates, nit, it_spl, 
                    Hmin, Hmax, rLHmax, eroBox,  tol, tol2, n, ah){
  #if(max_it <= min_it) max_it <- min_it + 1L
  if(nit/it_spl < 2) stop("lkjljlI")
  nit <- round(nit/it_spl)*it_spl
  uptp <- integer(nit)
  acc <- integer(nit)
  pdv <- numeric(nit)
  nch <- numeric(nit)
  X <- X0
  sX <- section(X, l, lim = list(x = vSec$llim, z = modbox$z))
  sXf <- filterSection(x = sX, Hmin = Hmin, Lmin = Lmin, 
                          rLHmax = rLHmax, eroBox = eroBox, 
                          tol = tol, tol2 = tol2, n = n)
  pD <- pseudoDist(x = sXf, y = Y, ah)
  types <- paste0(seq_along(probUpdates))
  i <- 0L
  ID <- matrix(ncol = length(Y), nrow = nit/it_spl )
  XL <- list()
  sXL <- list()
  ESL <- list()
  it <- 0L
  while(i < nit){
    i <- i + 1L
    if((i %% round(nit/10)) == 0) message(" * ", appendLF = FALSE)
    type <-   sample(types, 1, prob = probUpdates)
    uptp[i] <- as.integer(type)
    Xstar <- switch(type,
          "1" = {updateLay(X, type = "n",    para = para)},
          "2" = {updateLay(X, type = "pos",  para = para)},
          "3" = {updateObj(X, type = "pos",  para = para)},
          "4" = {updateObj(X, type = "n",    para = para)},
          "5" = {updateObj(X, type = "prop", para = para)})
    
    ##### give a limit to the section!!!!!
    sXstar <- section(Xstar, l , lim = list(x = vSec$llim, z = modbox$z) )
    if(all(type != c("2", "3", "5")) && identical(sXstar, sX)){
      acc[i] <- 1L
      nch[i] <- 1L
    }else{
      sXfstar <- filterSection(x = sXstar, Hmin = Hmin, Lmin = Lmin, 
                              rLHmax = rLHmax, eroBox = eroBox, 
                              tol = tol, tol2 = tol2, n = n)
      pDstar <- pseudoDist(x = sXfstar, y = Y, ah)
      if(!is.infinite(pDstar) && (pDstar <= pDMAX)){
        acc[i] <- 1L
        pD  <- pDstar
        X   <- Xstar
        sX  <- sXstar
        sXf <- sXfstar
      }
    }
    pdv[i] <- pD
    if((i %% it_spl) == 0){
      it <- it + 1L
      ID[it,] <- as.integer(sXf$E[,"id"])
      XL[[it]] <- X
      sXL[[it]] <- sX
      ESL[[it]] <- sXf$e
    }
  }
  return(list(uptp = uptp, acc = acc, pdv = pdv,
              XL = XL, sXL = sXL, ESL = ESL, ID = ID))
}    
  

###----------- filter section
#'@export
filterSection <- function(x, Hmin = NULL, Lmin = NULL, rLHmax = 25, 
                         eroBox, tol = 0.5, tol2 = 0.1, n = 200){
  if(is.null(x) || length(x)==0 ) return(NULL)
  A <- rmSmallObject(x, Hmin, Lmin)
  if( length(A) == 0 ){
    return(NULL)
  }else if(nrow(A) == 1){
    return(A)
  }else{
    y <- scours2Poly(A, n, eroBox, tol, tol2)
    nPoly <- nrow(y$E)
    pdiff <- list()
    if(nPoly == 0){
      return(NULL)
    }else if(nPoly==1){
      pdiff[[1]] <- y$p
      #return(y$E)
    }else if(nPoly==2){
      if(rgeos::gCovers(y$p[2],y$p[1]) || 
            rgeos::gArea(rgeos::gDifference(y$p[1],y$p[2])) < tol2){
        pdiff[[1]] <- y$p[2]
        y$E <- y$E[2,, drop = FALSE]
        #return( y$E[2,,drop=FALSE])
      }else{
        pdiff[[1]] <- rgeos::gDifference(y$p[1], y$p[2])
        pdiff[[2]] <- y$p[2]
      }
    }else{
      test <- rep(FALSE, nPoly)
      for(i in 1:(nPoly-1)){
        p_younger <- pUnion(y$p[(i+1):(nPoly)])
        # true if y$p completely covered by p_younger
        test[i] <- rgeos::gCovers(p_younger, y$p[i])  
        if(test[i] == FALSE){ 
          p_diff <- try(rgeos::gDifference(y$p[i], p_younger), silent = TRUE)
          if (class(p_diff) == "try-error") {
              p_younger <- rgeos::gBuffer(p_younger, byid = TRUE, width = 0.01)
              p_diff <- rgeos::gDifference(y$p[i], p_younger)
          }
          if(rgeos::gArea(p_diff) < tol2){
            test[i] <- TRUE # true is the rest of y$p are very small..
          }else{
            pdiff[[i]] <- p_diff
          }   
          #test[i] <- TRUE # true is the rest of y$p are very small..
        }
      }
      pdiff[[nPoly]] <- y$p[nPoly]
      pdiff <- pdiff[!test]
      #test[nPoly-1] <- rgeos::gCovers(y$p[nPoly],y$p[nPoly-1])     
      y$E <- y$E[!test, , drop = FALSE]
    }
  }
  #stop("lkjl")
  if(length(y$E) == 0 ) return(NULL)
  ES <- erosSurf(p = pdiff, obj = y$E, tol = 10^-7)
  return(list(E = y$E, p = pdiff, e = ES))
} 

#'@export
polygonizeSection <- function(x, Hmin = NULL, Lmin = NULL, rLHmax = 25, 
                         eroBox, tol = 0.5, tol2 = 0.1, n = 200){
  if(is.null(x) || length(x)==0 ) return(NULL)
  A <- rmSmallObject(x, Hmin, Lmin)
  if( length(A) == 0 ){
    return(NULL)
  }else{
    y <- scours2Poly(A, n, eroBox, tol, tol2)
    nPoly <- nrow(y$E)
    pdiff <- list()
    test <- rep(FALSE, nPoly)
    if(nPoly==0){
      return(NULL)
    }else if(nPoly==1){
      pdiff[[1]] <- y$p
    }else if(nPoly==2){
      if(rgeos::gCovers(y$p[2],y$p[1]) || 
            rgeos::gArea(rgeos::gDifference(y$p[1],y$p[2])) < tol2){
        pdiff[[1]] <- y$p[2]
        y$E <- y$E[2,, drop = FALSE]
        y$A <- y$A[2,, drop = FALSE]
        y$p <- y$p[-1]
      }else{
        pdiff[[1]] <- rgeos::gDifference(y$p[1], y$p[2])
        pdiff[[2]] <- y$p[2]
      }
    }else{
      for(i in 1:(nPoly - 1)){
        if(i == (nPoly - 1)){
          p_younger <- y$p[nPoly]
        }else{
          p_younger <- pUnion(y$p[(i+1):(nPoly)])
        }
        # true if y$p completely covered by p_younger
        test[i] <- rgeos::gCovers(p_younger, y$p[i])  
        if(test[i] == FALSE){
          p_diff <- try(rgeos::gDifference(y$p[i], p_younger), silent = TRUE)
          if (class(p_diff) == "try-error") {
              p_younger <- rgeos::gBuffer(p_younger, byid = TRUE, width = 0.01)
              p_diff <- rgeos::gDifference(y$p[i], p_younger)
          }
#           y$p@polygons[[i]] <- p_diff@polygons[[1]]
          #if(length(p_diff) > 1) stop("length(p_diff) > 1")
          if(rgeos::gArea(p_diff) < tol2){
            test[i] <- TRUE # true is the rest of y$p are very small..
          }else{
            pdiff[[i]] <- p_diff
          } 
        }
      }
      pdiff[[nPoly]] <- y$p[nPoly]
      y$E <- y$E[!test, , drop = FALSE]
      y$A <- y$A[!test, , drop = FALSE]
      y$p <- y$p[which(!test)]
      pdiff <- pdiff[!test]
    }
  }
  #stop("lkjl")
  if(length(y$E) == 0 ) return(NULL)
  return(list(E = y$E, A = y$A, p = y$p, pdiff = pdiff))
} 

      
rmSmallObject <- function(x, Hmin, Lmin){
  A <- as.matrix(x)
  # remove objects that are small: 
  # (1) almost horizontal (very small depth); 
  # (2) small surface (depth $\times$ width)
  if(!is.null(Hmin)){
    A <- A[A[,"H"] >= Hmin, , drop = FALSE]
  }
  if(!is.null(Lmin)){
    A <- A[A[,"L"] >= Lmin, , drop = FALSE]
  }
  return(A)
}
      
scours2Poly <- function(A, n, eroBox, tol, tol2){
  E <- mScour2mTrEll(A)
  ocoord <- ptsTrEllipse(E, n = n)
  # ocoord <- getObjectPts(B, n = n)
  p2 <- list2SpatialPolygons(ocoord)
  testP <- as.vector(rgeos::gIntersects(eroBox, p2, byid = TRUE))
  p2 <- p2[which(testP)]
  E <-  E[testP,, drop = FALSE]
  A <-  A[testP,, drop = FALSE]
  test <- rep(TRUE, length(p2))
  for(i in seq_along(p2)){
    pint <- rgeos::gIntersection(p2[i], eroBox, id = as.character(i))
    p2@polygons[[i]] <- pint@polygons[[1]]
    pArea <- rgeos::gArea(pint)
    if(pArea < tol2 || pArea/ rgeos::gLength(pint) < tol){
      test[i] <- FALSE
    }
    rgeos::gLength(pint)
  }
  return(list(p = p2[which(test)], E = E[test,, drop = FALSE], 
              A = A[test,, drop = FALSE]))
}
      
# A = as.matrix(x) with x from class Deposits2D
# return matrix from TrEllipse
mScour2mTrEll <- function(A){
  B <- A
  colnames(B)[c(4,5, 6)] <- c("a", "b", "zmax")
  bbb <-  A[,"rH"] *  A[,"H"]
  B[,"z"] <- A[,"z"] - A[,"H"] + bbb
  B[,"a"] <- A[,"L"] / (2* sqrt(1 - (A[,"z"] - B[,"z"])^2/bbb^2))
  B[,"b"] <- bbb
  B[,"zmax"] <- A[,"z"]
  return(B)
}

ptsTrEllipse <- function(x, n = 30){
  lapply(1:nrow(x), .ptsTrEllipse, x, n=n)
}

.ptsTrEllipse <- function(i, x, n = 30){
  x <- x[i,]
  .trEllipse(saxes = x[c("a", "b")], 
             loc   = x[c("x", "z")],
             theta = 0,
             zmax  = x["zmax"],
             alpha = c(0.5, 1),
             n = n)
}

list2SpatialPolygons <- function(x){
  pp <- lapply(x,sp::Polygon)
  ppp <- lapply(seq_along(pp), listPolygon2Polygons, pp)
  sp::SpatialPolygons(ppp)
}
      
listPolygon2Polygons <- function(i,pp){
  sp::Polygons(pp[i],i)
}  
      
#'@export
pUnion <- function(p){
  p_u <- try(rgeos::gUnaryUnion(p), silent = TRUE)
  if (class(p_u) == "try-error") {
    y$p <- rgeos::gBuffer(y$p, byid = TRUE, width = 0)
    p_u <- rgeos::gUnaryUnion(p)
  }
  return(p_u)
}

#areaSpatialPolygons <- function(x){
#  sapply(slot(x, "polygons"), function(x) sapply(slot(x, "Polygons"), 
#          slot, "area"))  
#}

#-- erosion surfaces         
         
# return bounding surfaces (list of coordinates)
#'@export
erosSurf <- function(p, obj, tol = 10^-2){
  L <- lapply(seq_along(p), .eSurf, p, obj, tol)
  return(unlist(L, recursive = FALSE))
}


.eSurf <- function(i, p, obj, tol){
   if(typeof(p) == "list"){
    pp <- p[[i]]
  }else{
    pp <- p[i]
  }
  #lengthLBES(p = pp, obj = obj[i,])
  return(.eSurfi(pp, obj[i,], tol = tol))
}
.eSurfi <- function(pi, obji, tol = 10^-2){
  p0 <- pi@polygons[[1]]@Polygons
  pdist <- 0
  BS <- list()
  it <- 0
  for(i in seq_along(p0)){
    it <- it + 1
    xy <- p0[[i]]@coords
    test <- abs( ((xy[,1] - obji["x"])/obji["a"])^2 +
          ((xy[,2] - obji["z"])/obji["b"])^2 - 1 ) < tol
    if(sum(test) > 1){
      xy <- xy[test,, drop = FALSE]
      xy <- xy[order(xy[,1]),, drop = FALSE]
      BS[[it]] <- xy
      ##lines(xy, pch = 20, col ="red", lwd = 2)
    }
  }
  #return(pdist)
  #if(length(BS) == 1) return(BS[[1]])
  return(BS)
}



#------- distance
# x = list of coordinates
# y = eroSurf, SpatialLines
#'@export
#pseudoDist <- function(x, y, ah, ...){
#  if(is.null(x) || length(x) == 0) return(Inf)
#  if(length(x$e) != length(y)) return(Inf)
#  os <- reorderSection(x$E)
#  xE <- x$E[os, ]
#  if( sum(abs((ageHierarchy(xE) - ah)*ah) )> 0 ){
#    return(Inf)
#  }
#  y <- y[os]
#  xsp <- ListXYToListSpatialLines(x$e)
#  res <- sapply(seq_along(y), dinf, xsp, y)
#  return(max(res))
  #xsp <-listXY2SpatialLines(x$e)  
  #return(rgeos::gDistance(xsp, y, hausdorff = TRUE, densifyFrac = 0.1))
#}
#'@export
dinf <- function(i, x, y, ...){
  rgeos::gDistance(x[[i]], y[[i]], hausdorff = TRUE, densifyFrac = 0.1)
}
    
## x = matrix of ellipse parameters
## y = SCOURS, list of matrix (coordinates n x 3 matrix)    
#'@export
pseudoDist <- function(x, y, ah, ...){
  if(is.null(x) || length(x) == 0) return(Inf)
  if(length(x$e) != nrow(x$E)) return(Inf)
  if(length(x$e) != length(y)) return(Inf)
  os <- reorderSection(x$E)
  xE <- x$E[os, ]
  if( sum(abs((ageHierarchy(xE) - ah)*ah) )> 0 ){
    return(Inf)
  }
  res <- getResiduals(y, xE)
  xmax <- sapply(x$e, function(x) max(x[,2]))
  ymax <- sapply(y, function(x) max(x[,2]))
  #penalty <- (xmax - ymax)^2
  DZ <- (xmax - ymax)
  resall <- res #+ (DZ < 0)*DZ^2
  #return(sqrt(sum(res^2)/length(res)))
  return(max(resall))
}
#'@export 
reorderSection <- function(x){
  # if(is.null(x) || length(x)==0) return(NULL)
  # check: 1) same number of elt; 2) age hierarchy
  # if(length(SCOURS) != nrow(x)){
  #   return(NULL)
  # }
  order34 <- order2values(x[c(3,4),"x"])
  order56 <- order2values(x[c(5,6),"x"])
  return(c(1, 2, order34 + 2, order56 + 4, 7))
  # x <- x[c(1, 2, order34 + 2, order56 + 4, 7), ]
  # return(x)
}

order2values <- function(a){
        ifelse(rep(a[1]> a[2],2) ,c(2,1),c(1,2))
}
  
#'@export
ageHierarchy <- function(A){
  H  <- matrix(0L, nrow = nrow(A), ncol = nrow(A))
  nzz <- length(A[,"zmax"])
  for(i in seq_len(nzz-1)){
          H[i, (i+1L):nzz][A[i,"zmax"] < A[(i+1L):nzz, "zmax"]] <- 1L
  }
  return(H)
} 
#'@export
getResiduals <- function(y,x){
        return(sapply(seq_along(y), .getResiduals, y, x))
}
  
.getResiduals <- function(i, y, x){
  CC <- ellipseToConicMatrix(saxes = x[i,c("a","b")], loc = x[i,c("x","z")], 
                             theta = 0)
  XY0 <- matrix(1, nrow = nrow(y[[i]]), ncol = 3)
  XY0[,1:2] <- y[[i]]
  res2 <- diag((XY0) %*% CC %*% t(XY0))^2
  # root-mean-square deviation
  return(sqrt(sum(res2)/length(res2)))
  #return(max(res2))
}

.listLines <- function(i, x){
  Lines(x[[i]], ID = as.character(i))
}
  
#'@export
listXY2SpatialLines <- function(x){
  x <- lapply(x, Line)
  return(SpatialLines(lapply(seq_along(x), .listLines, x)))
}
#'@export 
ListXYToListSpatialLines <- function(x){
  lapply(lapply(lapply(x, Line), Lines, ID = "1"), .SpatialLines)
}    
.SpatialLines <- function(x){
  SpatialLines(list(x))
}    

# .getResiduals <- function(i, A, x){
#   # u0 <- (A[[i]][,c(1,2)]) - x[rep(i,nrow(A[[i]])),c("xap","z"),drop=FALSE]
#   # tu <- sqrt(apply((u0/x[rep(i,nrow(A[[i]])),c("aap","bap")])^2,1,sum))
#   # return(mean(sqrt(apply((u0 - u0/tu )^2,1,sum))))
#   #u0 <- sweep(A[[i]][,c(1,2)], 2, x[i,c("x","z")], '-')
#   #tu <- sqrt(rowSums(sweep(u0,2,x[i,c("a","b")],'/')^2))
#   #return(mean(sqrt(rowSums((u0 - u0/tu )^2))))
#   u0 <- sweep(A[[i]][,c(1,2)], 2, x[i,c("x","z")], '-')
#   tu <- sqrt(rowSums(sweep(u0, 2, x[i,c("a","b")],'/')^2))
#   res2 <- rowSums((u0 - u0 / tu )^2)
#   # root-mean-square deviation
#   return(sqrt(sum(res2)/length(res2)))
# }
# getResiduals <- function(SCOURS,x){
#         return(sapply(seq_along(SCOURS), .getResiduals, SCOURS, x))
# }

# pseudoDist <- function(x, SCOURS, zmax, zmin, ah){
#   if(is.null(x)) return(Inf)
#   if( sum(abs((ageHierarchy(x) - ah)*ah) )> 0 ){
#     return(Inf)
#   }
#   zmaxPenalty <- rep(0, length(SCOURS))
#   zminPenalty <- zmaxPenalty
#   zmaxPenalty[x[,"zmax"] > zmax] <- (zmax - x[,"zmax"])[x[,"zmax"] > zmax]
#   zminPenalty[x[,"zmax"] < zmin] <- (zmin - x[,"zmax"])[x[,"zmax"] < zmin]
#   res <- getResiduals(SCOURS, x)
#   return(sqrt(sum( (res * (1 + zminPenalty)^2 * (1 + zmaxPenalty)^2)^2) / 
#           length(res)))
# }
  

#     | a1   0  | 
#e1 = |         |
#     |  0   b1 |
# geoDist <- function(ab1, ab2){
#   L_A <- 1/sqrt(ab1)
#   L_B <- 1/sqrt(ab2)
#   D <- L_A[,1] * L_A[,2]
#   dgeo <- sqrt(log((L_A[,2] * L_B[,1]) / D)^2 + 
#                log((L_A[,1] * L_B[,2]) / D)^2 )
#   return(dgeo)
# 
# }
  
# # need to check if the z-penalty are not in conflict with REF
# pseudoDist2 <- function(x, REF, zmax, zmin, ah){
#   if(is.null(x)) return(Inf)
#   if( sum(abs((ageHierarchy(x) - ah)*ah) )> 0 ){
#     return(Inf)
#   }
#   zmaxPenalty <- rep(0, nrow(REF))
#   zminPenalty <- zmaxPenalty
#   zmaxPenalty[x[,"zmax"] > zmax] <- (zmax - x[,"zmax"])[x[,"zmax"] > zmax]
#   zminPenalty[x[,"zmax"] < zmin] <- (zmin - x[,"zmax"])[x[,"zmax"] < zmin]
#   res <- geoDist(x[,c("a","b")], REF[,c("a","b")])
#   res2 <- sqrt(rowSums((x[, c("x", "zmax")] - REF[, c("x", "zmax")])^2))
#   R1 <- sqrt(sum( (res)^2) / 
#           length(res))
#   R2 <- sqrt(sum((res2 * (1 + zminPenalty^2) * (1 + zmaxPenalty^2))^2) / 
#               length(res2))
#   return(R1*10 + R2 )
# }
    
 


####----------------------- MC-EXPLORATION -----------------------------####
#'@export
setPara <- function(hyperP, rnd){
  para <- list()
  pp <- rfacP(hyperP$L, rnd[1:2])
  para$L <- list(type = "runif", min = pp[1], max = pp[2])
  pp <- rfacP(hyperP$rLW, rnd[3:4])
  para$rLW <- list(type = "runif", min = pp[1], max = pp[2])
  pp <- rfacP(hyperP$rLH, rnd[5:6])
  para$rLH <- list(type = "runif", min = pp[1], max = pp[2])
  pp <- rfacP(hyperP$theta, rnd[7])
  para$theta <- list(type = "runif", min = -pp[1], max = pp[1])
  para$rH <- 2
  para$vpp  <-  list(type = "poisson", 
                     lambda = rfac(hyperP$lambda, rnd[9]))
  para$hpp <- list(type = "strauss", 
                             bet = rfac(hyperP$bet, rnd[10]), 
                             gam = rfac(hyperP$gam, rnd[11]), 
                             d = rfac(hyperP$d, rnd[12]), 
                             fd = c(rfac(hyperP$fd, rnd[13]), 1), 
                             nit = 5000, n0 = 3)
  return(para)
}

rfacP <- function(hp, r){
  pmin <- rfac(hp, r[1])
  pmax <- rfac(hp, r[2])
  return(sort(c(pmin, pmax)))
}
  
rfac <- function(x, r){
  return( r * (x$max - x$min) + x$min)
}

         
         

#'@export
getAtt <- function(x, Hmin, Lmin, rLHmax, 
                   eroBox, tol, tol2, 
                   n){
  y <- polygonizeSection(x = x, Hmin = Hmin, Lmin = Lmin, rLHmax = rLHmax, 
                      eroBox = eroBox, tol = tol, tol2 = tol2, 
                      n = n)
  if(is.null(y)) return(NULL)
  
  AT <- rep(NA, 52)

  ES <- erosSurf(p = y$pdiff, obj = y$E, tol = 10^-7)
  ES <- .compactList(ES)
  #--- number of objt
  AT[52] <- nrow(y$E) 
  AT[1] <- length(ES) 
  #--- total area
  AT[2] <- rgeos::gArea(pUnion(y$p)) 
  if(AT[1] > 1){
    #--- eroded areas
    eA <- erodedAreas(p = y$p, pd = y$pdiff)
    tst <- eA < 1
    AT[3] <- sum(tst)/AT[1] # proportion scours not eroded
    if(sum(!tst) > 2){
      AT[4:9] <- summary(eA[!tst]) # stat scours eroded
    }else{
      AT[4:9] <- 0
    }
    AT[10] <- sum(eA) # sum(area scours eroded)
    #--- erosion length
    ES0 <- erosSurf(p = y$p, obj = y$E, tol = 10^-7)
    ES0 <- .compactList(ES0)
    eLl <- lapply(ES, posLine, last = TRUE)
    eLl <- .compactList(eLl)
    eL <- sum(unlist(eLl), na.rm = TRUE)
    eL0l <- lapply(ES0, posLine, last = TRUE)
    eL0l <- .compactList(eL0l)
    eL0 <- sum(unlist(eL0l), na.rm = TRUE)
    #eL0 <- sum(sapply(ES0, posLine, last = TRUE))
    deL <- eL0 - eL
    tst <- (deL > 0.5)
    AT[11] <- sum(tst)/AT[1] # proportion eros. surf. eroded
    if(sum(tst) > 2){
      AT[12:17] <- summary(deL[tst]) # stat eros. surf. eroded
    }else{
      AT[12:17] <- 0
    }
    AT[18] <- sum(deL[tst]) # sum(eros. surf. eroded)
    AT[19:24] <- summary(eL) # stat eros. surf. remnant
    AT[25] <- sum(eL) # sum(eros. surf. remnant)
    #--- size
    L <- y$A[,"L"]
    H <- y$A[,"H"]
    rLH <- L/H
    AT[26:31] <- summary(L) # stat length
    AT[32:37] <- summary(H) # stat height
    AT[38:43] <- summary(rLH) # stat length-height-ratio
    #--- z-elevation
    z <- y$A[,"z"]
    ztbl <- unname(table(z))
    AT[44] <- length(ztbl)  # number of elevation
    AT[45] <- mean(ztbl)  # mean number object per elevation
    AT[46:51] <- summary(diff(z)) # stat aggradation rate
  }else{
    #eL <- sum(sapply(ES, posLine, last = TRUE))
    AT[3] <- 1
    AT[4:51] <- 0
    AT[25] <- sum(sapply(ES, posLine, last = TRUE))
  }
  return(AT)
}

         
erodedAreas <- function(p, pd){
  return(sapply(seq_along(p), .erodedArea, p, pd))
}

.erodedArea <- function(i, p, pd){
  rgeos::gArea(p[i]) - rgeos::gArea(pd[[i]])
}

#'@export
posLine <- function(loc,last=FALSE){
  loc <- as.matrix(loc)
  all_dist <- cumsum(c(0,sqrt(apply(diff(loc)^2,1,sum))))
  if(last){
        return(tail(all_dist,1))
  }else{
        return(as.numeric(all_dist))
  }
}


  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
