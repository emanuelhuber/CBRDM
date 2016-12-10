# Check p. 60-61


## TODO
# - section: Cuboid
# - plotObj: Trough2D
# - extract()
# - length()
# - getParameters()


# don't return NULL but empty object (length 0)


# position of a point on ellipse as function of its angle with
# center ellipse for an ellipse centered in (0, 0) and axis-aligned
# x1 <- obj@L * obj@W / sqrt(obj@W^2 + obj@L^2 * tan(obj@theta)^2)
# y1 <- obj@W * sqrt( 1 - (x1 / obj@L)^2 )


#--- physical properties facies
# Huggenberger (1993)
# Jussel et al (1994)
# Huber and Huggenberger (2016) doi:10.5194/hess-20-2035-2016
# p = porosity
# de = dielectric number saturated zone
#
depprop <- list(gp = c(p  = 0.201,
                       K = 1.5e-3,
                       sdlogK = 0.5,
                       vaniK = 6,
                       de = 12.1),
                bm = c(p  = 0.25,
                       K = 1.5e-3,
                       sdlogK = 0.1,
                       vaniK = 1,
                       de = 9.2),
                ow = c(p  = 0.35,
                       K = 1e-1,
                       sdlogK = 0.1,
                       vaniK = 1,
                       de = 26.9))
                       
meanlog <- function(xmean, xsdlog){
  return( log(xmean) - 0.5*xsdlog^2 )
}
rlognorm <- function(n, mean, sdlog){
  rlnorm(n, meanlog = meanlog(mean, sdlog), sdlog = sdlog)
}
# rlnorm(n, meanlog = meanlog_tf, sdlog = sdlog_tf)
                       
                       
##------------------- CLASSES ------------------##

#' An S4 class to represent trough structure.
#'
#' @slot version A length-n character vector indicating the version of RGPR
#' @slot id A length-n numeric vector
#' @slot pos A nx3 numeric matrix corresponding to object center position.
setClass(
  Class="Trough",  
  slots=c(
    version = "character",   # version of the class
    id = "numeric",
    pos = "matrix",     # position, z = top of object
    L = "numeric",
    W = "numeric",
    H = "numeric",
    theta = "numeric",  # depth position
    rH = "numeric",
    fill = "list"
  )
)

#' An S4 class to represent Deposits.
#'
#' @slot version A length-n character vector indicating the version of RGPR
#' @slot id A length-n numeric vector
#' @slot pos A nx3 numeric matrix corresponding to object center position.
setClass(
  Class="Deposits",  
  slots=c(
    version = "character",   # version of the class
    troughs = "Trough",
    layers = "numeric",
    bbox = "list"
  )
)

#' An S4 class to represent Spoon
#'
#' @slot version A length-n character vector indicating the version of RGPR
#' @slot id A length-n numeric vector
#' @slot pos A nx3 numeric matrix corresponding to object center position.
setClass(
  Class="Spoon",  
  slots=c(
    version = "character",   # version of the class
    id = "numeric",
    pos = "matrix",     # position, z = top of object
    L = "numeric",
    W = "numeric",
    H = "numeric",
    theta = "numeric",  # depth position
    rH = "numeric",
    rL = "numeric",
    fill = "list"
  )
)

#' An S4 class to represent TrEllipsoid
#'
#' @slot version A length-n character vector indicating the version of RGPR
#' @slot id A length-n numeric vector
#' @slot pos A nx3 numeric matrix corresponding to object center position.
setClass(
  Class="TrEllipsoid",  
  slots=c(
    version = "character",   # version of the class
    id = "numeric",
    pos = "matrix",     # position, z = middle of object
    a = "numeric",
    b = "numeric",
    c = "numeric",
    theta = "numeric",  # depth position
    zmax = "numeric"
  )
)

#' An S4 class to represent Ellipsoid
#'
#' @slot version A length-n character vector indicating the version of RGPR
#' @slot id A length-n numeric vector
#' @slot pos A nx3 numeric matrix corresponding to object center position.
setClass(
  Class="Ellipsoid",  
  slots=c(
    version = "character",   # version of the class
    id = "numeric",
    pos = "matrix",     # position, z = middle of object
    a = "numeric",
    b = "numeric",
    c = "numeric",
    theta = "numeric"  # depth position
  )
)

#' An S4 class to represent Sphere
#'
#' @slot version A length-n character vector indicating the version of RGPR
#' @slot id A length-n numeric vector
#' @slot pos A nx3 numeric matrix corresponding to object center position.
setClass(
  Class="Sphere",  
  slots=c(
    version = "character",   # version of the class
    id = "numeric",
    pos = "matrix",     # position, z = middle of object
    r = "numeric"
  )
)

#' An S4 class to represent Cuboid
#'
#' @slot version A length-n character vector indicating the version of RGPR
#' @slot id A length-n numeric vector
#' @slot pos A nx3 numeric matrix corresponding to object center position.
setClass(
  Class="Cuboid",  
  slots=c(
    version = "character",   # version of the class
    id = "numeric",
    pos = "matrix",     # position, z = midlle of object
    L = "numeric",    # along x-axis?
    W = "numeric",    # along y-axis?
    H = "numeric",
    theta = "numeric"
  )
)

##--- object 2D: position on the plan and position in 3D
#' An S4 class to represent Trough2D
#'
#' @slot version A length-n character vector indicating the version of RGPR
#' @slot id A length-n numeric vector
#' @slot pos A nx3 numeric matrix corresponding to object center position.
setClass(
  Class="Trough2D",  
  slots=c(
    version = "character",   # version of the class
    id = "numeric",
    pos = "matrix",     # position, z = top of object
    L = "numeric",
    H = "numeric",
    rH = "numeric",
    fill = "list"      # header from *.dt1 file
  )
)

#' An S4 class to represent Deposits2D
#'
#' @slot version A length-n character vector indicating the version of RGPR
#' @slot id A length-n numeric vector
#' @slot pos A nx3 numeric matrix corresponding to object center position.
setClass(
  Class="Deposits2D",  
  slots=c(
    version = "character",   # version of the class
    troughs = "Trough2D",
    layers = "numeric",
    bbox = "list"
  )
)

#' An S4 class to represent TrEllipse
#'
#' @slot version A length-n character vector indicating the version of RGPR
#' @slot id A length-n numeric vector
#' @slot pos A nx3 numeric matrix corresponding to object center position.
setClass(
  Class="TrEllipse",  
  slots=c(
    version = "character",   # version of the class
    id = "numeric",
    pos = "matrix",     # position, z = middle of object
    a = "numeric",
    b = "numeric",
    zmax = "numeric"
  )
)

#' An S4 class to represent Ellipse
#'
#' @slot version A length-n character vector indicating the version of RGPR
#' @slot id A length-n numeric vector
#' @slot pos A nx3 numeric matrix corresponding to object center position.
setClass(
  Class="Ellipse",  
  slots=c(
    version = "character",   # version of the class
    id = "numeric",
    pos = "matrix",     # position, z = middle of object
    a = "numeric",
    b = "numeric",
    theta = "numeric"  # depth position
  )
)

#' An S4 class to represent Rectangle
#'
#' @slot version A length-n character vector indicating the version of RGPR
#' @slot id A length-n numeric vector
#' @slot pos A nx3 numeric matrix corresponding to object center position.
setClass(
  Class="Rectangle",  
  slots=c(
    version = "character",   # version of the class
    id = "numeric",
    pos = "matrix",     # position, z = middle of object
    L = "numeric",
    H = "numeric",
    theta = "numeric"  # depth position
  )
)

#' An S4 class to represent Line
#'
#' @slot version A length-n character vector indicating the version of RGPR
#' @slot id A length-n numeric vector
#' @slot pos A nx3 numeric matrix corresponding to object center position.
setClass(
  Class="Line",
  slots=c(
    version = "character",
    id = "numeric",
    a = "matrix",
    b = "numeric",
    c = "numeric"
  )
)

##------------------- CONSTRUCTORS ------------------##
#' constructeur
#'
#' @export
trough <- function(id = NULL, pos, size, theta, rH, fill = list()){
  if(is.null(dim(pos))){
    dim(pos) <- c(1, length(pos))
  }
  unname(pos)
  colnames(pos) <- c("x", "y", "z")
  if(is.null(dim(size))){
    dim(size) <- c(1, length(size))
  }
  if(is.null(id)){
    id <- seq_along(pos[,1])
  }
  new("Trough",
      version="0.1",
      id = id,
      pos = pos,     # position
      L = size[,1],
      W = size[,2],
      H = size[,3],
      theta = theta,  # depth position
      rH = rH,
      fill = fill
  )
}

#' constructeur
#'
#' @export
spoon <- function(id = NULL, pos, size, theta, rH, rL, fill = list()){
  if(is.null(dim(pos))){
    dim(pos) <- c(1, length(pos))
  }
  unname(pos)
  colnames(pos) <- c("x", "y", "z")
  if(is.null(dim(size))){
    dim(size) <- c(1, length(size))
  }
  if(is.null(id)){
    id <- seq_along(pos[,1])
  }
  new("Spoon",
      version="0.1",
      id = id,
      pos = pos,     # position
      L = size[,1],
      W = size[,2],
      H = size[,3],
      theta = theta,  # depth position
      rH = rH,
      rL = rL,
      fill = fill
  )
}

##------------------- SUBSETTING -----------------##
#' Subsetting
#'
#' @name [[
#' @rdname subsetting
#' @export
setMethod(
    f= "[[",
    signature="Trough",
    definition=function (x, i, j, ...){
      if(missing(i)) i <- j
      myFill <- x@fill
      if(length(myFill) > 0){
        myFill <- myFill[i]
      }
      new("Trough",
          version="0.1",
          id = x@id[i, drop = FALSE],
          pos = x@pos[i, , drop = FALSE],     # position
          L = x@L[i, drop = FALSE],
          W = x@W[i, drop = FALSE],
          H = x@H[i, drop = FALSE],
          theta = x@theta[i, drop = FALSE],  # depth position
          rH = x@rH[i, drop = FALSE],
          fill = myFill
      )
    }
)

#' Subsetting
#'
#' @rdname subsetting
#' @export
setMethod(
    f= "[[",
    signature="Spoon",
    definition=function (x, i, j, ...){
      if(missing(i)) i <- j
      myFill <- x@fill
      if(length(myFill) > 0){
        myFill <- myFill[i]
      }
      new("Spoon",
          version="0.1",
          id = x@id[i, drop = FALSE],
          pos = x@pos[i, , drop = FALSE],     # position
          L = x@L[i, drop = FALSE],
          W = x@W[i, drop = FALSE],
          H = x@H[i, drop = FALSE],
          theta = x@theta[i, drop = FALSE],  # depth position
          rH = x@rH[i, drop = FALSE],
          rL = x@rL[i, drop = FALSE],
          fill = myFill
      )
    }
)

#' Subsetting
#'
#' @rdname subsetting
#' @export
setMethod(
    f= "[[",
    signature="Trough2D",
    definition=function (x, i, j, ...){
      if(missing(i)) i <- j
      myFill <- x@fill
      if(length(myFill) > 0){
        myFill <- myFill[i]
      }
      new("Trough2D",
          version="0.1",
          id = x@id[i, drop = FALSE],
          pos = x@pos[i, , drop = FALSE],     # position
          L = x@L[i, drop = FALSE],
          H = x@H[i, drop = FALSE],
          rH = x@rH[i, drop = FALSE],
          fill = myFill
      )
    }
)


##------------------- CONVERTOR ------------------##
#' As("Ellipsoid", "TrEllipsoid")
#'
#' @name as
#' @family Ellipsoid
setAs(from = "Ellipsoid", to = "TrEllipsoid", def = function(from){
    new("TrEllipsoid",
      version = "0.1",   # version of the class
      id = from@id,
      pos = from@pos,     # position
      a = from@a,
      b = from@b,
      c = from@c,
      theta = from@theta,
      zmax = from@pos[,3] + from@c
    )
  }
)

#' As("TrEllipsoid", "Ellipsoid")
#'
#' @name as
#' @family TrEllipsoid
setAs(from = "TrEllipsoid", to = "Ellipsoid", def = function(from){
    new("Ellipsoid",
      version = "0.1",   # version of the class
      id = from@id,
      pos = from@pos,     # position
      a = from@a,
      b = from@b,
      c = from@c,
      theta = from@theta
    )
  }
)


#' As("Trough", "TrEllipsoid")
#'
#' @name as
#' @family Trough
setAs(from = "Trough", to = "TrEllipsoid", def = function(from){
    cstO2E <- from@rH/(2*sqrt(2*from@rH -1))
    pos <- from@pos
    pos[,3] <- from@pos[,3]  + from@H * (from@rH - 1)
    new("TrEllipsoid",
      version = "0.1",   # version of the class
      id = from@id,
      pos = pos,     # position
      a = from@L * cstO2E,
      b = from@W * cstO2E,
      c = from@H * from@rH,
      theta = from@theta,  # depth position
      zmax = from@pos[,3]
    )
  }
)
#' As("Trough", "Ellipsoid")
#'
#' @name as
#' @family Trough
setAs(from = "Trough", to = "Ellipsoid", def = function(from){
    E <- as(from, "TrEllipsoid")
    as(E, "Ellispoid")
  }
)
#' As("TrEllipsoid", "Trough")
#'
#' @name as
#' @family TrEllipsoid
setAs(from = "TrEllipsoid", to = "Trough", def = function(from){
    pos <- from@pos
    pos[,3] <- from@zmax
    H <- from@c - from@pos[,3] + from@zmax
    rH <- from@c/H
    cstO2E <- rH/(2*sqrt(2*rH -1))
    new("Trough",
      version = "0.1",   # version of the class
      id = from@id,
      pos = pos,     # position
      L = from@a / cstO2E,
      W = from@b / cstO2E,
      H = H,
      theta = from@theta,  # depth position
      rH = rH
    )
  }
)
#' As("Ellipsoid", "Sphere")
#'
#' @name as
#' @family Ellipsoid
setAs(from = "Ellipsoid", to = "Sphere", def = function(from){
    pa <- matrix(c(from@a, from@b, from@c), ncol = 3, nrow = length(from@a))
    new("Sphere",
      version = "0.1",   # version of the class
      id = from@id,
      pos = from@pos,     # position
      r = apply(pa, 1, max)/2
    )
  }
)
#' As("Trough", "Sphere")
#'
#' @name as
#' @family Trough
setAs(from = "Trough", to = "Sphere", def = function(from){
    pa <- matrix(c(from@L, from@W, from@H), ncol = 3, nrow = length(from@L))
    new("Sphere",
      version = "0.1",   # version of the class
      id = from@id,
      pos = from@pos,     # position
      r = apply(pa, 1, max)/2
    )
  }
)
#' As("TrEllipsoid", "Sphere")
#'
#' @name as
#' @family TrEllipsoid
setAs(from = "TrEllipsoid", to = "Sphere", def = function(from){
    O <- as(from, "Trough")
    as(O, "Sphere")
  }
)
#' As("Trough", "Cuboid")
#'
#' @name as
#' @family Trough
setAs(from = "Trough", to = "Cuboid", def = function(from){
    pos <- from@pos
    pos[,3] <- from@pos[,3] - from@H/2
    new("Cuboid",
      version = "0.1",   # version of the class
      id = from@id,
      pos = pos,     # position
      L = from@L,
      W = from@W,
      H = from@H,
      theta = from@theta
    )
  }
)
#' As("TrEllipsoid", "Cuboid")
#'
#' @name as
#' @family TrEllipsoid
setAs(from = "TrEllipsoid", to = "Cuboid", def = function(from){
    O <- as(from, "Trough")
    as(O, "Cuboid")
  }
)
#' As("Ellipsoid", "Cuboid")
#'
#' @name as
#' @family Ellipsoid
setAs(from = "Ellipsoid", to = "Cuboid", def = function(from){
    new("Cuboid",
      version = "0.1",   # version of the class
      id = from@id,
      pos = from@pos,     # position
      L = 2 * from@a,
      W = 2 * from@b,
      H = 2 * from@c,
      theta = from@theta
    )
  }
)


#' As("TrEllipse", "Trough2D")
#'
#' @name as
#' @family TrEllipse
setAs(from = "TrEllipse", to = "Trough2D", def = function(from){
    pos <- from@pos
    pos[,2] <- from@zmax
    H <- from@b - from@pos[,2] + from@zmax
    rH <- from@b/H
    new("Trough2D",
      version = "0.1",   # version of the class
      id = from@id,
      pos = pos,     # position
      L = from@a * 2* sqrt(1 - (from@zmax - from@pos[,2])^2/from@b^2),
      H = from@b - from@pos[,2] + from@zmax,
      rH = rH
    )
  }
)
#' As("Trough2D", "TrEllipse")
#'
#' @name as
#' @family Trough2D
setAs(from = "Trough2D", to = "TrEllipse", def = function(from){
    bbb <- from@rH * from@H
    pos <- from@pos
    pos[,2] <- from@pos[,2] - from@H + bbb
    new("TrEllipse",
      version = "0.1",   # version of the class
      id = from@id,
      pos = pos,     # position
      a = from@L / (2* sqrt(1 - (from@pos[,2] - pos[,2])^2/bbb^2)),
      b = bbb,
      zmax = from@pos[,2]
    )
  }
)


#' Conversion to matrix
#'
#' @rdname as.matrix
#' @export
setMethod("as.matrix", signature(x = "Trough"),function(x){ as(x, "matrix") })
setAs(from = "Trough", to = "matrix", def = function(from){
    M <- matrix(nrow = length(from@L), ncol = 9)
    M[, 1]   <- from@id
    M[, 2:4] <- from@pos
    M[, 5]   <- from@L
    M[, 6]   <- from@W
    M[, 7]   <- from@H
    M[, 8]   <- from@theta
    M[, 9]   <- from@rH
    colnames(M) <- c("id", "x", "y", "z", "L", "W", "H", "theta", "rH")
    return(M)
  }
)
#' Conversion to matrix
#'
#' @rdname as.matrix
#' @export
setMethod("as.matrix", signature(x = "Ellipsoid"), 
          function(x){ as(x, "matrix") })
setAs(from = "Ellipsoid", to = "matrix", def = function(from){
    M <- matrix(nrow = length(from@a), ncol = 8)
    M[, 1]   <- from@id
    M[, 2:4] <- from@pos
    M[, 5]   <- from@a
    M[, 6]   <- from@b
    M[, 7]   <- from@c
    M[, 8]   <- from@theta
    colnames(M) <- c("id", "x", "y", "z", "a", "b", "c", "theta")
    return(M)
  }
)

#' Conversion to matrix
#'
#' @rdname as.matrix
#' @export
setMethod("as.matrix", signature(x = "TrEllipsoid"), 
          function(x){ as(x, "matrix") })
setAs(from = "TrEllipsoid", to = "matrix", def = function(from){
    M <- matrix(nrow = length(from@a), ncol = 9)
    M[, 1]   <- from@id
    M[, 2:4] <- from@pos
    M[, 5]   <- from@a
    M[, 6]   <- from@b
    M[, 7]   <- from@c
    M[, 8]   <- from@theta
    M[, 9]   <- from@zmax
    colnames(M) <- c("id", "x", "y", "z", "a", "b", "c", "theta", "zmax")
    return(M)
  }
)
#' Conversion to matrix
#'
#' @rdname as.matrix
#' @export
setMethod("as.matrix", signature(x = "Sphere"), 
          function(x){ as(x, "matrix") })
setAs(from = "Sphere", to = "matrix", def = function(from){
    M <- matrix(nrow = length(from@r), ncol = 5)
    M[, 1]   <- from@id
    M[, 2:4] <- from@pos
    M[, 5]   <- from@r
    colnames(M) <- c("id", "x", "y", "z", "r")
    return(M)
  }
)
#' Conversion to matrix
#'
#' @rdname as.matrix
#' @export
setMethod("as.matrix", signature(x = "Cuboid"), 
          function(x){ as(x, "matrix") })
setAs(from = "Cuboid", to = "matrix", def = function(from){
    M <- matrix(nrow = length(from@L), ncol = 8)
    M[, 1]   <- from@id
    M[, 2:4] <- from@pos
    M[, 5]   <- from@L
    M[, 6]   <- from@W
    M[, 7]   <- from@H
    M[, 8]   <- from@theta
    colnames(M) <- c("id", "x", "y", "z", "L", "W", "H", "theta")
    return(M)
  }
)
#' Conversion to matrix
#'
#' @rdname as.matrix
#' @export
setMethod("as.matrix", signature(x = "Ellipse"), 
          function(x){ as(x, "matrix") })
setAs(from = "Ellipse", to = "matrix", def = function(from){
    M <- matrix(nrow = length(from@a), ncol = 6)
    M[, 1]   <- from@id
    M[, 2:3] <- from@pos
    M[, 4]   <- from@a
    M[, 5]   <- from@b
    M[, 6]   <- from@theta
    colnames(M) <- c("id", "x", "y", "a", "b", "theta")
    return(M)
  }
)
#' Conversion to matrix
#'
#' @rdname as.matrix
#' @export
setMethod("as.matrix", signature(x = "Ellipse"), 
          function(x){ as(x, "matrix") })
setAs(from = "Ellipse", to = "matrix", def = function(from){
    M <- matrix(nrow = length(from@a), ncol = 6)
    M[, 1]   <- from@id
    M[, 2:3] <- from@pos
    M[, 4]   <- from@a
    M[, 5]   <- from@b
    M[, 6]   <- from@theta
    colnames(M) <- c("id", "x", "y", "a", "b", "theta")
    return(M)
  }
)
#' Conversion to matrix
#'
#' @rdname as.matrix
#' @export
setMethod("as.matrix", signature(x = "TrEllipse"), 
          function(x){ as(x, "matrix") })
setAs(from = "TrEllipse", to = "matrix", def = function(from){
    M <- matrix(nrow = length(from@a), ncol = 6)
    M[, 1]   <- from@id
    M[, 2:3] <- from@pos
    M[, 4]   <- from@a
    M[, 5]   <- from@b
    M[, 6]   <- from@zmax
    colnames(M) <- c("id", "x", "y", "a", "b", "zmax")
    return(M)
  }
)
#' Conversion to matrix
#'
#' @rdname as.matrix
#' @export
setMethod("as.matrix", signature(x = "Trough2D"), 
          function(x){ as(x, "matrix") })
setAs(from = "Trough2D", to = "matrix", def = function(from){
    M <- matrix(nrow = length(from@L), ncol = 6)
    M[, 1]   <- from@id
    M[, 2:3] <- from@pos
    M[, 4]   <- from@L
    M[, 5]   <- from@H
    M[, 6]   <- from@rH
    colnames(M) <- c("id", "x", "y", "L", "H", "rH")
    return(M)
  }
)

#' Conversion to matrix
#'
#' @rdname as.matrix
#' @export
setMethod("as.matrix", signature(x = "Rectangle"), 
          function(x){ as(x, "matrix") })
setAs(from = "Rectangle", to = "matrix", def = function(from){
    M <- matrix(nrow = length(from@L), ncol = 6)
    M[, 1]   <- from@id
    M[, 2:3] <- from@pos
    M[, 4]   <- from@L
    M[, 5]   <- from@H
    M[, 6]   <- from@theta
    colnames(M) <- c("id", "x", "y", "L", "H", "theta")
    return(M)
  }
)

##------------------- METHODS ------------------##

#------------------------------
#' Bbox
#'
#' @name bbox
#' @rdname bbox
#' @export
setGeneric("bbox", function(x) standardGeneric("bbox"))
#' boundary
#'
#' @name boundary
#' @rdname boundary
#' @export
setGeneric("boundary", function(x) standardGeneric("boundary"))

#' extract
#'
#' @name extract
#' @rdname extract
#' @export
setGeneric("extract", function(x, modbox) standardGeneric("extract"))

#' section
#'
#' @name section
#' @rdname section
#' @export
setGeneric("section", function(x, l, pref = NULL, lim = NULL) 
            standardGeneric("section"))

#' plotSection
#'
#' @name plotSection
#' @rdname plotSection
#' @export
setGeneric("plotSection", function(x, add = FALSE, xlab = "x",
            ylab = "y", main = "", asp = NA, ...) 
            standardGeneric("plotSection"))

#' plotObj
#'
#' @name plotObj
#' @rdname plotObj
#' @export
setGeneric("plotObj", function(x, ...) standardGeneric("plotObj"))

#' doIntersect
#'
#' @name doIntersect
#' @rdname doIntersect
#' @export
setGeneric("doIntersect", function(x, y, ...) standardGeneric("doIntersect"))

#' pixelise
#'
#' @name pixelise
#' @rdname pixelise
#' @export
setGeneric("pixelise", function(x, mbox) standardGeneric("pixelise"))

#' crossBedding
#'
#' @name crossBedding
#' @rdname crossBedding
#' @export
setGeneric("crossBedding", function(x, prior)     
            standardGeneric("crossBedding"))

#' plotTopView
#'
#' @name plotTopView
#' @rdname plotTopView
#' @export
setGeneric("plotTopView", function(x, add = FALSE, xlab = "x",
            ylab = "y", main = "", asp = NA, ...) 
            standardGeneric("plotTopView"))

##--------------------------- BBOX ----------------------##
# bounding box of top view object
# source: http://stackoverflow.com/questions/87734/how-do-you-calculate-the-
# axis-aligned-bounding-box-of-an-ellipse
# see also 
# http://www.iquilezles.org/www/articles/ellipses/ellipses.htm
setMethod("bbox", "Trough", function(x){
    pos <- x@pos
    pos[,3] <- x@pos[,3] - x@H/2
    ux <- x@L / 2 * cos(x@theta)
    uy <- x@L / 2 * sin(x@theta)
    vx <- x@W / 2 * cos(x@theta + pi/2)
    vy <- x@W / 2 * sin(x@theta + pi/2)
    L <- sqrt(ux^2 + vx^2)
    W <- sqrt(uy^2 + vy^2)
    new("Cuboid",
      version = "0.1",   # version of the class
      id = x@id,
      pos = pos,     # position
      L = 2 * L,
      W = 2 * W,
      H = x@H,
      theta = rep(0, length(L))
    )
  }
)

setMethod("bbox", "Deposits", function(x){
    LWH <- sapply(x@bbox, function(x) diff(x))
    pos <- sapply(x@bbox, function(x) diff(x)/2 + x[1])
    new("Cuboid",
      version = "0.1",   # version of the class
      id = 1,
      pos = matrix(pos, nrow = 1),     # position
      L = LWH[1],
      W = LWH[2],
      H = LWH[3],
      theta = 0
    )
  }
)

spoon2trough <- function(from){
  xl <- new("Trough",
          version = "0.1",   # version of the class
          id = from@id,
          pos = from@pos,     # position
          L = 2*from@L * (1 - from@rL),
          W = from@W,
          H = from@H,
          theta = from@theta,  # depth position
          rH = from@rH
        )
  xs <- new("Trough",
          version = "0.1",   # version of the class
          id = from@id,
          pos = from@pos,     # position
          L = 2*from@L * from@rL,
          W = from@W,
          H = from@H,
          theta = from@theta,  # depth position
          rH = from@rH
        )
  return(list(xl, xs))
}

setMethod("bbox", "Spoon", function(x){
    xT <- spoon2trough(x)
    bbox1 <- bbox(xT[[1]])
    bbox2 <- bbox(xT[[2]])
    pos <- x@pos
    pos[,3] <- x@pos[,3] - x@H/2
    L1 <- x@L * (1 - x@rL)
    L2 <- x@L * x@rL
    l <- L1 - x@L/2
    pos[,1:2] <- l*c(cos(x@theta), sin(x@theta)) + x@pos[,1:2]
    new("Cuboid",
      version = "0.1",   # version of the class
      id = x@id,
      pos = pos,     # position
      L = bbox1@L/2 +  bbox2@L/2,
      W = bbox1@W/2 +  bbox2@W/2,
      H = x@H,
      theta = rep(0, length(x@L))
    )
  }
)

setMethod("bbox", "TrEllipsoid", function(x){
    O <- as(x, "Trough")
    bbox(O)
    # pos <- x@pos
    # H <- x@zmax - x@pos[,3] + x@c
    # # z_cuboid = z - c + H/2
    # pos[,3] <- x@pos[,3] - x@c + H/2
    # rH <- x@c/H
    # cstO2E <- rH/(2*sqrt(2*rH - 1))
    # L0 <- x@a/cstO2E
    # W0 <- x@b/cstO2E
    # ux <- L0 / 2 * cos(x@theta)
    # uy <- L0 / 2 * sin(x@theta)
    # vx <- W0 / 2 * cos(x@theta + pi/2)
    # vy <- W0 / 2 * sin(x@theta + pi/2)
    # L <- sqrt(ux^2 + vx^2)
    # W <- sqrt(uy^2 + vy^2)
    # new("Cuboid",
    #   version = "0.1",   # version of the class
    #   id = x@id,
    #   pos = pos,     # position
    #   L = 2 * L,
    #   W = 2 * W,
    #   H = H,
    #   theta = rep(0, length(L))
    # )
  }
)

setMethod("bbox", "Ellipsoid", function(x){
    ux <- x@a * cos(x@theta)
    uy <- x@a * sin(x@theta)
    vx <- x@b * cos(x@theta + pi/2)
    vy <- x@b * sin(x@theta + pi/2)
    L <- sqrt(ux^2 + vx^2)
    W <- sqrt(uy^2 + vy^2)
    new("Cuboid",
      version = "0.1",   # version of the class
      id = x@id,
      pos = x@pos,     # position
      L = 2 * L,
      W = 2 * W,
      H = 2* x@c,
      theta = rep(0, length(L))
    )
  }
)

setMethod("bbox", "Sphere", function(x){
    new("Cuboid",
      version = "0.1",   # version of the class
      id = x@id,
      pos = x@pos,     # position
      L = 2 * x@r,
      W = 2 * x@r,
      H = 2 * x@r,
      theta = rep(0, length(x@r))
    )
  }
)

setMethod("bbox", "Cuboid", function(x){
    costheta <- abs(cos(x@theta))
    sintheta <- abs(sin(x@theta))
    L <- x@L * costheta + x@W * sintheta
    W <- x@L * sintheta + x@W * costheta
    new("Cuboid",
      version = "0.1",   # version of the class
      id = x@id,
      pos = x@pos,     # position
      L = L,
      W = W,
      H = x@H,
      theta = rep(0, length(L))
    )
  }
)


setMethod("bbox", "Trough2D", function(x){
    pos <- x@pos
    pos[,2] <- x@pos[,2] - x@H/2
    new("Rectangle",
      version = "0.1",   # version of the class
      id = x@id,
      pos = pos,     # position
      L = x@L,
      H = x@H,
      theta = rep(0, length(x@L))
    )
  }
)

setMethod("bbox", "TrEllipse", function(x){
    E <- as(x, "Trough2D")
    bbox(E)
    # pos <- x@pos
    # H <- x@zmax - x@pos[,2] + x@b
    # # z_cuboid = z - c + H/2
    # pos[,2] <- x@pos[,2] - x@b + H/2
    # rH <- x@b/H
    # cstO2E <- rH/(2*sqrt(2*rH - 1))
    # L0 <- x@a/cstO2E
    # W0 <- x@b/cstO2E
    # ux <- L0 / 2 * cos(x@theta)
    # uy <- L0 / 2 * sin(x@theta)
    # vx <- W0 / 2 * cos(x@theta + pi/2)
    # vy <- W0 / 2 * sin(x@theta + pi/2)
    # L <- sqrt(ux^2 + vx^2)
    # W <- sqrt(uy^2 + vy^2)
    # new("Rectangle",
    #   version = "0.1",   # version of the class
    #   id = x@id,
    #   pos = pos,     # position
    #   L = 2 * L,
    #   H = H,
    #   theta = rep(0, length(L))
    # )
  }
)
setMethod("bbox", "Ellipse", function(x){
    ux <- x@a / 2 * cos(x@theta)
    uy <- x@a / 2 * sin(x@theta)
    vx <- x@b / 2 * cos(x@theta + pi/2)
    vy <- x@b / 2 * sin(x@theta + pi/2)
    L <- sqrt(ux^2 + vx^2)
    W <- sqrt(uy^2 + vy^2)
    new("Rectangle",
      version = "0.1",   # version of the class
      id = x@id,
      pos = x@pos,     # position
      L = 2 * L,
      H = 2 * W,
      theta = rep(0, length(L))
    )
  }
)

setMethod("bbox", "Rectangle", function(x){
    costheta <- abs(cos(x@theta))
    sintheta <- abs(sin(x@theta))
    L <- x@L * costheta + x@H * sintheta
    H <- x@L * sintheta + x@H * costheta
    new("Rectangle",
      version = "0.1",   # version of the class
      id = x@id,
      pos = x@pos,     # position
      L = L,
      H = H,
      theta = rep(0, length(L))
    )
  }
)


##---------------------------- EXTRACT ----------------------##
setMethod("extract", "Deposits", function(x, modbox){
    x@troughs <- extract(x@troughs, modbox)
    return(x)
  }
)
setMethod("extract", "Trough", function(x, modbox){
    bb <- bbox(x)
    bbmin <- bb@pos + c(bb@L, bb@W, bb@H)/2
    bbmax <- bb@pos - c(bb@L, bb@W, bb@H)/2
    mbmin <- matrix(c(modbox$x[1], modbox$y[1], modbox$z[1]),
                    nrow = nrow(bbmin), ncol = 3, byrow = TRUE)
    mbmax <-  matrix(c(modbox$x[2], modbox$y[2], modbox$z[2]),
                    nrow = nrow(bbmax), ncol = 3, byrow = TRUE)
    sel <- apply( bbmin >= mbmin & bbmax <= mbmax , 1, all)
    return(x[[sel]])
  }
)


##--------------------------- BOUNDARY ----------------------##
.boundary3D <- function(x){
  B <- bbox(x)
  xmin <- B@pos[, 1] - B@L/2 
  xmax <- B@pos[, 1] + B@L/2 
  ymin <- B@pos[, 2] - B@W/2 
  ymax <- B@pos[, 2] + B@W/2 
  zmin <- B@pos[, 3] - B@H/2 
  zmax <- B@pos[, 3] + B@H/2
  b <- c(xmin = min(xmin),
          xmax = max(xmax),
          ymin = min(ymin),
          ymax = max(ymax),
          zmin = min(zmin),
          zmax = max(zmax))
  return(b)
}

setMethod("boundary", "Trough", function(x){
    .boundary3D(x)
  }
)
setMethod("boundary", "TrEllipsoid", function(x){
    x <- as(x, "Trough")
    .boundary3D(x)
  }
)
setMethod("boundary", "Ellipsoid", function(x){
    .boundary3D(x)
  }
)
setMethod("boundary", "Sphere", function(x){
    .boundary3D(x)
  }
)
setMethod("boundary", "Cuboid", function(x){
    .boundary3D(x)
  }
)

.boundary2D <- function(x){
  B <- bbox(x)
  xmin <- B@pos[, 1] - B@L/2 
  xmax <- B@pos[, 1] + B@L/2 
  ymin <- B@pos[, 2] - B@H/2 
  ymax <- B@pos[, 2] + B@H/2
  b <- c(xmin = min(xmin),
          xmax = max(xmax),
          ymin = min(ymin),
          ymax = max(ymax))
  return(b)
}

setMethod("boundary", "Ellipse", function(x){
    .boundary2D(x)
  }
)
setMethod("boundary", "TrEllipse", function(x){
    .boundary2D(x)
  }
)
setMethod("boundary", "Trough2D", function(x){
    .boundary2D(x)
  }
)
setMethod("boundary", "Rectangle", function(x){
    .boundary2D(x)
  }
)


##----------------------- plot -------------------------##
setMethod("plotObj", "Rectangle", function(x, add = FALSE, xlab = "x",
            ylab = "y", main = "", asp = NA, xaxs = "i", yaxs = "i", ...){
    if(add == FALSE){
      b <- boundary(x)
      plot(0,0, type = "n", xlab = xlab, ylab = ylab, main = main,
            xlim = b[c("xmin", "xmax")], ylim = b[c("ymin", "ymax")],
            asp = asp, xaxs = xaxs, yaxs = yaxs)
    }
    E <- as.matrix(x)
    if(length(E) > 0 ){
      invisible(apply(E, 1, .plotRectangle, ...) )
    }else{
      cat("no objects to plot!\n")
    }
  }
)


setMethod("plotObj", "Ellipse", function(x, add = FALSE, xlab = "x",
            ylab = "y", main = "", asp = NA, xaxs = "i", yaxs = "i", ...){
    if(add == FALSE){
      b <- boundary(x)
      plot(0,0, type = "n", xlab = xlab, ylab = ylab, main = main,
            xlim = b[c("xmin", "xmax")], ylim = b[c("ymin", "ymax")],
            asp = asp, xaxs = xaxs, yaxs = yaxs)
    }
    E <- as.matrix(x)
    if(length(E) > 0 ){
      invisible(apply(E, 1, .plotEllipse, ...) )
    }else{
      cat("no objects to plot!\n")
    }
  }
)

setMethod("plotObj", "TrEllipse", function(x, add = FALSE, xlab = "x",
            ylab = "y", main = "", asp = NA, xaxs = "i", yaxs = "i", ...){
    if(add == FALSE){
      b <- boundary(x)
      plot(0,0, type = "n", xlab = xlab, ylab = ylab, main = main,
            xlim = b[c("xmin", "xmax")], ylim = b[c("ymin", "ymax")],
            asp = asp, xaxs = xaxs, yaxs = yaxs)
    }
    E <- as.matrix(x)
    if(length(E) > 0 ){
      invisible(apply(E, 1, .plotTrEllipse, ...) )
    }else{
      cat("no objects to plot!\n")
    }
  }
)

setMethod("plotObj", "Trough2D", function(x, add = FALSE, xlab = "x",
            ylab = "y", main = "", asp = NA, xaxs = "i", yaxs = "i", ...){
    xE <- as(x, "TrEllipse")
    dots <- list()
    if(add == FALSE){
      b <- boundary(xE)
      dots <- list(...)
      if(!is.null(dots$xlim)){
        xlim <- dots$xlim
        dots$xlim <- NULL
      }else{
        xlim <- b[c("xmin", "xmax")]
      }
      if(!is.null(dots$ylim)){
        ylim <- dots$ylim
        dots$ylim <- NULL
      }else{
        ylim <- b[c("ymin", "ymax")]
      }
      plot(0,0, type = "n", xlab = xlab, ylab = ylab, main = main,
            xlim = xlim, ylim = ylim, asp = asp, xaxs = xaxs, yaxs = yaxs)
    }
    E <- as.matrix(xE)
    n <- nrow(E)
    if(length(E) > 0 ){
      for(i in seq_len(n)){
        .plotTrEllipse(E[i,], ...)
        if(length(x@fill) != 0){
          if(!is.null(x@fill[[i]])){
            plotObj(as(x@fill[[i]], "TrEllipse"), add = TRUE, ...)
          }
        }
      }
#       invisible(apply(E, 1, .plotTrEllipse) )
    }else{
      cat("no objects to plot!\n")
    }
  }
)

.plotRectangle <- function(ob, ...){
  XY <- .rect(xy = ob[c("x", "y")], L = ob["L"], H = ob["H"],
              theta = ob["theta"])
  polygon(x = XY[,1], y = XY[,2], ...)
}

# return the corner coordinates
.rect <- function(xy, L, H, theta = 0){
  X <- matrix(c(-L/2, -L/2, L/2,  L/2,
                -H/2,  H/2, H/2, -H/2),
              ncol = 2, nrow = 4)
  if(theta  != 0){
    rot <- matrix(c(cos(theta),  -sin(theta),
                    sin(theta),   cos(theta)), nrow = 2, ncol = 2)
    X <- X %*% rot
  }
  return(X + matrix(xy, ncol = 2, nrow = 4, byrow = TRUE))
}

.plotEllipse <- function(e, ...){
  polygon(RConics::ellipse(saxes = e[c("a", "b")], 
                           loc = e[c("x", "y")], 
                           theta = e[c("theta")]),...)
}

.plotTrEllipse <- function(e, ...){
  polygon(.trEllipse(saxes = e[c("a", "b")], 
                     loc   = e[c("x", "y")],
                     theta = 0,
                     zmax  = e["zmax"],
                     alpha = c(0.5, 1)), ...)
}


.trEllipse <- function(saxes=c(2,1), loc = c(0,0), theta = 0, alpha = c(0,1),
                       n = 201, zmax = NULL, xmax = NULL, side = 1){
  phi <- 2*pi*seq(alpha[1], alpha[2], len = n)
  P <- matrix(nrow=n,ncol=2)
  P[,1] <- saxes[1] * cos(phi)
  P[,2] <- saxes[2] * sin(phi)
  if(theta != 0){
    P <- P %*% matrix(c( cos(theta), sin(theta), -sin(theta), cos(theta)),
                      byrow = TRUE, nrow = 2,ncol = 2)
  }
  P <- P + matrix(loc[1:2],nrow=nrow(P),ncol=2,byrow=TRUE)
  if(!is.null(zmax)){
    halfLength <- saxes[1] *sqrt(1 - (zmax - loc[2])^2/saxes[2] ^2)
    xtop1 <- loc[1] - halfLength
    xtop2 <- loc[1] + halfLength
    P <- rbind( c(xtop1, zmax)  , P[ P[,2] < zmax, ] , c(xtop2, zmax))
  }
  if(!is.null(xmax)){
    ytop1 <- loc[2] - saxes[2] *sqrt(1 - (xmax - loc[1])^2/saxes[1] ^2)
    if(side==2){
      P <- rbind( c(xmax, ytop1), P[ P[,1] > xmax, ] )
    }else{
      P <- rbind( P[ P[,1] < xmax, ] , c(xmax, ytop1))
    }
  }
  return(P)
}

##--------------------------- TOPVIEW ----------------------##
# for 3D object
# ... arguments to be passed to "base::polygon" function
setMethod("plotTopView", "Deposits", function(x, add = FALSE, xlab = "x",
            ylab = "y", main = "", asp = NA, ...){
    dots <- list(...)
    if(!is.null(dots$xlim)){
      xlim <- dots$xlim
    }else{
      xlim <- x@bbox$x
    }
    if(!is.null(dots$ylim)){
      ylim <- dots$ylim
    }else{
      ylim <- x@bbox$y
    }
    plotTopView(x@troughs, add = add, xlab = xlab, ylab = ylab, main = main, 
                asp = asp, xlim = xlim, ylim = ylim, ...)
  }
)

# for 3D object
# ... arguments to be passed to "base::polygon" function
setMethod("plotTopView", "Trough", function(x, add = FALSE, xlab = "x",
            ylab = "y", main = "", asp = NA, xaxs = "i", yaxs = "i", ...){
    if(add==FALSE){
      b <- boundary(x)
      dots <- list(...)
      if(!is.null(dots$xlim)){
        xlim <- dots$xlim
      }else{
        xlim <- b[c("xmin", "xmax")]
      }
      if(!is.null(dots$ylim)){
        ylim <- dots$ylim
      }else{
        ylim <- b[c("ymin", "ymax")]
      }
      plot(0,0, type = "n", xlab = xlab, ylab = ylab, main = main,
            xlim = xlim, ylim = ylim,
            asp = asp, xaxs = xaxs, yaxs = yaxs)
    }
    E <- as.matrix(x)
    n <- nrow(E)
    if(length(E) > 0 ){
      for(i in seq_len(n)){
        .plotTroughTop(E[i,], ...)
        if(length(x@fill) != 0){
          if(!is.null(x@fill[[i]])){
            plotTopView(x@fill[[i]], add = TRUE, ...)
          }
        }
      }
#       invisible(apply(E, 1, .plotTrEllipse) )
    }else{
      cat("no objects to plot!\n")
    }
#     
#     
#     if(length(E) > 0 ){
#       invisible(apply(E, 1, .plotTroughTop, ...) )
#       if(length(x@fill) > 0){
#         invisible(lapply(x@fill, plotTopView, add = TRUE, asp = NA, ...))
#       }
#     }else{
#       cat("no objects to plot!\n")
#     }
  }
)

setMethod("plotTopView", "Spoon", function(x, add = FALSE, xlab = "x",
            ylab = "y", main = "", asp = NA, xaxs = "i", yaxs = "i", ...){
    if(add==FALSE){
      b <- boundary(x)
      plot(0,0, type = "n", xlab = xlab, ylab = ylab, main = main,
            xlim = b[c("xmin", "xmax")], ylim = b[c("ymin", "ymax")],
            asp = asp, xaxs = xaxs, yaxs = yaxs)
    }
    E <- as.matrix(x)
    if(length(E) > 0 ){
      invisible(apply(E, 1, .plotTroughTop, ...) )
      if(length(x@fill) > 0){
        invisible(lapply(x@fill, plotTopView, add = TRUE, asp = NA, ...))
      }
    }else{
      cat("no objects to plot!\n")
    }
  }
)


# ... arguments to be passed to "base::polygon" function
setMethod("plotTopView", "TrEllipsoid", function(x, add = FALSE, xlab = "x",
            ylab = "y", main = "", asp = NA, ...){
    plotTopView(as(x, "Trough"), add = add, xlab = xlab,
                ylab = ylab, main = main, asp = asp, ...)
  }
)

# ... arguments to be passed to "base::polygon" function
setMethod("plotTopView", "Sphere", function(x, add = FALSE, xlab = "x",
            ylab = "y", main = "", asp = NA, xaxs = "i", yaxs = "i", ...){
    if(add==FALSE){
      b <- boundary(x)
      plot(0,0, type = "n", xlab = xlab, ylab = ylab, main = main,
            xlim = b[c("xmin", "xmax")], ylim = b[c("ymin", "ymax")],
            asp = asp, xaxs = xaxs, yaxs = yaxs)
    }
    E <- matrix(0, nrow = length(x@r), ncol = 7)
    colnames(E) <- c("id", "x", "y", "z", "a", "b", "theta")
    E[,1:5] <- as.matrix(x)
    E[,"a"] <- E[,"a"]
    E[,"b"] <- E[,"a"]
    if(length(E) > 0 ){
      invisible(apply(E, 1, .plotEllipse, ...) )
    }else{
      cat("no objects to plot!\n")
    }
  }
)

# ... arguments to be passed to "base::polygon" function
setMethod("plotTopView", "Cuboid", function(x, add = FALSE, xlab = "x",
            ylab = "y", main = "", asp = NA, xaxs = "i", yaxs = "i", ...){
    if(add==FALSE){
      b <- boundary(x)
      plot(0,0, type = "n", xlab = xlab, ylab = ylab, main = main,
            xlim = b[c("xmin", "xmax")], ylim = b[c("ymin", "ymax")],
            asp = asp, xaxs = xaxs, yaxs = yaxs)
    }
    # first corner
    E <- as.matrix(x)
    if(length(E) > 0 ){
      invisible(apply(E, 1, .plotCuboidTop, ...) )
    }else{
      cat("no objects to plot!\n")
    }
  }
)


.plotTroughTop <- function(e, ...){
  polygon(RConics::ellipse(saxes = e[c("L", "W")]/2, 
                           loc = e[c("x", "y")], 
                           theta = e[c("theta")]),...)
}

.plotCuboidTop <- function(ob, ...){
  XY <- .rect(xy = ob[c("x", "y")], L = ob["L"], H = ob["W"],
              theta = ob["theta"])
  polygon(x = XY[,1], y = XY[,2], ...)
}

##----------------------- pixelise -------------------------##
# mbox <- list(x = c(xmin, xmax),
#             ...
#             dx = 1,
#             ...)
setMethod("pixelise", "Trough", function(x, mbox){
    nx <- (mbox$x[2] - mbox$x[1])/mbox$dx
    ny <- (mbox$y[2] - mbox$y[1])/mbox$dy
    nz <- (mbox$z[2] - mbox$z[1])/mbox$dz
    vx <- seq(mbox$x[1], to = mbox$x[2], length.out = nx)
    vy <- seq(mbox$y[1], to = mbox$y[2], length.out = ny)
    vz <- seq(mbox$z[1], to = mbox$z[2], length.out = nz)
    XYZ <- array( 0, dim=c(nx, ny,nz))
    E <- as.matrix(x)
    cstO2E <- x@rH/(2*sqrt(2*x@rH -1))
    n <- nrow(E)
    vol <- numeric(n)
    b <- bbox(x)
    for(i in 1:n){
      A <- .pixeliseTrough(e = E[i, ], i, L = b@L[i], W = b@W[i],
                                vx = vx, vy = vy, vz = vz, 
                                mbox = mbox, XYZ = XYZ, cstO2Ei = cstO2E[i])
      vol[i] <- A$vol
      XYZ <- A$XYZ
    }
    return(list("XYZ" = XYZ, x = vx, y = vy, z = vz, "vol" = vol))
  }
)



setMethod("pixelise", "Deposits", function(x, mbox){
    # 0. mbox
    nx <- (mbox$x[2] - mbox$x[1])/mbox$dx
    ny <- (mbox$y[2] - mbox$y[1])/mbox$dy
    nz <- (mbox$z[2] - mbox$z[1])/mbox$dz
    vx <- seq(mbox$x[1] + mbox$dx/2, to = mbox$x[2] - mbox$dx/2, 
              length.out = nx)
    vy <- seq(mbox$y[1] + mbox$dy/2, to = mbox$y[2] - mbox$dy/2, 
              length.out = ny)
    vz <- seq(mbox$z[1] + mbox$dz/2, to = mbox$z[2] - mbox$dz/2, 
              length.out = nz)
    XYZ <- array( 0, dim=c(nx, ny,nz))
    # 1. discretise layers
    #    -> negative id
    lay <- x@layers
    for(i in seq_along(lay)){
      XYZ[, , lay[i] <= vz] <- -i
    }
    # Pix <- list("XYZ" = XYZ, x = vx, y = vy, z = vz)
    # 2. discretise trough
    #    -> postive id -> odd  = bimodal
    #                  -> even = open-framework
    E <- as.matrix(x@troughs)
    cstO2E <- x@troughs@rH/(2*sqrt(2*x@troughs@rH -1))
    n <- nrow(E)
    vol <- numeric(n)
    b <- bbox(x@troughs)
    it <- 0
    for(i in 1:n){
      it <- it + 1
      if((it %% 2) == 0) it <- it + 1
      A <- .pixeliseTrough(e = E[i, ], it, L = b@L[i], W = b@W[i],
                                vx = vx, vy = vy, vz = vz,  
                                mbox = mbox, XYZ = XYZ, cstO2Ei = cstO2E[i])
      vol[i] <- A$vol
      XYZ <- A$XYZ
      if(!is.null(x@troughs@fill[[i]]) && length(x@troughs@fill[[i]]) > 0){
        Ei <- as.matrix(x@troughs@fill[[i]])
        for(k in 1:nrow(Ei)){
          it <- it + 1
          A <- .pixeliseTrough(e = Ei[k, ], it, L = b@L[i], W = b@W[i],
                                vx = vx, vy = vy, vz = vz, 
                                mbox = mbox, XYZ = XYZ, cstO2Ei = cstO2E[i])
          vol[i] <- A$vol
          XYZ <- A$XYZ
        }
      }
    }
    Pix <- list("XYZ" = XYZ, x = vx, y = vy, z = vz, "vol" = vol)
    return(list("XYZ" = XYZ, x = vx, y = vy, z = vz, "vol" = vol))
  }
)

setMethod("pixelise", "Deposits2D", function(x, mbox){
    nx <- (mbox$x[2] - mbox$x[1])/mbox$dx
    nz <- (mbox$z[2] - mbox$z[1])/mbox$dz
    vx <- seq(mbox$x[1] + mbox$dx/2, to = mbox$x[2] - mbox$dx/2, 
              length.out = nx)
    vz <- seq(mbox$z[1] + mbox$dz/2, to = mbox$z[2] - mbox$dz/2, 
              length.out = nz)
    XZ <- matrix(0, nrow = nx, ncol = nz)
    # 1. discretise layers
    #    -> negative id
    lay <- x@layers
    for(i in seq_along(lay)){
      XZ[, lay[i] <= vz] <- -i
    }
    # Pix <- list("XYZ" = XYZ, x = vx, y = vy, z = vz)
    # 2. discretise trough
    #    -> postive id -> odd  = bimodal
    #                  -> even = open-framework
    b <- bbox(x@troughs)
    E <- as.matrix(as(x@troughs, "TrEllipse"))
    it <- 0
    for(i in 1:nrow(E)){
      it <- it + 1
      if((it %% 2) == 0) it <- it + 1
      e <- E[i,]  # ellispoid e
      L <- b@L[i]
      H <- b@H[i]
      XZ <- .pixeliseTrEllipse(e = E[i,], i = it, L = b@L[i], H = b@H[i], 
                         vx, vz, XZ)
      if(!is.null(x@troughs@fill[[i]]) && length(x@troughs@fill[[i]]) > 0){
        Ei <- as.matrix(as(x@troughs@fill[[i]], "TrEllipse"))
        for(k in 1:nrow(Ei)){
          it <- it + 1
          XZ <- .pixeliseTrEllipse(e = Ei[k, ], i = it, L = b@L[i], H = b@H[i],
                                vx = vx, vz = vz, XZ)
        }
      }
    }
    return(list(z = XZ, x = vx, y = vz))
  }
)


.pixeliseTrEllipse <- function(e, i, L, H, vx, vz, XZ){
  testx <- vx >= (e["x"] - L/2)  & vx <= (e["x"] + L/2)
  testz <- vz >= (e["zmax"] - H) & vz <= (e["zmax"])
  if( any(testx) && any(testz) ){
    xComponent <- ( (vx[testx] - e["x"]) / e["a"] )^2
    zComponent <- ( (vz[testz] - e["y"]) / e["b"] )^2
    xyComponent <- outer(xComponent,zComponent,'+')
    XZ[testx, testz][xyComponent <=1 ] <- i
  }
  return(XZ)
}

# L <- b@L[i]
# W <- b@W[i]
# cstO2Ei <- cstO2E[i]
# voli <- vol[i]
.pixeliseTrough <- function(e, i, L, W, vx, vy, vz, mbox, XYZ, cstO2Ei){
  vol <- 0
  xr <- vx[ vx >= (e["x"] - L/2)    & vx <= (e["x"] + L/2)]
  yr <- vy[ vy >= (e["y"] - W/2)    & vy <= (e["y"] + W/2)]
  zr <- vz[ vz >= (e["z"] - e["H"]) & vz <= (e["z"])]
  if( length(xr)!=0 && length(yr)!=0 && length(zr) != 0 ){
    xnew <- rep(xr - e["x"], length(yr))
    ynew <- rep(yr - e["y"], each = length(xr))
    xComponent <- (( xnew * cos(e["theta"]) + ynew *sin(e["theta"])) / 
                    (e["L"] * cstO2Ei))^2
    yComponent <- ((-xnew*sin(e["theta"]) + ynew*cos(e["theta"])) / 
                    (e["W"] * cstO2Ei))^2
    xyComponent <- xComponent + yComponent
    id_i <- round((xr - mbox$x[1] + mbox$dx/2) / mbox$dx)
    id_j <- round((yr - mbox$y[1] + mbox$dy/2) / mbox$dy)
    z <- e["z"]  + e["H"] * (e["rH"] - 1)
    zComponent <- (( zr - z) / (e["H"] * e["rH"]) )^2
    id_k <- round((zr - mbox$z[1] + mbox$dz/2) / mbox$dz)
    for(k in seq_along(zr)){
      condition <- (xyComponent + zComponent[k]) <= 1
      vol <- vol + sum(condition)
      XYZ[id_i, id_j, id_k[k]][condition] <- i
    }
  }
  return(list("XYZ" = XYZ, "vol" = vol))
}

#---------------------- set properties to pixels


#' Set properties
#'
#' 
#' @export
setProp <- function(A, type = c("facies", "K"), FUN, ...){
  fac <- list()
  fac$gp <- A < 0                # poorly sorted gravel (GP)
  fac$bm <- (A %% 2) == 0 & !fac$gp   # bimodal gravel (BM)
  fac$ow <- (A %% 2) != 0 & !fac$gp   # open-framework gravel (OW)
  if(!is.null(type)){
    type <- match.arg(type, c("facies", "K"))
    if(type == "K"){
      TT <- lapply(names(fac), .setProp, A, fac, .funK, depprop)
    }else if( type == "facies"){
      TT <- lapply(names(fac), .setProp, A, fac, .funn)
    }
  }else{
    TT <- lapply(names(fac), .setProp, A, fac, FUN, ...)
  }
  A[] <- NA
  A[fac$gp] <- TT[[1]][fac$gp]
  A[fac$bm] <- TT[[2]][fac$bm]
  A[fac$ow] <- TT[[3]][fac$ow]
  return(A)
}

.setProp <- function(facies = "gp", A, fac, FUN, ...){
  A0 <- A
  A0[] <- NA
  ufac <- unique(A[fac[[facies]]])
  n <- length(ufac)
  prop <- FUN(n, facies, ...)
  for(k in seq_len(n)){
    A0[A == ufac[k]] <- prop[k] 
  }
  return(A0)
}

.funK <- function(n, facies, depprop){
  rlognorm(n, mean = depprop[[facies]]["K"], 
              sdlog = depprop[[facies]]["sdlogK"])
}

.funn <- function(n, facies){
  if(facies == "gp") x <- 0L
  if(facies == "bm") x <- 1L
  if(facies == "ow") x <- 2L
  rep(x, n)
}

##--------------------------- INTERSECT ----------------------##
setMethod("doIntersect", "Trough", function(x, y, ...){
    test <- apply(as.matrix(x), 1, .intersectTrough, l = y)
    return(test)
  }
)
         
setMethod("doIntersect", "Sphere", function(x, y, ...){
    l <- y
    # orthogonal projection matrix
    OPmat <- .matOP(l)
    # point (x,y) on the line l
    pl <- c(-l[3]/l[1],0)
    test <- apply(as.matrix(x), 1, .intersectSphere, OPmat = OPmat, pl = pl)
    return(test)
  }
)

.intersectTrough <- function(ob, l){
  RC <- RConics::ellipseToConicMatrix(saxes = c(ob["L"], ob["W"])/2,
                                      loc = ob[c("x","y")], 
                                      theta = ob["theta"])
  p0 <- RConics::intersectConicLine(RC , l) 
  test <- ifelse(length(p0) >0, TRUE, FALSE)
  return(test)
}

.intersectSphere <- function(ob, OPmat, pl, p1 = NULL, p2 = NULL){
  p_proj <- as.vector(OPmat %*% (ob[c("x","y")] - pl) + pl)
  test <- sqrt(sum((p_proj - ob[c("x","y")])^2)) <= ob["r"]
  if(test && !is.null(p1) && !is.null(p2)){
    test <- p_proj[1] > p1[1] & p_proj[1] < p2[1] &
            p_proj[2] > p1[2] & p_proj[2] < p2[2] 
      test <- ifelse(test == FALSE, min(sqrt(sum((p_proj - p1)^2)), 
                      sqrt(sum((p_proj - p2)^2))) <  ob["r"],test)
    return(test)
  }
  return(test)
}


##--------------------------- SECTION ----------------------##
setMethod("section", "TrEllipsoid", function(x, l, pref = NULL, lim = NULL){
    E <- as.matrix(x)
    E <- apply(E, 1, .sectionEllipsoid, l = l, pref = pref)
    if(length(E) > 0) {
      if(is.matrix(E)){
        E <-t(E)
      }else{
        E <- do.call(rbind, .compactList(E))
      }
      new("TrEllipse",
        version = "0.1",   # version of the class
        id = E[,"id"],
        pos = E[,c("xap", "z"), drop = FALSE],     # position, z = top of object
        a = E[,"aap"],
        b = E[,"bap"],
        zmax = E[, "zmax"]      # header from *.dt1 file
      )
      #  return(E)
    }
  }
)

setMethod("section", "Trough", function(x, l, pref = NULL, lim = NULL){
    # E0 <- as(x, "TrEllipsoid")
    El <- section(as(x, "TrEllipsoid"), l, pref = pref, lim = lim)
    if(!is.null(El)){
      xsec <- as(El, "Trough2D")
      if(length(x@fill) > 0){
        xsec@fill <- lapply(x@fill[El@id], section, l = l, 
                            pref = pref, lim = lim)
      }
      return(xsec)
    }else{
      return(NULL)
    }
  }
)

setMethod("section", "Deposits", function(x, l, pref = NULL, lim = NULL){
    pp <- section(bbox(x), l)
    xsec <- section(x@troughs, l, pref = pp[[1]], lim = lim)
    if(!is.null(xsec)){
      new("Deposits2D",
          version = "0.1",
          troughs = xsec,
          layers = x@layers
          )
    }else{
      return(NULL)
    }
  }
)


setMethod("section", "Cuboid", function(x, l, pref = NULL, lim = NULL){
    crns <- .rect(x@pos[1:2], x@L, x@W, x@theta)
    ls <- list()
    ls$bot <- RConics::join(c(crns[4, ], 1), c(crns[1, ], 1))
    ls$lef <- RConics::join(c(crns[1, ], 1), c(crns[2, ], 1))
    ls$top <- RConics::join(c(crns[2, ], 1), c(crns[3, ], 1))
    ls$rig <- RConics::join(c(crns[3, ], 1), c(crns[4, ], 1))
    # plot(0, type = "n", ylim = c(-50, 250), xlim = c(-50, 250))
    # invisible(sapply(ls, RConics::addLine))
    # RConics::addLine(l, col = "red")
    pts <- lapply(ls, RConics::join,  l)
    # invisible(sapply(pts, function(x, ...) points(t(x), ...)))
    fSel <- function(p, xmin, xmax, ymin, ymax){
        (p[1] >= xmin & p[1] <= xmax) & (p[2] >= ymin & p[2] <= ymax)
    }
    test <- sapply(pts, fSel, xmin = mod@bbox$x[1], xmax = mod@bbox$x[2],
                              ymin = mod@bbox$y[1], ymax = mod@bbox$y[2])
    return( unname(pts[test]))
  }
)

# remove NULL from a list!!
.compactList <- function(x) Filter(Negate(is.null), x) 

.sectionEllipsoid <- function(x, l, pref = NULL){
  ob <- x
  RC <- RConics::ellipseToConicMatrix(saxes = c(ob["a"], ob["b"]),
                                      loc = ob[c("x","y")], 
                                      theta = ob["theta"])
  p0 <- RConics::intersectConicLine(RC , l) 
  if(length(p0) >0){
    pp <- t(p0)
    # line direction z
    newLoc = as.vector(diff((pp))/2 + pp[1,])
    # points(t(newLoc))
    x0y0 <- ob[c("x","y")]
    if( all(abs(newLoc - c(x0y0,1)) < .Machine$double.eps^0.5)){
      # if the section pass trough the ellipsoid center
      b_new <- as.numeric(ob["c"])
    }else{
      RC <- RConics::ellipseToConicMatrix( saxes = ob[c("a","b")], 
                                          loc = x0y0,
                                          theta = ob["theta"])
      # conicPlot(RC,asp=1,ylim=c(-30,30), xlim=c(-30,30))
      l_a_z <-RConics::join(newLoc, c(x0y0,1))
      # addLine(l_a_z,col="red")
      p_a_z <- RConics::intersectConicLine(RC , l_a_z) 
      # points(t(p_a_z),pch=20,col="blue")
      a_z <- sqrt(sum((p_a_z[1:2,1] - x0y0)^2))
      # distance O-newLoc 
      xy = sqrt(sum((newLoc[1:2] -  x0y0)^2))
      # definition of a new ellipse in the plan O - newLoc oriented toward z
      C2 = matrix(c(1/a_z^2, 0 ,0, 0, 1/ob["c"]^2, 0, 0,0,-1),
                  nrow = 3, byrow = TRUE)
      # conicPlot(C2,asp=1,ylim=c(-30,30), xlim=c(-30,30))
      l2 = c(1,   0, -xy) # Line going through newLoc with z-direction
      # addLine(l2,col="red")
      pp2 <- RConics::intersectConicLine(C2 , l2) 
      # points(t(pp2[1:2,]),pch=20,col="green")
      # ellipse parameters corresponding to the intersection between the
      # ellipsoïd and the plan define by the the line l and the z-direction
      b_new = as.numeric(abs(pp2[2,1]))
    }
    if((ob["z"] - b_new) >= ob["zmax"]){
      return(NULL)
    }else{
      a_new = sqrt(sum(( diff(pp))^2))/2   # apparent length of the object
      if(is.null(pref)){
        # center_xsection = null point on the cross-section = 
        # intersection cross-section with x=0
        # center_xsection <- c(0,-l[3]/l[2])
        # Projection des points sur la ligne
        myloc <- ifelse(l[1] != 0 && l[2] != 0, 
                        -sign(l[1])*sign(l[2]) *
                        sqrt(sum((newLoc[1:2] - c(-l[3]/l[1],0))^2)),
                        newLoc[l == 0][1])
      }else{
        myloc <- ifelse(l[1] != 0 && l[2] != 0, 
                        #-sign(l[1])*sign(l[2]) *
                        sqrt(sum((newLoc[1:2] - pref[1:2])^2)),
                        newLoc[l == 0][1])
      }
      return(c(ob, "xap" = myloc,     "aap" = a_new, "bap" = b_new, 
                   "x0"  = newLoc[1], "y0"  = newLoc[2]))
    }
  }else{
    return(NULL)
  }
}

##--------------------------- PLOT SECTION ----------------------##
setMethod("plotSection", "NULL", function(x, add = FALSE, xlab = "x",
            ylab = "y", main = "", asp = NA, ...){
    warnings("NULL")
  }
)

setMethod("plotSection", "TrEllipse", function(x, add = FALSE, xlab = "x",
            ylab = "y", main = "", asp = NA, ...){
      plotObj(x, add = add, xlab = xlab,
                ylab = ylab, main = main, asp = asp, ...)
  }
)

setMethod("plotSection", "Trough2D", function(x, add = FALSE, xlab = "x",
            ylab = "y", main = "", asp = NA, ...){
      plotObj(x, add = add, xlab = xlab,
                ylab = ylab, main = main, asp = asp, ...)
  }
)

setMethod("plotSection", "Deposits2D", function(x, add = FALSE, xlab = "x",
            ylab = "y", main = "", asp = NA, lay = NULL, xaxs = "i", 
            yaxs = "i", ...){
    if(add == FALSE){
      b <- boundary(x@troughs)
      dots <- list(...)
      if(!is.null(dots$xlim)){
        xlim <- dots$xlim
      }else{
        xlim <- b[c("xmin", "xmax")]
      }
      if(!is.null(dots$ylim)){
        ylim <- dots$ylim
      }else{
        ylim <- b[c("ymin", "ymax")]
      }
      plot(0,0, type = "n", xlab = xlab, ylab = ylab, main = main,
            xlim = xlim, ylim = ylim,
            asp = asp, xaxs = xaxs, yaxs = yaxs)
    }
#         plot(0,0, type = "n", xlab = xlab, ylab = ylab, main = main,
#               xlim = b[c("xmin", "xmax")], ylim = b[c("ymin", "ymax")],
#               asp = asp, xaxs = xaxs, yaxs = yaxs)
#       }
      # plot layers
    if(!is.null(lay)){
      lay$h <- x@layers
      do.call(abline, lay)
    }else{
      abline(h = x@layers )
    }
    # plot troughs
    plotSection(x@troughs, add = TRUE, ...)
  }
)

.plotSectionTrough2D <- function(){
  
}

##-------------------------------- FILLING --------------------------##
setMethod("crossBedding", "Deposits", function(x, prior = NULL){
#     n <- length(x@troughs@id)
#     if(is.null(prior)){
#       nF   <- rep(6, n)
#       rpos <- rep(0.75, n)
#       phi  <- rep(2.2, n)
#     }else{
#       nF <- round(W / .rsim(prior$nF, n)) +1
#       rpos <- .rsim(prior$rpos, n)
#       phi  <- .rsim(prior$phi, n)
#     }
#     xbed <- list()
#     for( i in seq_len(n)){
#       xbed[[x@troughs@id[i]]] <- .regCrossBedding(x@troughs[[i]], nF = nF[i],
#                                              rpos = rpos[i], phi = phi[i])
#     }
    x@troughs <- crossBedding(x@troughs, prior)
    return(x)
  }
)

# setMethod("crossBedding","Trough",function(x,nF=6, phi=1.05, rpos=1){
setMethod("crossBedding", "Trough", function(x, prior){
    n <- length(x@id)
    if(is.null(prior)){
      nF   <- rep(6, n)
      rpos <- rep(0.75, n)
      phi  <- rep(2.2, n)
    }else{
      nF <- round(x@W / .rsim(prior$nF, n)) +1
      rpos <- .rsim(prior$rpos, n)
      phi  <- .rsim(prior$phi, n)
    }
    xbed <- list()
    for( i in seq_len(n)){
      xbed[[x@id[i]]] <- .regCrossBedding(x[[i]], nF = nF[i],
                                             rpos = rpos[i], phi = phi[i])
    }
    x@fill <- xbed
    return(x)
  }
)


# nF number of foresets
# rpos = position relative from the ellipse center
# phi = angle to the ellispe length axis
.regCrossBedding <- function(x, nF = 6, rpos = 0.75, phi = 2.2){
  phi <- x@theta + phi  
  # compute Dr = radius smallest fill
  Dr <- x@L/2/(nF + 1)
  DH <- x@H/(nF + 1)
  # fillings length
  rF <- rev(cumsum(rep(2 * Dr, nF))/2)
  # available length
  oL2 <- (x@L/2 - tail(rF, 1))
  # fillings positions
  xy0 <- oL2 * rpos * c(cos(phi), sin(phi))
  xF <- seq(0, xy0[1], length = nF + 1)[-1]
  yF <- seq(0, xy0[2], length = nF + 1)[-1]
  xyF <- cbind(xF, yF)
  rot <- matrix(c( cos(x@theta), sin(x@theta), 
                  -sin(x@theta), cos(x@theta)),
                   byrow = TRUE, nrow = 2,ncol = 2)
  scl <- x@W / x@L
  xyFRot <- xyF[,1:2, drop = FALSE] %*% t(rot)
  xyFScl <- xyFRot
  xyFScl[,2] <- xyFScl[,2] * scl
  xyFSclRot <- xyFScl %*% (rot)
  oF <- new("Trough",
            version="0.1",
            id = seq_along(rF),
            pos = cbind(xyFSclRot, 0) + 
                  matrix(x@pos, nrow=nF, ncol = 3, byrow = TRUE),
            L = rF * 2,
            W = rF * 2 * x@W / x@L,
            H = rev(cumsum(rep(DH, nF))),
            theta = rep(x@theta, nF),  # depth position
            rH = rep(x@rH, nF)
           )
  return(oF)
}



##----------------------------- SIMULATION --------------------------##

#' Simulate
#'
#' Simulate coarse, braided river deposits
#' @export
sim <- function(modbox, hmodel = c("poisson", "strauss"), prior, 
                crossbeds = TRUE){
  hmodel <- match.arg(tolower(hmodel), c("poisson", "strauss"))
  #--- 1. vertical distribution layers: Poisson process
  dz <- diff(modbox$z)
  lambdaz <- dz/prior$ag
  nZ <- rpois(1, lambdaz)
  zLevel <- sort(modbox$z[1] + dz*runif(nZ))
  #--- 2. horizontal distribution scour fill: Poisson|Strauss model
  if(hmodel == "poisson"){
    # number of objects is Poisson distributed
    meanNObjects <- prior$lambda * diff(modbox$x) * diff(modbox$y)
    nPois <- rpois(nZ, meanNObjects)
    # total number of object
    n <-  sum(nPois)
    # length
    L   <- .rsim(prior$L, n)
    rLW <- .rsim(prior$rLW, n)
    rLH <- .rsim(prior$rLH, n)
    W <- L/rLW
    # position
    maxL <- max(L, W)
    xyz <- matrix(c(runif(n, min = modbox$x[1] - maxL, 
                             max = modbox$x[2] + maxL),
                    runif(n, min = modbox$y[1] - maxL, 
                             max = modbox$y[2] + maxL),
                    rep(zLevel, nPois )), byrow = FALSE, ncol = 3)
  }else if(hmodel == "strauss"){
    L   <- .rsim(prior$L,   n = 500)
    rLW <- .rsim(prior$rLW, n = 500)
    rLH <- .rsim(prior$rLH, n = 500)
    W <- L/rLW
    # position
    maxL <- ceiling(max(L, W)*1.5)
    modbox2 <- list(x = c(modbox$x[1] - 2 * prior$d - maxL,
                          modbox$x[2] + 2 * prior$d + maxL),
                    y = c(modbox$y[1] - 2 * prior$d - maxL,
                          modbox$y[2] + 2 * prior$d + maxL))
    XL <- replicate(nZ, straussMH(bet = prior$bet, gam = prior$gam, 
                                  d   = prior$d,   nit = prior$nit, 
                                  n0  = prior$n0,  W = modbox2, fd = prior$fd) )
    Xmat <- do.call(rbind, XL)
    nStrauss <- sapply(XL, nrow)
    n <- nrow(Xmat)
    xyz <- matrix(nrow = n, ncol = 3)
    xyz[,1:2] <- Xmat
    xyz[,3] <- rep(zLevel, nStrauss )
    L   <- .rsim(prior$L, n)
    rLW <- .rsim(prior$rLW, n)
    rLH <- .rsim(prior$rLH, n)
    W   <- L/rLW
  }
  trgh <- new("Trough",
              version = "0.1",
              id      = seq_len(n),
              pos     = xyz,
              L       = L,
              W       = W,
              H       = L/rLH,
              theta   = .rsim(prior$theta, n),  # depth position
              rH      = rep(prior$rH, n)
            )
  if(hmodel == "strauss"){
    trgh <- extract(trgh, modbox)
  }
  #--- 3. CROSS-BEDS
  if(isTRUE(crossbeds)){
    nF <- round(W / .rsim(prior$nF, n)) +1
    rpos <- .rsim(prior$rpos, n)
    phi  <- .rsim(prior$phi, n)
    xbed <- list()
    for( i in seq_len(n)){
      xbed[[trgh@id[i]]] <- .regCrossBedding(trgh[[i]], nF = nF[i],
                                             rpos = rpos[i], phi = phi[i])
    }
    trgh@fill <- xbed
  }
  new("Deposits",
      version = "0.1",
      troughs = trgh,
      layers  = zLevel,
      bbox = modbox
     )
}

#' @export
.rsim <- function(x, n=1){
  arg <- x[-1]
  arg[["n"]] <- n
  do.call(x$type, arg)
}





##--------------------------- POINT PROCESS ----------------------##
#' Strauss process simulation (MCMC)
#'
#' strauss process: bet^(n(y)) * gam^(s(y))
#' with 0 <= gam <= 1 and bet > 0
#' if gam = 1, Strauss process = Poisson process
#' if gam = 0, Strauss process = Hard core process
#' @param count boolean TRUE: return the number of points for each iteration
#' @export
straussMH <- function(bet = 10, gam = 0.5, d = 0.1, n0 = 10, nit = 5000,
                      W = list(x = c(0, 1), y = c(0, 1)), fd = NULL,
                      count = FALSE){
  # initialisation
  if(gam < 0 || gam > 1) stop("gam must be >= 0 and <= 0!\n")
  if(is.null(fd)) fd <- c(1,1)
  xmax  <- diff(W$x)/fd[1]
  ymax  <- diff(W$y)/fd[2]
  X     <- matrix(ncol = 2, nrow = n0)
  X[,1] <- runif(n0, 0, xmax)
  X[,2] <- runif(n0, 0, ymax)
  nv     <- integer(nit)
  i     <- 0
  n <- n0
  while(i < nit){
    i <- i + 1
    nv[i] <- n
    #if(nv[i] != nrow(X)) stop()
    #n[i] <- nrow(X)
    # BIRTH
    if(n <= 1 || sample(c(TRUE,FALSE), 1 )){
      x_cand <- c(runif(1, 0, xmax), runif(1, 0, ymax))
      phi <- sum(distxtoX(X, x_cand) <= d)
      pp <- bet * gam^phi / (n + 1)
      if(runif(1) <= min(1, pp)){
          #X <- X[c(seq_len(n), n), ]
          #X[n + 1, ] <- x_cand
          X <- rbind(X, x_cand, deparse.level = 0)
          n <- n + 1
      }
    # DEATH
    }else{
      x_cand_pos <- sample(seq_along(X[,1]),1)
      phi <- sum(distxtoX(X[-x_cand_pos, , drop=FALSE], 
                          X[ x_cand_pos, , drop=FALSE])  <= d)
      pm <- n * gam^phi / bet
      if(runif(1) <= min(1, pm)){
          X <- X[-x_cand_pos,,drop=FALSE]
          n <- n - 1
      }
    }
  }
  X[,1] <- W$x[1] + (X[,1]) * fd[1]
  X[,2] <- W$y[1] + (X[,2]) * fd[2]
  if( isTRUE(count) ){
    return( list("X" = X,"n" = nv) )
  }else{
    return( X )
  }
}

distxtoX <- function(X,x){
    sqrt(rowSums(sweep(X,2,x,'-')^2))
}




##--------------------------- HELPER FUNCTIONS ----------------------##
# example:
# pts <- locator(type="p",n=2)
# l_pts <- joinLine(pts)  # line joining the two points
# RConics::addLine(l_pts, col="red")
#' @export
joinLine <- function(pts){
  return(RConics::join(c(pts$x[1], pts$y[1] , 1),c(pts$x[2], pts$y[2] , 1)))
}

#----------- PROJECTION MATRIX -----------#
# Orthogonal Projection matrix on a Line
.matOP <- function(l){
  v <- c(1, -l[1]/l[2])
  return(matrix(c(v[1]^2, v[1]*v[2], v[1]*v[2], v[2]^2),
          nrow=2,ncol=2)/(v[1]^2+v[2]^2))
}
#' @export
measureDistance <- function(last=TRUE){
  A <-locator()
  loc <- cbind(A$x,A$y)
  return(.myDist(loc,last=last))
}

.myDist <-function(loc,last=FALSE){
  loc <- as.matrix(loc)
  all_dist <- cumsum(c(0,sqrt(rowSums(diff(loc)^2))))
  if(last){
  return(all_dist[length(all_dist)])
  }else{
  return(as.numeric(all_dist))
  
  }
}



