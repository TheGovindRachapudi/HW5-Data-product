## HW5_Class.R

## Define the sparse_numeric class
setClass(
  Class = "sparse_numeric",
  slots = c(
    value = "numeric",
    pos = "integer",
    length = "integer"
  )
)

## -----------------------------
## VALIDITY METHOD
## -----------------------------
setValidity("sparse_numeric", function(object) {
  if (length(object@value) != length(object@pos)) {
    return("Length of value and pos must match")
  }
  if (any(object@pos > object@length) || any(object@pos <= 0)) {
    return("Positions must be within the vector length and positive")
  }
  if (anyDuplicated(object@pos)) {
    return("Duplicate positions not allowed")
  }
  TRUE
})

## -----------------------------
## COERCION METHODS
## -----------------------------

## numeric -> sparse_numeric
setAs("numeric", "sparse_numeric", function(from) {
  nz <- which(from != 0)
  new("sparse_numeric",
      value = from[nz],
      pos = as.integer(nz),
      length = as.integer(length(from)))
})

## sparse_numeric -> numeric
setAs("sparse_numeric", "numeric", function(from) {
  x <- numeric(from@length)
  x[from@pos] <- from@value
  x
})

## -----------------------------
## SHOW METHOD
## -----------------------------
setMethod("show", "sparse_numeric", function(object) {
  cat("An object of class 'sparse_numeric'\n")
  cat("Length:", object@length, "\n")
  cat("Nonzero elements:", length(object@value), "\n")
  if (length(object@value) > 0) {
    df <- data.frame(position = object@pos, value = object@value)
    print(df)
  }
})

## -----------------------------
## PLOT METHOD
## -----------------------------
setMethod("plot", c("sparse_numeric", "sparse_numeric"), function(x, y, ...) {
  plot(x@pos, x@value, col = "blue", pch = 19,
       xlab = "Position", ylab = "Value", main = "Sparse Numeric Plot", ...)
  points(y@pos, y@value, col = "red", pch = 19)
  legend("topright", legend = c("x", "y"), col = c("blue", "red"), pch = 19)
})

## -----------------------------
## GENERIC FUNCTIONS
## -----------------------------
setGeneric("sparse_add", function(x, y, ...) standardGeneric("sparse_add"))
setGeneric("sparse_sub", function(x, y, ...) standardGeneric("sparse_sub"))
setGeneric("sparse_mult", function(x, y, ...) standardGeneric("sparse_mult"))
setGeneric("sparse_crossprod", function(x, y, ...) standardGeneric("sparse_crossprod"))

## -----------------------------
## HELPER FUNCTION
## -----------------------------
combine_sparse <- function(x, y, op) {
  if (x@length != y@length) stop("Sparse vectors must have same length")
  
  all_pos <- sort(unique(c(x@pos, y@pos)))
  xv <- numeric(length(all_pos))
  yv <- numeric(length(all_pos))
  
  match_x <- match(all_pos, x@pos)
  match_y <- match(all_pos, y@pos)
  
  xv[!is.na(match_x)] <- x@value[match_x[!is.na(match_x)]]
  yv[!is.na(match_y)] <- y@value[match_y[!is.na(match_y)]]
  
  result <- switch(op,
                   "+" = xv + yv,
                   "-" = xv - yv,
                   "*" = xv * yv)
  
  nz <- which(result != 0)
  new("sparse_numeric",
      value = result[nz],
      pos = as.integer(all_pos[nz]),
      length = x@length)
}

## -----------------------------
## METHODS IMPLEMENTATION
## -----------------------------
setMethod("sparse_add", c("sparse_numeric", "sparse_numeric"), function(x, y) {
  combine_sparse(x, y, "+")
})

setMethod("sparse_sub", c("sparse_numeric", "sparse_numeric"), function(x, y) {
  combine_sparse(x, y, "-")
})

setMethod("sparse_mult", c("sparse_numeric", "sparse_numeric"), function(x, y) {
  combine_sparse(x, y, "*")
})

setMethod("sparse_crossprod", c("sparse_numeric", "sparse_numeric"), function(x, y) {
  if (x@length != y@length) stop("Sparse vectors must have same length")
  common <- intersect(x@pos, y@pos)
  if (length(common) == 0) return(0)
  sum(x@value[match(common, x@pos)] * y@value[match(common, y@pos)])
})

## -----------------------------
## OPERATOR OVERLOADING
## -----------------------------
setMethod("+", c("sparse_numeric", "sparse_numeric"), function(e1, e2) {
  sparse_add(e1, e2)
})

setMethod("-", c("sparse_numeric", "sparse_numeric"), function(e1, e2) {
  sparse_sub(e1, e2)
})

setMethod("*", c("sparse_numeric", "sparse_numeric"), function(e1, e2) {
  sparse_mult(e1, e2)
})

## -----------------------------
## ADDITIONAL METHOD OF CHOICE
## -----------------------------
## Example: norm() method to compute vector magnitude
setGeneric("sparse_norm", function(x, ...) standardGeneric("sparse_norm"))
setMethod("sparse_norm", "sparse_numeric", function(x) {
  sqrt(sum(x@value^2))
})


