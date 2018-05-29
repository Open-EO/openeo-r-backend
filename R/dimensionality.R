# dimensionality ----
#' @export
create_dimensionality = function(space=NULL,time=NULL,band=NULL,raster=NULL,feature=NULL) {
  dimensionality = list(
    space = .arg_logical(space),
    time = .arg_logical(time),
    band = .arg_logical(band),
    raster = .arg_logical(raster),
    feature = .arg_logical(feature)
  )
  class(dimensionality) <- "Dimensionality"
  return(dimensionality)
}

.arg_logical = function(val) {
  if (is.null(val)) return(FALSE)
  
  log = as.logical(val)
  if (is.na(log)) return(FALSE)
  
  return(log)
}

#' @export
format.Dimensionality = function(x, ...) {
  n = names(x)
  val = as.integer(unlist(x))
  return(paste(n,val, sep=":",collapse = " "))
}

#' @export
code = function(x, ...) {
  UseMethod("code",x)
}

#' @export
code.Dimensionality = function(x, ...) {
  return(paste(as.integer(x),sep="",collapse = ""))
}

#' @export
code.Collection = function(x, ...) {
  return(code(x$dimensions))
}


# modifier ----
#' @export
create_dimensionality_modifier = function(add=NULL, remove=NULL) {
  modifier = list(
    add_dimension = create_dimensionality(),
    remove_dimension = create_dimensionality()
  )
  
  # set the dimensions that are added
  if (!is.null(add) && length(add) > 0) { # only try to set added dimensions if parameter is not null
    if (!is.null(names(add))) { # case 1: add list is a named list
      # check if the names match with modifier$add_dimension
      selector = names(add) %in% names(modifier$add_dimension)
      if (any(selector)) {
        add = add[selector]
      } else {
        stop("Cannot find any added dimensions in allowed dimensions")
      }
      
      for (index in names(add)) {
        value = add[[index]]
        if (!is.null(value) && !is.na(value) && is.logical(value) && value) {
          modifier$add_dimension[[index]] = add[[index]]
        }
      }
    } else if (length(add) == length(modifier$add_dimension)) {
      for (index in 1:length(add)) {
        value = add[[index]]
        if (!is.null(value) && !is.na(value) && is.logical(value) && value) {
          modifier$add_dimensions[[index]] = add[[index]]
        }
      }
    } else {
      stop("Cannot match any dimension that are to be added")
    }
  }
 
  # set the removed dimensions
  if (!is.null(remove) && length(remove) > 0) { 
    if (!is.null(names(remove))) {
      # check if the names match with modifier$remove_dimension
      selector = names(remove) %in% names(modifier$remove_dimension)
      
      if (any(selector)) {
        remove = remove[selector]
      } else {
        stop("Cannot find any removed dimensions in allowed dimensions")
      }
      
      for (index in names(remove)) {
        value = remove[[index]]
        if (!is.null(value) && !is.na(value) && is.logical(value) && value) {
          modifier$remove_dimension[[index]] = remove[[index]]
        }
      }
    }else if (length(remove) == length(modifier$remove_dimension) && length(remove) > 0) {
      
      for (index in 1:length(remove)) {
        value = remove[[index]]
        
        if (!is.null(value) && !is.na(value) && is.logical(value) && value) {
          modifier$remove_dimensions[[index]] = value
        }
        
      }
    } else {
      stop("Cannot match any dimension that are to be removed")
    }
  }  
  
  # also check that add and remove don't have the same positive dimensions (dimension will be added and removed ?!)
  add_test = modifier$add_dimension[unlist(modifier$add_dimension)]
  
  for (name in names(add_test)) {
    if (modifier$remove_dimension[[name]]) {
      stop("Trying to add and remove a dimension at the same time")
    }
  }
  
  class(modifier) = "DimensionalityModifier"
  
  return(modifier)
}

#' @export
dim.apply = function(x,y) {
  if (class(x) != "Dimensionality") stop("x is no Dimensionality object")
  if (class(y) != "DimensionalityModifier") stop("y is no DimensionalityModifier object")
  
  # apply added dimension
  add_dimensions = y$add_dimension[unlist(y$add_dimension)] #selects all dimensions that shall be modified to TRUE
  for (name in names(add_dimensions)) {
    if (!x[[name]]) {
      # case: dimension was not present before and shall be added
      x[[name]] = TRUE
    }
  }
  
  # apply remove dimension
  remove_dimensions = y$remove_dimension[unlist(y$remove_dimension)] #selects all dimensions that shall be modified to TRUE
  for (name in names(remove_dimensions)) {
    if (x[[name]]) {
      x[[name]] = FALSE
    } else {
      stop("Trying to remove a dimension that is not existing")
    }
  }
  
  return(x)
}
