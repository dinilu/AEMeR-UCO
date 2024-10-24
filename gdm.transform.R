# devtools::install_version("gdm", "1.5.0-3")

gdm.transform <- function (model, data, filename = "", ...) {
    # model <- gdmRast.fit
    # data <- bioclim
    # filename = ""
    options(warn.FPU = FALSE)
    if (!is(model, "gdm")) {
      stop("model object must be of class 'gdm'.")
    }
    if (!(gdm:::.is_raster(data) | is(data, "data.frame"))) {
      stop("Data to be transformed must either be a raster object or data frame.")
    }
    if (!is(data, "data.frame")) {
      gdm:::.check_pkgs("terra")
      data <- gdm:::.check_rast(data, "data")
    }
    geo <- model$geo
    if (gdm:::.is_raster(data) && geo) {
      x <- terra::init(data[[1]], fun = "x")
      y <- terra::init(data[[1]], fun = "y")
      data <- c(stats::setNames(x, "xCoord"),
                stats::setNames(y, "yCoord"), 
                data)
    }
    gdm_trans <- function(mod, dat, ...) {
      nr <- nrow(dat)
      nc <- ncol(dat)
      z <- .C("GDM_TransformFromTable", as.integer(nr), as.integer(nc), 
              as.integer(mod$geo), as.integer(length(mod$predictors)), 
              as.integer(mod$splines), as.double(mod$knots), as.double(mod$coefficients), 
              as.matrix(dat), trandata = as.double(matrix(0, nr, 
                                                          nc)), PACKAGE = "gdm")
      transformed <- matrix(z$trandata, nrow = nr, byrow = FALSE)
      return(transformed)
    }
    if (gdm:::.is_raster(data)) {
      output <- terra::predict(object = data, model = model, 
                               fun = gdm_trans, na.rm = TRUE, filename = filename)
      splineindex <- 1
      predInd <- c()
      for (i in 1:length(model$predictors)) {
        numsplines <- model$splines[i]
        coeff_sum <- sum(model$coefficients[splineindex:(splineindex + 
                                                           numsplines - 1)])
        if (coeff_sum > 0) {
          predInd <- c(predInd, i)
        }
        splineindex <- splineindex + numsplines
      }
      if (geo) {
        if(sum(model$coefficients[1:model$splines[1]]) == 0){
          predInd <- c(1, 2, predInd + 1)
        } else {
          predInd <- c(1, 2, predInd[-1] + 1)
        }
      }
      output <- terra::subset(output, predInd)
      return(output)
    } else {
      output <- gdm_trans(mod = model, dat = data)
      colnames(output) <- colnames(data)
      return(output)
    }
  }