# helpful spatial plotting functions using base R and fields:

# continuous or gridded plotting ----

#' plot points spatially with colors
#' 
#' Column major ordering, row and col start at index 1
#' 
#' @param x Horizontal spatial coordinates
#' @param y Vertical spatial coordinates
#' @param z Response. Determines color of the points
#' @param zlim Range of the response
#' @param col A vector of colors determining the color scale for plotting (it is
#' recommended to use the `colScale` argument instead)
#' @param nlevel Number of colors to include in generated color scale if `col` 
#' not specified
#' @param colScale A vector of colors determining the color scale for plotting
#' @param center Value at which to center the color scale if `centerScale` set 
#' to TRUE
#' @param centerScale Whether to center the color scale at `center` (e.g. for 
#' diverging color scales)
#' @param compressed Rather than cutting off a color scale to make the center at
#' the desired point, compress the scale on the shorter side so it changes color
#' faster.
#' @param legend.mar (as legend.args) see \code{\link{fields::imagePlot}}
#' @param legend.width (as legend.args) see \code{\link{fields::imagePlot}}
#' @param legendArgs Other arguments to legend.args. See 
#' @param new Whether to make a new plot or add to the current one
#' @param n.ticks Approximate number of ticks in color scale. See 
#' \code{\link{pretty}}
#' @param min.n Minimum number of ticks in color scale. See 
#' \code{\link{pretty}}
#' @param scaleFun How to scale the color scale. For example, log
#' @param scaleFunInverse Inverse of `scaleFun`. For example, exp
#' @param addColorBar Whether to add the color bar/legend
#' @param leaveRoomForLegend Whether to leave room for the color bar/legend
#' @param forceColorsInRange Whether or not to force colors in the plotted 
#' range. Useful if you have a value outside of the range or that is NA after 
#' being transformed via the scale that you still want to plot at the edge of 
#' the color scale
#' @param pch see \code{\link{graphics::points}}
#' @param colorName What plotting parameter controls the color of the points. 
#' @param resetGraphics Whether or not to call graphics::par()
#' @param ... Arguments to the plot function. For example, if `pch==19`, set to 
#' 'bg', but if `pch==1`, set to 'col'.
#' \code{\link{graphics::plot.default}}
#' @details
#' A wrapper around fields::imagePlot for easy plotting of points with color 
#' and easy use of nonlinear color scales in base R.
#' 
#' @author John Paige
#' @seealso \code{\link{fields::imagePlot}}, 
#' \code{\link{graphics::plot.default}}
#' @examples 
#' set.seed(123)
#' x = matrix(rnorm(100), ncol=2)
#' z = rnorm(50)
#' splot(x, z, colScale=redBlueDivCols, centerScale=TRUE)
#' @export
#' @importFrom fields imagePlot
#' @importFrom graphics plot.default
#' @importFrom graphics points.default
#' @importFrom graphics par
splot = function(x, y, z, zlim=NULL, col=NULL, nlevel=64, 
                 colScale=blueGreenYellowSeqCols, 
                 center=0, centerScale=FALSE, compressed=FALSE, 
                 legend.mar=7, new=TRUE, scaleFun=I, 
                 n.ticks=5, min.n=5, ticks=NULL, tickLabels=NULL, legend.width=1.2, addColorBar=TRUE, 
                 legendArgs=list(), leaveRoomForLegend=TRUE, forceColorsInRange=FALSE, 
                 pch=19, colorName=c("col", "bg"), resetGraphics=TRUE, ...) {
  colorName = match.arg(colorName)
  
  # remove NA points
  nas = is.na(x) | is.na(y) | is.na(z)
  if(any(nas)) {
    warning("Removing NAs")
    x = x[!nas]
    y = y[!nas]
    z = z[!nas]
  }
  
  # do setup for plotting data if necessary
  if(is.null(zlim)) {
    nas = !is.finite(scaleFun(z))
    zlim = range(z[!nas])
  }
  
  if(is.null(col)) {
    if(!centerScale) {
      col = colScale(n=nlevel)
    }
    else {
      col = centerColorScale(nlevel, zlim, center=center, colScale=colScale, scaleFun=scaleFun)
    }
  }
  
  # get colors of points
  cols = getColorsFromScale(z, zlim, cols=col, scaleFun=scaleFun, 
                            forceValuesInRange=forceColorsInRange)
  
  # generate new plot if necessary
  if(new) {
    # set graphical parameters so the legend won't overlap with plot
    currPar = graphics::par()
    newPar = currPar
    newMar = newPar$mar
    newMar[4] = max(newMar[4], legend.mar)
    newPar$mar = newMar
    if((currPar$mar[4] != newMar[4]) && resetGraphics)
      suppressWarnings({graphics::par(newPar)})
    
    if(colorName == "col") {
      do.call("plot", c(list(x=x, y=y, col=cols, pch=pch), list(...)))
    } else {
      do.call("plot", c(list(x=x, y=y, bg=cols, pch=pch), list(...)))
    }
  } else {
    if(colorName == "col") {
      do.call("points", c(list(x=x, y=y, col=cols, pch=pch), list(...)))
    } else {
      do.call("points", c(list(x=x, y=y, bg=cols, pch=pch), list(...)))
    }
  }
  
  if(addColorBar) {
    # add legend
    if(is.null(tickLabels))
      setTickLabels = TRUE
    
    if(is.null(ticks)) {
      if(setTickLabels)
        tickLabels = pretty(zlim, n=n.ticks, min.n=min.n)
      ticks = scaleFun(tickLabels)
    }
    else {
      if(setTickLabels)
        tickLabels = ticks
      ticks = scaleFun(ticks)
    }
    if(setTickLabels)
      tickLabels = tickLabels[is.finite(ticks)]
    ticks = ticks[is.finite(ticks)]
    
    # set list of arguments to imagePlot
    
    legendArgs$zlim=scaleFun(zlim)
    legendArgs$nlevel=length(col)
    legendArgs$legend.only=TRUE
    legendArgs$horizontal=FALSE
    legendArgs$col=col
    legendArgs$add = TRUE
    if(is.null(legendArgs$axis.args))
      legendArgs$axis.args=list(at=ticks, labels=tickLabels)
    else {
      if(is.null(legendArgs$axis.args$at)) {
        legendArgs$axis.args$at=ticks
      }
      if(is.null(legendArgs$axis.args$labels)) {
        legendArgs$axis.args$labels=tickLabels
      }
    }
    legendArgs$legend.mar=legend.mar
    legendArgs$legend.width=legend.width
    
    do.call("imagePlot", legendArgs)
    
  }
  invisible(NULL)
}

#' Scaled quilt plots
#' 
#' Wrapper around fields::quilt.plot for spatial quilt plots with easy automatic
#' color scaling
#' 
#' @param x Horizontal spatial coordinates, or 2d matrix with x-y coords
#' @param y Vertical spatial coordinates, or response is x is 2d matrix
#' @param z Response (prior to scaling), or null if x is 2d matrix. Determines 
#' color of the points
#' @param zlim range of the response (prior to scaling)
#' @param colScale a function for generating a color scale. See 
#' \code{?colorScales}
#' @param nlevel Number of colors to include in generated color scale if `col` 
#' not specified
#' @param center Value at which to center the color scale if `centerScale` set 
#' to TRUE
#' @param centerScale Whether to center the color scale at `center` (e.g. for 
#' diverging color scales)
#' @param compressed Rather than cutting off a color scale to make the center at
#' the desired point, compress the scale on the shorter side so it changes color
#' faster.
#' @param col a vector of colors determining the color scale for plotting 
#' (generally it is recommended to use the `colScale` argument instead)
#' @param scaleFun How to scale the color scale. For example, log
#' @param scaleFunInverse Inverse of `scaleFun`. For example, exp
#' @param forceColorsInRange Whether or not to force colors in the plotted 
#' range. Useful if you have a value outside of the range or that is NA after 
#' being transformed via the scale that you still want to plot at the edge of 
#' the color scale
#' @param new Whether to make a new plot or add to the current one
#' @param n.ticks Approximate number of ticks in color scale. See 
#' \code{\link{pretty}}
#' @param min.n Minimum number of ticks in color scale. See 
#' \code{\link{pretty}}
#' @param ticks Unscaled colorscale tick marks
#' @param tickLabels labels for ticks
#' @param otherAxis.args Arguments to pass to `fields::imagePlot` via 
#' `axis.args` (aside from `at` and `labels`)
#' @param ... Arguments to the `fields::imagePlot` and `fields::quilt.plot` 
#' functions.
#' @details
#' A wrapper around fields::quilt.plot for easy nonlinear color scales.
#' @returns Output from \code{\link{fields::quilt.plot}}
#' @author John Paige
#' @seealso \code{\link{fields::imagePlot}}, \code{\link{fields::quilt.plot}}, 
#' \code{\link{colorScales}}
#' @examples 
#' set.seed(123)
#' x = matrix(rnorm(100), ncol=2)
#' z = rnorm(50)
#' splot(x, z, colScale=redBlueDivCols, centerScale=TRUE)
#' @export
#' @importFrom fields imagePlot
#' @importFrom fields quilt.plot
squilt = function(x, y, z=NULL, zlim=NULL, col=NULL, 
                  colScale=blueGreenYellowSeqCols, 
                  nlevel=64, scaleFun=I, scaleFunInverse=I, 
                  center=0, centerScale=FALSE, compressed=FALSE, 
                  n.ticks=5, min.n=5, ticks=NULL, tickLabels=NULL, 
                  forceColorsInRange=FALSE, otherAxis.args=NULL, ...) {
  
  # set default x and y labels based on this function call
  dotList = list(...)
  if(!("xlab" %in% names(dotList))) {
    dotList = c(dotList, list(xlab=deparse(substitute(x))))
  }
  if(!("ylab" %in% names(dotList))) {
    dotList = c(dotList, list(ylab=deparse(substitute(y))))
  }
  
  # clean up input arguments
  if(!is.null(zlim) && any(!is.finite(scaleFun(zlim)))) {
    stop("scaleFun(zlim) not finite")
  }
  
  x <- as.matrix(x)
  if (ncol(x) == 2) {
    z <- y
  }
  if (ncol(x) == 1) {
    x <- cbind(x, y)
  }
  if (ncol(x) == 3) {
    z <- x[, 3]
    x <- x[, 1:2]
  }
  
  # do setup for plotting data if necessary
  if(is.null(zlim)) {
    nas = !is.finite(scaleFun(z))
    zlim = range(z[!nas])
  }
  
  if(forceColorsInRange) {
    z[z < zlim[1]] = zlim[1]
    z[z > zlim[2]] = zlim[2]
  }
  
  # set tick marks automatically if not done already
  if(is.null(ticks)) {
    ticks = scaleFun(pretty(z, n.ticks=n.ticks, min.n=min.n))
  }
  else {
    ticks = scaleFun(ticks)
  }
  
  if(is.null(tickLabels)) {
    tickLabels = as.character(scaleFunInverse(ticks))
  }
  
  # make the color scale, centered if necessary, accounting for the scaling fun
  if(is.null(col)) {
    if(centerScale) {
      col = centerColorScale(nlevel, vals=z, valRange=zlim, center=center, 
                             colScale=colScale, scaleFun=scaleFun, 
                             compressed=compressed)
    }
    else {
      col = colScale(nlevel)
    }
  }
  
  # scale the data and ticks, but plot with unscaled tick labels
  z = scaleFun(z)
  
  out = do.call(fields::quilt.plot, c(list(x[,1], x[,2], z, 
                                           zlim=scaleFun(zlim), col=col, 
                                           add.legend=FALSE), dotList))
  
  fields::imagePlot(zlim=scaleFun(zlim), nlevel=length(col), legend.only=TRUE, col=col, 
                    axis.args=c(list(at=ticks, labels=tickLabels), 
                                otherAxis.args),
                    ...)
  
  invisible(out)
}

# color scales ----

#' Gets colors from color scale
#' 
#' Gets colors of responses from a color scale, potentially based on scale 
#' transformations and with centering
#' 
#' @param n Number of colors to generate in the scale
#' @param rev Whether to reverse the color scale
#' @param ggplot If FALSE, returns a vector of `n` colors. Otherwise, returns a
#' color scale object for use in ggplot functions
#' @param valRange Range of the data to make the color scale based on
#' @param center Center of the range of responses set setting diverging color 
#' scales
#' @param cols A vector of colors forming color scale
#' @param colScale a function taking 'n' as input and returns a color scale. See
#' `?colorScales` for formatting
#' @details
#' A set of color scale generators. For other interesting color scales, 
#' including sequential, diverging, discrete, and color blind friendly scales, 
#' see colorspace::choose_palette().
#' 
#' @author John Paige
#' @examples 
#' set.seed(123)
#' x = rnorm(100)
#' col = redBlueDivCols(64, valRange=range(x), center=0)
#' splot(x, x, x, colScale=col)
#' splot(x, x, x, colScale=rainbow)
#' 
#' @export
#' @name colorScales
#' @seealso \code{\link{nonlinearScales}}, \code{\link{nonlinearTicks}}, 
#' \code{\link{colorScaleUtilities}}
#' @importFrom colorspace diverging_hcl
#' @importFrom colorspace scale_colour_continuous_diverging
purpleYellowSeqCols = function(n, rev=FALSE, ggplot=FALSE) {
  if(!ggplot)
    colorspace::sequential_hcl(n, h1=-100, h2=100, c1=60, cmax=74, c2=100, l1=15, l2=95, p1=2, p2=0.9, rev=rev)
  else {
    colorspace::scale_colour_continuous_sequential(h1=-100, h2=100, c1=60, cmax=74, c2=100, l1=15, l2=95, p1=2, p2=0.9, rev=rev, n_interp=n)
  }
}

#' @rdname colorScales
#' @export
#' @importFrom colorspace diverging_hcl
#' @importFrom colorspace scale_colour_continuous_diverging
redBlueDivCols = function(n, valRange=NULL, center=NULL, rev=FALSE, ggplot=FALSE) {
  if(is.null(valRange) && is.null(center)) {
    if(!ggplot)
      colorspace::diverging_hcl(n, h1=10, h2=-115, c1=90, l1=40, l2=100, p1=0.9, rev=rev)
    else
      colorspace::scale_colour_continuous_diverging(h1=10, h2=-115, c1=90, l1=40, l2=100, p1=0.9, rev=rev, n_interp=n)
  } else if(!is.null(valRange)) {
    if(is.null(center)) {
      center = mean(valRange)
    }
    
    # in this case we want white to be at the center of valRange if center is NULL
    if(!ggplot) {
      propUp = (valRange[2] - center) / diff(valRange)
      propDown = 1 - propUp
      totalColors = ceiling(2 * max(propUp, propDown) * n)
      tempColors = redBlueDivCols(totalColors, rev=rev)
      totalMissingColors = totalColors - n
      
      if(propUp >= propDown)
        tempColors[-(1:totalMissingColors)]
      else
        tempColors[1:n]
    } else {
      if(is.null(center))
        center = min(valRange) + abs(diff(valRange))/2
      colorspace::scale_colour_continuous_diverging(h1=10, h2=-115, c1=90, l1=40, l2=100, p1=0.9, rev=rev, n_interp=n, mid=center)
    }
  } else {
    stop("if center is provided, must also provide valRange")
  }
}

#' @rdname colorScales
#' @export
#' @importFrom colorspace diverging_hcl
#' @importFrom colorspace scale_colour_continuous_diverging
redGrayBlueDivCols = function(n, valRange=NULL, center=NULL, rev=FALSE, ggplot=FALSE) {
  if(is.null(valRange) && is.null(center)) {
    if(!ggplot)
      colorspace::diverging_hcl(n, h1=10, h2=-115, c1=90, l1=40, l2=90, p1=0.9, rev=rev)
    else
      colorspace::scale_colour_continuous_diverging(n_interp=n, h1=10, h2=-115, c1=90, l1=40, l2=90, p1=0.9, rev=rev)
    # diverging_hcl(n, h1=10, h2=-115, c1=90, l1=40, l2=100, p1=0.9, p2=0.6)
  }
  else {
    # in this case we want white to be at the center of valRange if center is NULL
    if(!ggplot) {
      propUp = (valRange[2] - center) / diff(valRange)
      propDown = 1 - propUp
      totalColors = ceiling(2 * max(propUp, propDown) * n)
      tempColors = redGrayBlueDivCols(totalColors, rev=rev)
      totalMissingColors = totalColors - n
      
      if(propUp >= propDown && totalMissingColors > 0)
        tempColors[-(1:totalMissingColors)]
      else
        tempColors[1:n]
    } else {
      if(is.null(center))
        center = min(valRange) + abs(diff(valRange))/2
      colorspace::scale_colour_continuous_diverging(n, h1=10, h2=-115, c1=90, l1=40, l2=90, p1=0.9, rev=rev, mid=center)
    }
  }
}

#' @rdname colorScales
#' @export
#' @importFrom colorspace sequential_hcl
#' @importFrom colorspace scale_colour_continuous_sequential
blueSeqCols = function(n, ggplot=FALSE) {
  if(!ggplot)
    colorspace::sequential_hcl(n, h1=245, c1=50, cmax=75, l1=20, l2=98, p1=0.8, rev=TRUE)
  else
    colorspace::scale_colour_continuous_sequential(h1=245, c1=50, cmax=75, l1=20, l2=98, p1=0.8, rev=TRUE, n_interp=n)
}

#' @rdname colorScales
#' @export
#' @importFrom colorspace sequential_hcl
#' @importFrom colorspace scale_colour_continuous_sequential
greenSeqCols = function(n, ggplot=FALSE, rev=FALSE) {
  if(!ggplot)
    colorspace::sequential_hcl(n, h1=128, c1=100, l1=72, l2=95, p1=1.0, rev=rev)
  else
    colorspace::scale_colour_continuous_sequential(h1=128, c1=100, l1=72, l2=95, p1=1.0, rev=rev, n_interp=n)
}

# based on viridis
#' @rdname colorScales
#' @export
#' @importFrom colorspace sequential_hcl
#' @importFrom colorspace scale_colour_continuous_sequential
blueGreenYellowSeqCols = function(n, ggplot=FALSE, rev=FALSE) {
  if(!ggplot)
    colorspace::sequential_hcl(n, h1=300, h2=75, c1=40, c2=95, l1=15, l2=90, p1=1.0, p2=1.1, rev=rev)
  else
    colorspace::scale_colour_continuous_sequential(h1=300, h2=75, c1=40, c2=95, l1=15, l2=90, p1=1.0, p2=1.1, n_interp=n, rev=rev)
}

#' @rdname colorScales
#' @export
#' @importFrom colorspace divergingx_hcl
#' @importFrom colorspace scale_colour_continuous_sequential
redYellowBlueCols = function(n, ggplot=FALSE) {
  if(!ggplot)
    colorspace::divergingx_hcl(n, palette="RdYlBu")
  else
    colorspace::scale_colour_continuous_sequential(palette="RdYlBu", n_interp=n)
}

# internal color scale utility functions ----
#' Internal functions for building color scales
#' 
#' Internal functions for building color scales
#' 
#' @param n Number of levels in the color scale
#' @param rev Whether or not to reverse the color scale
#' @param ggplot Whether to output a ggplot2-based color scale or based R color 
#' vector
#' @param ... Other arguments to the color scale
#' @details 
#' Internal functions for building color scales
#' @author John Paige
#' @examples 
#' # TODO
#' @name colorScaleInternals
#' @seealso \code{\link{nonlinearScales}}
# combine two color scale functions (that return vector of colors given number of colors), 
# given the number of colors in the scale desired
combineTwoScales = function(n, scale1, scale2, args1, args2) {
  if(n %% 2 == 0) {
    n1 = n2 = n/2
  } else {
    n1 = ceiling(n/2)
    n2 = floor(n/2)
  }
  
  c(do.call(scale1, c(args1, list(n=n1))), 
    do.call(scale2, c(args2, list(n=n2))))
}

# convert a single color sequential scale into a diverging scale
#' @rdname colorScaleInternals
makeDivScale = function(n, scale, ...) {
  do.call("combineTwoScales", list(n=n, scale1=scale, scale2=scale, args1=list(...), args2=list(...)))
}

#' @rdname colorScaleInternals
#' @importFrom grDevices col2rgb
#' @importFrom grDevices rgb
twoColorScale = function(n, col1=grDevices::rgb(.8,.8,.8), col2="green") {
  rgb1 = c(grDevices::col2rgb(col1))
  rgb2 = c(grDevices::col2rgb(col2))
  props = seq(0, 1, l=n)
  cols = sapply(props, function(x) {x*rgb2 + (1-x)*rgb1})
  cols[cols < 0] = 0
  cols[cols > 255] = 255
  colStrs = apply(cols, 2, function(x) {do.call("rgb", as.list(x/255))})
}

#' @rdname colorScaleInternals
#' @importFrom grDevices rgb
ThreeColorDivScale = function(n, vals=NULL, valRange=NULL, center=NULL, 
                              col1="green", col2=grDevices::rgb(.8,.8,.8), 
                              col3="blue") {
  if(!is.null(vals)) {
    valRange = range(vals)
  }
  if(is.null(center) && !is.null(valRange)) {
    center = mean(valRange)
  }
  
  if(is.null(valRange) && is.null(center)) {
    n1 = round(n/2)
    n2 = n - n1
  } else if(!is.null(valRange)) {
    
    # in this case we want white to be at the center of valRange if center is NULL
    propUp = (valRange[2] - center) / diff(valRange)
    propDown = 1 - propUp
    n1 = round(n * propDown)
    n2 = n - n1
  } else {
    stop("if center is provided, must also provide valRange")
  }
  
  c(twoColorScale(n1, col1, col2), twoColorScale(n2+1, col2, col3)[-1])
}

# external color scale utility functions ----

#' Gets colors from color scale
#' 
#' Gets colors of responses from a color scale, potentially based on scale 
#' transformations and with centering
#' 
#' @param vals Response. Determines colors from the scale
#' @param valRange Range of responses for setting the scale
#' @param center Center of the range of responses set setting diverging color 
#' scales
#' @param cols A vector of colors forming color scale
#' @param colScale a function taking 'n' as input and returns a color scale. See
#' `?colorScales` for formatting
#' @param scaleFun How to scale the color scale. For example, log
#' @param forceColorsInRange Whether or not to force colors in the plotted 
#' range. Useful if you have a value outside of the range or that is NA after 
#' being transformed via the scale that you still want to plot at the edge of 
#' the color scale
#' @param compressed Rather than cutting off a color scale to make the center at
#' the desired point, compress the scale on the shorter side so it changes color
#' faster.
#' @param ... additional arguments passed to `colScale`
#' @details 
#' Use `getColorsFromScale` when using a log, logit, or some other 
#' fancy transformation of a color scale. Given data to be assigned colors from 
#' scale (vals), the range of the values, the center of the value range (only 
#' for diverging scales), the color scale as a vector of colors (cols), the 
#' scale of the color scale (e.g. identity, log, logit), and whether or not to 
#' force the colors into valRange, `getColorsFromScale` returns the colors 
#' associated with each value in vals.
#' 
#' `centerColorScale` centers a color scale at its midpoint. Returns vector of 
#' the centered color scale. Useful when using diverging scales centered at 0 
#' for data with asymmetric range colScale a function taking 'n' as input and 
#' returns a color scale centered in the middle
#' @author John Paige
#' @examples 
#' 
#' set.seed(123)
#' 
#' # construct data:
#' testX = exp(stats::rnorm(100))
#' 
#' # construct centered color scale on log scale
#' test = centerColorScale(64, testX, center=1, colScale=redBlueDivCols, scaleFun=log)
#' 
#' # get the colors associated with each value
#' testCols = getColorsFromScale(testX, center=1, cols=test, scaleFun=log)
#' 
#' # plot the result
#' plot(testX, col=testCols, pch=19)
#' @export
#' @name colorScaleUtilities
#' @seealso \code{\link{nonlinearScales}}, \code{\link{nonlinearTicks}}, 
#' \code{\link{colorScales}}
getColorsFromScale = function(vals, valRange=NULL, center=NULL, cols, 
                              scaleFun=function(x) {x}, 
                              forceValuesInRange=FALSE) {
  
  if(is.null(valRange)) {
    nas = !is.finite(scaleFun(vals))
    valRange = range(vals[!nas])
  }
  
  if(forceValuesInRange) {
    vals[vals < valRange[1]] = valRange[1]
    vals[vals > valRange[2]] = valRange[2]
  }
  
  valRange = scaleFun(valRange)
  vals = scaleFun(vals)
  vals = vals - valRange[1]
  vals = vals/(valRange[2] - valRange[1])
  
  if(!is.null(center)) {
    center = scaleFun(center)
    n = length(cols)
    
    propUp = (valRange[2] - center) / diff(valRange)
    propDown = 1 - propUp
    totalColors = ceiling(2 * max(propUp, propDown) * n)
    tempColors = cols
    totalMissingColors = totalColors - n
    
    if(propUp >= propDown)
      tempColors[-(1:totalMissingColors)]
    else
      tempColors[1:n]
    
    cols = tempColors
  }
  
  col = cols[round(vals*(length(cols)-1))+1]
  
  col
}

#' @rdname colorScaleUtilities
#' @export
centerColorScale = function(n, vals=NULL, valRange=NULL, center, colScale, 
                            scaleFun=function(x) {x}, compressed=FALSE, ...) {
  if(is.null(valRange)) {
    nas = !is.finite(scaleFun(vals))
    valRange = range(vals[!nas])
  }
  
  valRange = scaleFun(valRange)
  center = scaleFun(center)
  
  propUp = (valRange[2] - center) / diff(valRange)
  propDown = 1 - propUp
  totalColors = ceiling(2 * max(propUp, propDown) * n)
  tempColors = do.call(colScale, c(list(totalColors), list(...)))
  totalMissingColors = totalColors - n
  
  if(!compressed) {
    if(propUp >= propDown && totalMissingColors > 0)
      tempColors[-(1:totalMissingColors)]
    else
      tempColors[1:n]
  } else {
    if(propUp >= propDown && totalMissingColors > 0) {
      lowIndices = round(seq(1, round(totalColors/2), l=round(totalColors/2)-totalMissingColors))
      highIndices = round(seq(round(totalColors/2)+1, totalColors, l=round(totalColors/2)))
      tempColors[c(lowIndices, highIndices)]
    }
    else {
      lowIndices = round(seq(1, round(totalColors/2), l=round(totalColors/2)))
      highIndices = round(seq(round(totalColors/2)+1, totalColors, l=round(totalColors/2)-totalMissingColors))
      tempColors[c(lowIndices, highIndices)]
    }
  }
}

# axis scale ticks ---- 
#' Generates pretty tick marks for nonlinear scales
#' 
#' Similar to `pretty`, but for nonlinear scale tick marks
#' 
#' @param x Data to base ticks from prior to scaling
#' @param nint Approximate desired number of ticks in each part of the scale. 
#' See `n` in ?pretty` and `nint` in `grDevices::axisTicks`
#' @param nintSmall Approximate desired number of ticks in the small 
#' magnitude/linear part of the scale
#' @param nintLarge Approximate desired number of ticks in the large 
#' magnitude/log part of the scale
#' @param smallThresh Threshold in magnitude dividing the small/linear and 
#' large/log portions of the scale
#' @details 
#' Use these functions when using nonlinear transformations of a color scale. 
#' Given data to be assigned colors from scale (x), and the number of tick 
#' intervals/integration points (nint), these functions returns a pretty set of 
#' tick marks for a legend.
#' 
#' Each function assumes a different typpe of scale, such as a logit scale for 
#' data in (0,1), a log scale for data in (0, infty), and a log1p-like scale for
#' data in (-infty, infty) with large outliers in either direction.
#' 
#' For a log1p-based scale (log(x+1)), the scale is approximately linear near 0,
#' and approximately log otherwise. Hence, the final tick marks are constructed 
#' by first generating a tick marks for a linear scale for small magnitude 
#' values (between `-smallThresh` and `smallThresh`) with approximately 
#' `nintSmall` tick marks, and concatenating this with another set of tick marks
#' assuming a log scale for magnitudes greater than `smallThresh`.
#' @author John Paige
#' @examples 
#' set.seed(123)
#' x = matrix(rnorm(100), ncol=2)
#' z = sign(runif(50)-0.5) * rlnorm(50)
#' 
#' ticks = getLog1pLikeScaleTicks(z)
#' splot(x, z, colScale=redBlueDivCols, centerScale=TRUE, 
#'       scaleFun=scaleFunLog1p, ticks=ticks)
#' @name nonLinearTicks
#' @seealso \code{\link{grDevices::axisTicks}}, \code{\link{pretty}}, 
#' \code{\link{nonlinearScales}}
#' @importFrom grDevices axisTicks
#' @export
getLogitScaleTicks = function(x, nint=3) {
  minX = min(x)
  maxX = max(x)
  
  # first generate ticks below .5, then flip about .5
  rx = x
  rx[rx > .5] = 1 - rx[rx > .5]
  
  # now add log scale ticks
  lowerTicks = grDevices::axisTicks(range(log10(rx)), log=TRUE, nint=nint)
  upperTicks = rev(1 - lowerTicks)
  
  c(lowerTicks, upperTicks)
}

#' @rdname nonlinearScales
#' @importFrom grDevices axisTicks
#' @export
getLogScaleTicks = function(x, nint=5) {
  grDevices::axisTicks(range(log10(x)), log=TRUE, nint=nint)
}

#' @rdname nonlinearScales
#' @export
getLog1pLikeScaleTicks = function(x=NULL, nint=5, nintSmall=nint, 
                                  nintLarge=nint, smallThresh=10) {
  
  valRange = range(x, na.rm=TRUE)
  absVals = abs(x)
  
  # simple linear or log base cases
  if(max(valRange) < smallThresh) {
    # values span mainly linear portion of the scale
    return(pretty(x, n=nint))
  } else if(min(valRange) > smallThresh) {
    # values span only (positive) log portion of the scale
    return(getLogScaleTicks(x, nint=nint))
  } else if(max(valRange) < -smallThresh) {
    # values span only (negative) log portion of the scale
    return(-getLogScaleTicks(-x, nint=nint))
  }
  
  linTicks = pretty(c(-smallThresh, smallThresh), n=nintSmall)
  logTicks = getLogScaleTicks(c(smallThresh, max(abs(valRange))), nint=nintLarge)
  ticks = c(linTicks, logTicks)
  ticks = unique(c(-rev(ticks), ticks))
  
  ticks
}

# nonlinear scales ----

#' Nonlinear scales
#' 
#' Utility functions for scales aside from the usual from linear and log scales
#' 
#' @param x Data prior to scaling or inverse scaling
#' 
#' @details
#' Logit scales are useful when data is in (0, 1). Log scales are useful when 
#' data is in (0, infty) with large outliers. Scales based on log1p (log(x+1)) 
#' can be useful when much of the data is in (-infty, infty), but with large 
#' outliers in the positive and negative directions. For a log1p-based scale, 
#' the scale is approximately linear near 0, and approximately log otherwise.
#' 
#' @name nonlinearScales
#' @author John Paige
#' @examples 
#' set.seed(123)
#' x = matrix(rnorm(100), ncol=2)
#' z = sign(runif(50)-0.5) * rlnorm(50)
#' 
#' ticks = getLog1pLikeScaleTicks(z)
#' splot(x, z, colScale=redBlueDivCols, centerScale=TRUE, 
#'       scaleFun=scaleFunLog1p, ticks=ticks)
#' @seealso \code{\link{nonlinearTicks}}
#' @export
scaleFunLog1p = function(x) {
  sign(x) * log1p(abs(x))
}

#' @rdname nonlinearScales
#' @export
scaleFunInvLog1p = function(x) {
  sign(x) * (exp(abs(x)) - 1)
}

#' @rdname nonlinearScales
#' @export
scaleFunSqrt = function(x) {
  sign(x) * sqrt(abs(x))
}

#' @rdname nonlinearScales
#' @export
scaleFunInvSqrt = function(x) {
  sign(x) * x^2
}

#' @rdname nonlinearScales
#' @export
scaleFunLogit = function(x) {
  log(x/(1-x))
}

#' @rdname nonlinearScales
#' @export
scaleFunInvLogit = function(x) {
  res = 1/(1+exp(-x))
  res[x > 36] = 1
  res[x < -709] = 0
  res
}






