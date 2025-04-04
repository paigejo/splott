% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/splott.R
\name{colorScales}
\alias{colorScales}
\alias{purpleYellowSeqCols}
\alias{redBlueDivCols}
\alias{redGrayBlueDivCols}
\alias{blueSeqCols}
\alias{greenSeqCols}
\alias{blueGreenYellowSeqCols}
\alias{redYellowBlueCols}
\title{Gets colors from color scale}
\usage{
purpleYellowSeqCols(n, rev = FALSE, ggplot = FALSE)

redBlueDivCols(n, valRange = NULL, center = NULL, rev = FALSE, ggplot = FALSE)

redGrayBlueDivCols(
  n,
  valRange = NULL,
  center = NULL,
  rev = FALSE,
  ggplot = FALSE
)

blueSeqCols(n, ggplot = FALSE, rev = FALSE)

greenSeqCols(n, ggplot = FALSE, rev = FALSE)

blueGreenYellowSeqCols(n, ggplot = FALSE, rev = FALSE)

redYellowBlueCols(n, ggplot = FALSE)
}
\arguments{
\item{n}{Number of colors to generate in the scale}

\item{rev}{Whether to reverse the color scale}

\item{ggplot}{If FALSE, returns a vector of \code{n} colors. Otherwise, returns a
color scale object for use in ggplot functions}

\item{valRange}{Range of the data to make the color scale based on}

\item{center}{Center of the range of responses set setting diverging color
scales}

\item{cols}{A vector of colors forming color scale}

\item{colScale}{a function taking 'n' as input and returns a color scale. See
\code{?colorScales} for formatting}
}
\description{
Gets colors of responses from a color scale, potentially based on scale
transformations and with centering
}
\details{
A set of color scale generators. For other interesting color scales,
including sequential, diverging, discrete, and color blind friendly scales,
see colorspace::choose_palette().
}
\examples{
set.seed(123)
x = rnorm(100)
col = redBlueDivCols(64, valRange=range(x), center=0)
splot(x, x, x, colScale=col)
splot(x, x, x, colScale=rainbow)

}
\seealso{
\code{\link{nonlinearScales}}, \code{\link{nonlinearTicks}},
\code{\link{colorScaleUtilities}}
}
\author{
John Paige
}
