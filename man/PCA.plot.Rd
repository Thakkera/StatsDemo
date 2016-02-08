\name{PCA.plot}
\alias{PCA.plot}
\alias{scoreplot}
\alias{loadingplot}
\alias{scoreplot.PCA}
\alias{loadingplot.PCA}
\alias{screeplot}
\alias{biplot.PCA}
\title{Principal Component Analysis plotting functions}
\description{
  Plotting functions for PCA: for scores, loadings, scores and loadings
  simultaneously (a biplot), and variances (a screeplot, where the log
  of the explained variance is plotted for each PC).
}

\usage{
\method{scoreplot}{PCA}(object, pc = c(1,2),
                        pcscores = scores(object),
                        show.names = FALSE, xlab, ylab, xlim, ylim,
                        \dots)
\method{loadingplot}{PCA}(object, pc = c(1,2),
                          pcloadings = loadings(object),
                          scalefactor = 1, add = FALSE,
                          show.names = FALSE, xlab, ylab, xlim, ylim,
                          col = "blue", min.length = .01, varnames = NULL,
                          \dots)
screeplot(object, type = c("scree", "percentage"), npc, \dots)
\method{biplot}{PCA}(x, pc = c(1,2),
       pcscores = scores(object),
       pcloadings = loadings(object),
       show.names = c("none", "scores", "loadings", "both"),
       score.col = 1, loading.col = "blue",
       min.length = .01, varnames = NULL, \dots)
}

\arguments{
  \item{object, x}{an object of class "PCA" (see below).}
  \item{pc}{which PCs to show.}
  \item{pcscores}{what scores to plot. If given, this makes the
    \code{object} argument superfluous; the \code{pc} argument may still
    be used to select columns from the given \code{pcscores}.}
  \item{pcloadings}{what loadings to plot. If given, this makes the
    \code{object} argument superfluous; the \code{pc} argument may still
    be used to select columns from the given \code{pcloadings}.}
  \item{scalefactor}{scaling factor for the loadings; used internally, when
    the \code{loadingplot} function is called from within
    \code{biplot.PCA}.}
  \item{add}{logical, whether to add to the existing plot (again, useful
    when \code{loadingplot} is called from within \code{biplot.PCA}).}
  \item{show.names}{show names rather than plotting
    symbols. For \code{loadingplot} and \code{scoreplot} a logical
    (default: FALSE), for \code{biplot} one of 'scores', 'loadings',
    'both' or 'none' (default).}
  \item{xlab, ylab, xlim, ylim, col, \dots}{additional graphical arguments
    (see \code{\link{par}}).}
  \item{npc}{how many PCs to show in the scree plot (starting from 1).}
  \item{type}{show a real screeplot (\code{scree}) or show the
    percentage of variance explained (\code{percentage}).}
  \item{score.col, loading.col}{colours of the scores and loadings in a
    biplot.}
  \item{min.length}{minimal length of loading vectors to be plotted by
    arrows. Vectors that are too short lead to warning messages, are not
    interesting, and only clutter the graphic.}
  \item{varnames}{alternative vector of variable names.}
}

\details{Score plots and loading plots show the amount of explained
  variance at the axis labels, but only when PCA
  has been performed at mean-centered data.}

\references{
  R. Wehrens:  "Chemometrics with R - Multivariate Data Analysis in the
  Natural Sciences and Life Sciences". Springer, Heidelberg, 2011.
}
\seealso{\code{\link{PCA}}}
\examples{
data(wines)
wines.PC <- PCA(scale(wines))
scoreplot(wines.PC)
loadingplot(wines.PC)
biplot(wines.PC)
screeplot(wines.PC)
}
\keyword{hplot}
