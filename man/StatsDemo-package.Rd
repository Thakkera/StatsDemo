\name{StatsDemo-package}
\alias{StatsDemo-package}
\alias{StatsDemo}
\docType{package}
\title{
\packageTitle{StatsDemo}
}
\description{The StatsDemo package provides some very simple hands-on
  exercises for a first encounter with multivariate
  statistics. Originally developed for the EMBO course in computational
  metabolomics in Hinxton (UK).
}
\details{The package covers four topics: a very brief quiz on univariate
  testing, some basic plotting methods, PCA and PLS. This version
  contains shiny apps for the four topics (see the examples below); the
  older versions of the exercises are still available as functions
  \code{StatsDemo:::StatsQuiz()}, \code{StatsDemo:::ExPlot()},
  \code{StatsDemo:::ExPCA()} and \code{StatsDemo:::ExPLS()}.
}
\author{
\packageAuthor{StatsDemo}

Maintainer: \packageMaintainer{StatsDemo}
}
\keyword{ package }
\examples{
\dontrun{
  runEx("Quiz")
  runEx("Plotting")
  runEx("PCA")
  runEx("PLS")
}
}
