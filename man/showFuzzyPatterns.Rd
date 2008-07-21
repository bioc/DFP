\name{showFuzzyPatterns}
\alias{showFuzzyPatterns}
\title{ Plots the Fuzzy Patterns corresponding to a class }

\description{
  This functions prints (in text mode) the \emph{Fuzzy Patterns} (discrete labels) calculated for a single class of samples.
}

\usage{
showFuzzyPatterns(fps, class)
}

\arguments{
  \item{fps}{ A matrix with the \emph{Fuzzy Patterns} (discrete labels) for all the samples and genes. }
  \item{class}{ A class to which the samples belong. It must be one of the classes stored in the phenoData 
    of the original \code{\link[Biobase:class.ExpressionSet]{ExpressionSet}} \emph{rmadataset} object. }
}

\value{
  A vector of \emph{Fuzzy Patterns} (discrete labels) for a single class of samples, with the genes associated.
}

\author{
Rodrigo Alvarez-Gonzalez\cr
Daniel Glez-Pena\cr
Fernando Diaz\cr
Florentino Fdez-Riverola\cr
Maintainer: Rodrigo Alvarez-Gonzalez <\email{rodrigo.djv@uvigo.es}>
}

\references{
F. Diaz; F. Fdez-Riverola; D. Glez-Pena; J.M. Corchado. Using Fuzzy
Patterns for Gene Selection and Data Reduction on Microarray Data. 7th
International Conference on Intelligent Data Engineering and Automated
Learning: IDEAL 2006, (2006) pp. 1095-1102
}

\keyword{manip}