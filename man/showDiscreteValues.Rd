\name{showDiscreteValues}
\alias{showDiscreteValues}
\title{ Prints the labels to which the algorithm converts the gene expression values }

\description{
  In an intermediate step, the algorithm \code{\link[DFP:discriminantFuzzyPattern]{discriminantFuzzyPattern}} 
  converts the gene expression values into discrete labels (combining \sQuote{Low}, \sQuote{Medium} and \sQuote{High}, 
  depending on the value of the param \sQuote{overlapping}).\cr
  This function permits printing these labels, specifying a set of genes (a vector) and/or classes of samples.
}

\usage{
  showDiscreteValues(dvs, genes, classes)
}

\arguments{
  \item{dvs}{ A matrix with discrete labels for a set of genes (in rows) of several samples (in columns). }
  \item{genes}{ [optional] The set of genes to plot. }
  \item{classes}{ [optional] A set of classes to which the samples belong. It must be one of the classes stored in the 
    phenoData of the original \code{\link[Biobase:class.ExpressionSet]{ExpressionSet}} object. }
}

\value{
  A subset of the matrix \code{dvs} determined by the restrictions (\code{genes} and/or \code{classes}).
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