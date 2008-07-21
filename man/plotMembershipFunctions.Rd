\name{plotMembershipFunctions}
\alias{plotMembershipFunctions}
\title{ Plots the Membership Functions (Low, Medium, High) used to discretize gene expression values }

\description{
  Each gene has 3 \emph{Membership Functions} (\sQuote{Low}, \sQuote{Medium} and \sQuote{High}) which can be plotted as curves in graphical mode.\cr
  In the text mode a membership function is represented with its \emph{center} and \emph{width}.\cr
  This function receives one or more gene names and plots the results in both graphical and text mode.
  If a set of genes containing more than 36 elements is provided, only the text mode is available.
}

\usage{
plotMembershipFunctions(rmadataset, mfs, genes)
}

\arguments{
  \item{rmadataset}{ An \code{\link[Biobase:class.ExpressionSet]{ExpressionSet}} object with \code{\link[Biobase:class.AnnotatedDataFrame]{AnnotatedDataFrame}} metadata. }
  \item{mfs}{ A list of 3 ExpressionLevel objects (\sQuote{Low}, \sQuote{Medium} and \sQuote{High}) for each gene (a list of lists). }
  \item{genes}{ The set of genes to plot (a vector). }
}

\value{
  A dataframe with the values of the membership functions (\sQuote{Low}, \sQuote{Medium} and \sQuote{High}) for each gene (in rows) received as a parameter.
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