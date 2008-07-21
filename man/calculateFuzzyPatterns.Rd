\name{calculateFuzzyPatterns}
\alias{calculateFuzzyPatterns}
\title{ Calculates a Fuzzy Pattern for each category of the samples }
\description{
  Calculates a \emph{Fuzzy Pattern} for each category. To do this, a given percentage of the samples belonging to a category must have the same label (\sQuote{Low}, \sQuote{Medium} or \sQuote{High}).
}
\usage{
calculateFuzzyPatterns(rmadataset, dvs, piVal = 0.9, overlapping = 2)
}
\arguments{
  \item{rmadataset}{ \code{\link[Biobase:class.ExpressionSet]{ExpressionSet}} with numeric values containing gene expression values (rows) of samples belonging to different categories (columns).\cr 
    The \code{\link[Biobase:class.ExpressionSet]{ExpressionSet}} also contains an \code{\link[Biobase:class.AnnotatedDataFrame]{AnnotatedDataFrame}} with metadata regarding the classes to which each sample belongs.}
  \item{dvs}{ 
    Matrix containing discrete values according to the overlapping parameter after discretizing the gene expression values.\cr 
    Includes an attribute \var{types} which determines the category of each sample.}
  \item{piVal}{ Controls the degree of exigency for selecting a gene as a member of a \emph{Fuzzy Pattern}.\cr
    \code{Default value = 0.9}. \code{Range[0,1]}. }
  \item{overlapping}{ Modifies the number of membership functions used in the discretization process.\cr 
    Possible values: 
    \enumerate{
    \item \sQuote{Low}, \sQuote{Medium}, \sQuote{High}.
    \item \sQuote{Low}, \sQuote{Low-Medium}, \sQuote{Medium}, \sQuote{Medium-High}, \sQuote{High}.
    \item \sQuote{Low}, \sQuote{Low-Medium}, \sQuote{Low-Medium-High}, \sQuote{Medium}, \sQuote{Medium-High}, \sQuote{High}.
    }
    \code{Default value = 2}.
  }
}
\value{
  Genes belonging to each \emph{Fuzzy Patterns}. There are one FP for each class.\cr
    Includes an attribute \var{ifs} with the \emph{Impact Factor} for each category.
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