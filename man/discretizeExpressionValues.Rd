\name{discretizeExpressionValues}
\alias{discretizeExpressionValues}
\title{ Function to discretize gene expression data }
\description{
  Discretizes the gene expression data (float values) into \sQuote{Low}, \sQuote{Medium} or \sQuote{High} labels.
}
\usage{
discretizeExpressionValues(rmadataset, mfs, zeta = 0.5, overlapping = 2)
}
\arguments{
  \item{rmadataset}{ \code{\link[Biobase:class.ExpressionSet]{ExpressionSet}} with numeric values containing gene expression values (rows) of samples belonging to different categories (columns).\cr 
    The \code{\link[Biobase:class.ExpressionSet]{ExpressionSet}} also contains an \code{\link[Biobase:class.AnnotatedDataFrame]{AnnotatedDataFrame}} with metadata regarding the classes to which each sample belongs.}
  \item{mfs}{ \emph{Membership functions} to determine the discret value (linguistic label) corresponding to a given gene expression level. }
  \item{zeta}{ Threshold value which controls the activation of a linguistic label ('Low', 'Medium' or 'High').\cr 
    The lower, the less posibilities of having genes with more than one assigned linguistic label.\cr
    \code{Default value = 0.5}. \code{Range[0,1]}. }
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
  Matrix containing discrete values according to the overlapping parameter after discretizing the gene expression values.\cr 
    Includes an attribute \var{types} which determines the category of each sample.
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