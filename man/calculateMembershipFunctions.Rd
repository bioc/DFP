\name{calculateMembershipFunctions}
\alias{calculateMembershipFunctions}
\title{ Calculates Membership Functions }
\description{
  Calculates the \emph{Membership Functions}. These functions are used in the next step (\code{\link[DFP:discretizeExpressionValues]{discretizeExpressionValues}}) to discretize gene expression data.
}
\usage{
calculateMembershipFunctions(rmadataset, skipFactor = 3)
}
\arguments{
  \item{rmadataset}{ \code{\link[Biobase:class.ExpressionSet]{ExpressionSet}} with numeric values containing gene expression values (rows) of samples belonging to different categories (columns).\cr 
    The \code{\link[Biobase:class.ExpressionSet]{ExpressionSet}} also contains an \code{\link[Biobase:class.AnnotatedDataFrame]{AnnotatedDataFrame}} with metadata regarding the classes to which each sample belongs.}
  \item{skipFactor}{ Numeric value to omit odd values (a way of normalization).\cr 
     Higher values imply that less samples of a gene are considered as odd. If \code{\var{skipFactor}=0} do \strong{NOT} skip.\cr
     \code{Default value = 3}. \code{Range[0,)}. }
}
\value{
  \emph{Membership functions} to determine the discret value (linguistic label) corresponding to a given gene expression level.
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