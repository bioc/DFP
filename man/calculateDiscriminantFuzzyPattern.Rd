\name{calculateDiscriminantFuzzyPattern}
\alias{calculateDiscriminantFuzzyPattern}
\title{ Calculates the Discriminant Fuzzy Pattern to select significative genes }
\description{
  Calculates the \emph{Discriminant Fuzzy Pattern} (DFP) that includes those genes present in two or more FPs with different assigned labels.
}
\usage{
calculateDiscriminantFuzzyPattern(rmadataset, fps)
}
\arguments{
  \item{rmadataset}{ \code{\link[Biobase:class.ExpressionSet]{ExpressionSet}} with numeric values containing gene expression values (rows) of samples belonging to different categories (columns).\cr 
    The \code{\link[Biobase:class.ExpressionSet]{ExpressionSet}} also contains an \code{\link[Biobase:class.AnnotatedDataFrame]{AnnotatedDataFrame}} with metadata regarding the classes to which each sample belongs.}
  \item{fps}{ Genes belonging to each \emph{Fuzzy Patterns}. There are one FP for each class.\cr
    Includes an attribute \var{ifs} with the \emph{Impact Factor} for each category. }
}
\value{
  Genes belonging to the final DFP.\cr 
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