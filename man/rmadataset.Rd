\name{rmadataset}
\alias{rmadataset}
\docType{data}

\title{ A sample ExpressionSet object }

\description{
  This \code{\link[Biobase:class.ExpressionSet]{ExpressionSet}} object includes an \code{\link[Biobase:class.AnnotatedDataFrame]{AnnotatedDataFrame}} with metadata about \sQuote{Disease type} (the most important for the algorithm), \sQuote{Patient age} and \sQuote{Patient gender}.\cr
  This data set gives the expression values of 500 genes in 35 samples.
}

\usage{data(rmadataset)}

\format{
\tabular{ll}{
\code{\link[Biobase:class.ExpressionSet]{ExpressionSet}} \tab \code{str(pData(phenoData(rmadataset)))}\cr
\code{\link[Biobase:class.AnnotatedDataFrame]{AnnotatedDataFrame}} \tab \code{str(exprs(rmadataset))}\cr
}
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

\examples{
data(rmadataset)
featureNames(rmadataset)[1:20]
sampleNames(rmadataset)
varLabels(rmadataset)
pData(phenoData(rmadataset))
exprs(rmadataset)[1:10,1:5]
}

\keyword{datasets}