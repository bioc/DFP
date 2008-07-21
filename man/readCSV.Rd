\name{readCSV}
\alias{readCSV}
\title{ Creates an ExpressionSet with an AnnotatedDataFrame from CSV files }

\description{
  This function creates an \code{\link[Biobase:class.ExpressionSet]{ExpressionSet}} with an \code{\link[Biobase:class.AnnotatedDataFrame]{AnnotatedDataFrame}}. To do this, it requires two CSV files in a predefined format:
  \enumerate{
	\item \sQuote{\code{exprsData}} with the expression values of genes (in rows) of different samples (in columns).
	\item \sQuote{\code{pData}} with the samples (in columns) and the metadata \sQuote{class} (the most important for the algorithm \code{\link[DFP:discriminantFuzzyPattern]{discriminantFuzzyPattern}}), \sQuote{age} and \sQuote{sex}.
  }
}

\usage{
readCSV(fileExprs, filePhenodata)
}

\arguments{
  \item{fileExprs}{ The path to the \code{exprsData} file. }
  \item{filePhenodata}{ The path to the \code{pData} file. }
}

\value{
  An \code{\link[Biobase:class.ExpressionSet]{ExpressionSet}} object with an \code{\link[Biobase:class.AnnotatedDataFrame]{AnnotatedDataFrame}} storing \sQuote{class}, \sQuote{age} and \sQuote{sex} information.
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
dataDir <- system.file("extdata", package="DFP"); dataDir
fileExprs <- file.path(dataDir, "exprsData.csv"); fileExprs
filePhenodata <- file.path(dataDir, "pData.csv"); filePhenodata
rmadataset <- readCSV(fileExprs, filePhenodata); rmadataset
pData(phenoData(rmadataset))
exprs(rmadataset)[1:10,1:5]
}

\keyword{manip}