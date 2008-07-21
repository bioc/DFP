\name{DFP-package}
\alias{DFP-package}
\alias{DFP}
\docType{package}
\title{DFP Package Overview}

\description{
  This package provides a supervised technique able to identify differentially expressed genes,	
  based on the construction of \emph{Fuzzy Patterns} (FPs). 
  The \emph{Fuzzy Patterns} are built by means of applying 3 \emph{Membership Functions} to discretized gene expression values.
}

\details{
\tabular{ll}{
Package: \tab DFP\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2008-07-03\cr
License: \tab GPL-2\cr
}
The main functionality of the package is provided by the \code{\link[DFP:discriminantFuzzyPattern]{discriminantFuzzyPattern}} 
function, which works in a 4-step process:
\enumerate{
\item Calculates the \emph{Membership Functions}. These functions are used in the next step to discretize gene expression data.
\item Discretizes the gene expression data (float values) into \sQuote{Low}, \sQuote{Medium} or \sQuote{High} labels.
\item Calculates a \emph{Fuzzy Pattern} for each category. To do this, a given percentage of the samples belonging to a category must have the same label (\sQuote{Low}, \sQuote{Medium} or \sQuote{High}).
\item Calculates the \emph{Discriminant Fuzzy Pattern} (DFP) that includes those genes present in two or more FPs with different assigned labels.
}
Additional data classes: \code{\link[Biobase:class.ExpressionSet]{ExpressionSet}}, \code{\link[Biobase:class.AnnotatedDataFrame]{AnnotatedDataFrame}}.
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
#########################################
############ Get sample data ############
#########################################
library(DFP)
data(rmadataset)

#########################################
# Filter the most representative genes  #
#########################################
res <- discriminantFuzzyPattern(rmadataset)

#########################################
###### Different result displays ########
#########################################
plotMembershipFunctions(rmadataset, res$membership.functions, featureNames(rmadataset)[1:2])
showDiscreteValues(res$discrete.values, featureNames(rmadataset)[1:10], c("healthy", "AML-inv"))
showFuzzyPatterns(res$fuzzy.patterns, "healthy")[21:50]
plotDiscriminantFuzzyPattern(res$discriminant.fuzzy.pattern)
}
\keyword{ package }
