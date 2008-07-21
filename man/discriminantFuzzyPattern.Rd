\name{discriminantFuzzyPattern}
\alias{discriminantFuzzyPattern}
\title{ Discriminant Fuzzy Pattern to filter genes }

\description{
  \emph{discriminantFuzzyPattern} discovers significant genes based on the construction of \emph{Fuzzy Patterns} (FPs). 
  The \emph{Fuzzy Patterns} are built by means of applying 3 \emph{Membership Functions} to the gene expression values in the matrix \emph{rmadataset}.
}

\usage{
discriminantFuzzyPattern(rmadataset, skipFactor = 3, zeta = 0.5, overlapping = 2, piVal = 0.9)
}

\arguments{
  \item{rmadataset}{ \code{\link[Biobase:class.ExpressionSet]{ExpressionSet}} with numeric values containing gene expression values (rows) of samples belonging to different categories (columns).\cr 
    The \code{\link[Biobase:class.ExpressionSet]{ExpressionSet}} also contains an \code{\link[Biobase:class.AnnotatedDataFrame]{AnnotatedDataFrame}} with metadata regarding the classes to which each sample belongs.}
  \item{skipFactor}{ Numeric value to omit odd values (a way of normalization).\cr 
     Higher values imply that less samples of a gene are considered as odd. If \code{\var{skipFactor}=0} do \strong{NOT} skip.\cr
     \code{Default value = 3}. \code{Range[0,)}. }
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
  \item{piVal}{ Controls the degree of exigency for selecting a gene as a member of a \emph{Fuzzy Pattern}.\cr
    \code{Default value = 0.9}. \code{Range[0,1]}. }
}

\details{
The \code{\link[DFP:discriminantFuzzyPattern]{discriminantFuzzyPattern}} function works in a 4-step process:
\enumerate{
\item Calculates the \emph{Membership Functions}. These functions are used in the next step to discretize gene expression data.
\item Discretizes the gene expression data (float values) into \sQuote{Low}, \sQuote{Medium} or \sQuote{High} labels.
\item Calculates a \emph{Fuzzy Pattern} for each category. To do this, a given percentage of the samples belonging to a category must have the same label (\sQuote{Low}, \sQuote{Medium} or \sQuote{High}).
\item Calculates the \emph{Discriminant Fuzzy Pattern} (DFP) that includes those genes present in two or more FPs with different assigned labels.
}
}

\value{
  \item{membership.functions}{ \emph{Membership functions} to determine the discret value corresponding to a given gene expression level. }
  \item{discrete.values}{ Discrete values according to the overlapping parameter after discretizing the gene expression values.\cr 
    Includes an attribute \var{types} which determines the category of each sample. }
  \item{fuzzy.patterns}{ Genes belonging to each \emph{Fuzzy Patterns}. There are one FP for each class.\cr
    Includes an attribute \var{ifs} with the \emph{Impact Factor} for each category.  }
  \item{discriminant.fuzzy.pattern}{ Genes belonging to the final DFP.\cr 
    Includes an attribute \var{ifs} with the \emph{Impact Factor} for each category. }
  \item{params}{ The parameters used to tune the algorithm (as arguments in the function). }
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
# Filters the most representative genes #
#########################################
res <- discriminantFuzzyPattern(rmadataset)
summary(res)
}
\keyword{manip}