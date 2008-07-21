\name{HighExpressionLevel-class}
\docType{class}
\alias{HighExpressionLevel-class}
\alias{computeMembership,HighExpressionLevel-method}
\alias{setValues,HighExpressionLevel-method}
\title{Class "HighExpressionLevel"}

\description{
  A class which represents a \emph{Membership Function} to determine the membership of a numeric value to 
  the \sQuote{High} discrete label. The result depends on the \sQuote{center} and \sQuote{width} values.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("HighExpressionLevel")}.
}
\section{Slots}{
	 \describe{
    \item{\code{center}:}{Object of class \code{"numeric"}. Represents the peak point in the function curve. }
    \item{\code{width}:}{Object of class \code{"numeric"}. Represents the length of values lower than 1 and greater than 0 in the function curve. }
  }
}
\section{Extends}{
Class \code{"\linkS4class{ExpressionLevel}"}, directly.
}
\section{Methods}{
  \describe{
    \item{setValues}{\code{signature(object = "HighExpressionLevel", values = "numeric")}: Establishes the \sQuote{center} and \sQuote{width} slots of the object, given a vector of numeric values.}
    \item{computeMembership}{\code{signature(object = "HighExpressionLevel", x = "numeric")}: Returns a value in the [0,1] interval, which represents the membership to the \sQuote{High} discrete label.}
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
showClass("HighExpressionLevel")
}
\keyword{classes}