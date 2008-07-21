\name{ExpressionLevel-class}
\docType{class}
\alias{ExpressionLevel-class}
\alias{show,ExpressionLevel-method}
\title{Class "ExpressionLevel"}

\description{
  A virtual class which represents a generic \emph{Membership Function}.
}
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Slots}{
	 \describe{
    \item{\code{center}:}{Object of class \code{"numeric"}. Represents the peak point in the function curve. }
    \item{\code{width}:}{Object of class \code{"numeric"}. Represents the length of values lower than 1 and greater than 0 in the function curve. }
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "ExpressionLevel")}: Prints the ExpressionLevel subclass of the object.}
    \item{setValues}{\code{signature(object = "ExpressionLevel", values = "numeric")}: Generic function to be implemented in the subclasses.}
    \item{computeMembership}{\code{signature(object = "ExpressionLevel", x = "numeric")}: Generic function to be implemented in the subclasses.}
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
showClass("ExpressionLevel")
}
\keyword{classes}