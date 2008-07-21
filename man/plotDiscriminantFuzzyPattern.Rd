\name{plotDiscriminantFuzzyPattern}
\alias{plotDiscriminantFuzzyPattern}
\title{ Plots the Discriminant Fuzzy Pattern of the relevant genes }

\description{
  This function plots the \emph{Discriminant Fuzzy Pattern} of the relevant genes (in rows) for the sample 
  classes (in columns), as well as the impact factor which determines if a gene belongs to a \emph{Fuzzy Pattern} 
  in a class (if its value is higher than the \var{piVal}).\cr
  The relevant genes are those which are present in almost two different \emph{Fuzzy Patterns} with different linguistic labels.\cr
  The plotting is made in both graphical and text mode.
}

\usage{
plotDiscriminantFuzzyPattern(dfp, overlapping = 2)
}

\arguments{
  \item{dfp}{ A matrix with the fuzzy patterns and impact factors for the relevant genes. }
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
A matrix with the discriminant genes in rows, along with the \emph{Fuzzy Pattern} for each class (in columns).\cr
This object contains an attribute (\code{ifs}) which stores the \emph{Impact Factors} used to determine if a gene 
belongs to a \emph{Fuzzy Pattern} in a class (if the value is higher than the \var{piVal}).
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