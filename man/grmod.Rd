\name{grmod}
\alias{grmod}
\title{
Interface for gretl estimation commands
}
\description{The function provides interface for
gretl estimation commands using R data frames, or
gretl native data files.
}
\usage{
grmod(text, data = NULL, output = NULL)

# With gretl native data file; outputs coeff, vcov, etc
# mod <- grmod("arima 0 1 1; 0 1 1; lg --nc", "bjg.gdt", "std")
# names(mod)

# With AirPassengers from datasets, only prints results
# grmod("arima 0 1 1; 0 1 1; log(AirPassengers) --nc",AirPassengers)
}

\arguments{
  \item{text}{
quoted gretl estimation command, e.g. \code{"ols y const x"}
}
  \item{data}{can be one of two types: 
  (i) an R \code{data.frame}, \code{ts}, or \code{mts} object;
  (ii) quoted name of gretl data file, or name of a file gretl
  can open as data file 
}
  \item{output}{if missing it is \code{list()}, also can be (i) \code{"std"}: in this case the function output is a \code{list} with main estimation results, and (ii) \code{"$extractor"}, where  the latter is an accesible gretl model extractor, e.g. \code{"$lnl"}, in this case the function outputs a \code{list} with the extractor as its sole element
}
}
\value{
a list described in \code{output} argument section
}
\references{
Cottrell, A., and Lucchetti, R. (2017) "Gretl User's Guide," URL
{http://ricardo.ecn.wfu.edu/pub//gretl/manual/en/gretl-guide.pdf}
}
\author{
Oleh Komashko
}
\examples{
set.seed(13)
dfr = data.frame(y = rnorm(20), x = rnorm(20))
grmod("ols y 0 x --simple-print",dfr)
}

