\name{open_gdt}
\alias{open_gdt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
opening gretl data files
}
\description{
Opens gretl data file, or any of one of the gretl-supported data files formats e.g. Stata, E-views, Octave data files.
}
\usage{
open_gdt(fpath, mkstruct = TRUE)
# open_gdt("denmark.gdt")
# open_gdt("bjg.gdt", mkstruct = FALSE)

}
\arguments{
  \item{fpath}{
quoted name of a data file; the rules of adding the full path are the same as in gretl; can open data files of all gretl-supported formats: Stata, E-views, SPSS, SAS, Octave, etc. 
}
  \item{mkstruct}{boolean; whether to preserve time series structure; currently only yearly, quarteerly, and monthly data are supported; otherwise the output \code{data.frame} contains character-valued column named \code{"obs"} which can be translated into time structure by a user
}
}
\value{
a \code{data.frame}
}
\author{
Oleh Komashko
}
\examples{
dfr = open_gdt("denmark.gdt")
head(dfr)

\dontrun{
gld = open_gdt("http://www.principlesofeconometrics.com/poe4/data/stata/gold.dta")
head(gld)}
}

