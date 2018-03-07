\name{open_gdt}
\alias{open_gdt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
opening \emph{gretl} and \emph{gretl}-supported data files
}
\description{
The function can open data files of the following formats: \file{*.gdt, *.gdtb} (\emph{gretl} data files), \file{*.csv}, \file{*.txt}, \file{*.gnumeric} (Gnumeric), \file{*.ods} (Open Document), \file{*.xls, *.xlsx} (Excel), \file{*.dta} (Stata), \file{*.wf1} (Eviews), \file{*.sav} (SPSS), \file{*.xpt} (SAS xport), \file{*.dat} (JMulTi).
}
\usage{
open_gdt(fpath, mkstruct = TRUE, info = TRUE)
# open_gdt("denmark.gdt")


}
\arguments{
  \item{fpath}{
quoted name of a data file; the rules of adding the full path are the same as in gretl; can open data files of all gretl-supported formats: Stata, E-views, SPSS, SAS, etc. Path to a file is not needed for \emph{gretl} sample files  and for files in \emph{gretl} working directory}

 \item{mkstruct}{logical; whether to preserve time series structure; currently only yearly, quarteerly, monthly, weekly and dayly data are supported: in these cases the output data frame columns will be \code{ts} objects with 'right' time-series attributes; otherwise the output \code{data.frame} contains character-valued column named \code{"obs"} which can be translated into time structure by a user.
} 

\item{info}{logical; whether to import data description (if any); if  \code{info = TRUE} (the default) the output data frame will have additional \code{"description"} attribute : a character vector containing data set information. It can be printed in intelligible form by \code{description} function. 
}
}
\value{
\code{data.frame} if \code{info = TRUE} it will have "description" attribute.
}
\author{
Oleh Komashko
}
\examples{
denmark = open_gdt("denmark.gdt")
head(denmark)
description(denmark)

\dontrun{
gold = open_gdt("http://www.principlesofeconometrics.com/poe4/data/stata/gold.dta")
description(gold)}
}

