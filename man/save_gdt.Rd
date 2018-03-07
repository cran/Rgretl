\name{save_gdt}
\alias{save_gdt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
saving data as \emph{gretl}(-export) data files 
}
\description{
Saves data frames and/or (multiple) time-series objects as gretl data files, or in any the \emph{gretl}-supported data files formats e.g. Stata, Octave, PcGive data files.
}
\usage{
save_gdt(fname, ...)
# save_gdt(c("AirPassengers.gdt", "AirPassengers.dta"), AirPassengers, AirPassengers.gdt)
# save_gdt("AirPassengers.dat --jmulti", AirPassengers)
# save_gdt("AirPassengers.m --gnu-octave", AirPassengers)
# save_gdt("AirPassengers.dat --dat", AirPassengers)

}
\arguments{
  \item{fname}{
character vector consisting of names of data files to be created; supported extensions are 'gdt', 'gdtb' (both \emph{gretl}), 'txt', 'csv', 'asc', 'dta' (Stata), 'dat' (JMulTI, adding \code{ --jmulty} is needed, see Usage above), 'm' (Octave, adding \code{ --gnu-octave} is needed, see Usage above), 'dat' (PcGive, adding \code{ --dat} is needed, see Usage above). Omitted extension is the same as '.gdt'. The default path is current \R working directory. In '.gdt' and '.gdtb' files dayly, weekly, monthly, quarterly, and yearly series will retain dates and frequencies.
}
  \item{\dots}{\R data objects, each can be \code{data.frame}, \code{ts}, or \code{mts} object, the number of arguments in \dots \strong{must} coinside with the length of \code{fname}.
}
}
\value{
void
}
\author{
Oleh Komashko
}
\examples{
\dontrun{
save_gdt("AirPassengers", AirPassengers) # saving as '.gdt'

path <- paste0(get_grwd(),"AirPassengers")
save_gdt(path, AirPassengers) # saving to gretl working directory
}
}
