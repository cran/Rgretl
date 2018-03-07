\name{save_bin}
\alias{save_bin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
saving data as \emph{gretl} data bases 
}
\description{
Saves data in the form of a list containing time-series objects of different lengths and frequencies as gretl data bases.
}
\usage{
save_bin(f_name, data_list, overwrite = FALSE, select = NULL)
}
\arguments{
  \item{f_name}{
the name of file to be created or changed, as character vector; \emph{gretl} data bases have 'bin' extension, which will be appended by default. The default path is current \R working directory.
}
  \item{data_list}{non-branched list (e.g. output of \code{merge_data}); its components must be \code{ts} objects; the set of admissible frequencies is determined by those, supported by \emph{gretl} data bases: monthly, quortery, yearly; if \code{data_list} contains series with another frequencies they will not not saved and warning(s) will be produced.}
  \item{overwrite}{logical;  \code{save_bin} is essentially a wrapper over \emph{gertl} \command{store "filename" --database [--overwrite]} command; setting \code{overwrite = TRUE} switches on the corresponding option; execute \code{run_grcli("help store")} for details.
}
\item{select}{integer vector,  must be a subset of \code{1:length(data_list)}; \code{data_list} could consist of dozens of thousands series, so a user may want to recode not all series.}
}
\value{
void
}
\author{
Oleh Komashko
}
\examples{
\dontrun{
save_bin("airmix ", merge_data(AirPassengers,airmiles))
}
}
