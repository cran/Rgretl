\name{merge_data}
\alias{merge_data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
merging data frames and/or (multiple) time-series objects as simple list 
}
\description{
Merges data frames and/or (multiple) time-series objects as simple list, with retainig time series structure (if any). If there were duplicated names, some components are renamed for all output list elements to have unique names. Can be used for creating datasets where individual series can have different lenghts and frequencies. Also it is used for data preparation to create \emph{gretl} data bases.
}
\usage{
merge_data(...)
# merge_data(airmiles,AirPassengers)

}
\arguments{
  \item{\dots}{\R data objects, each can be \code{data.frame}, \code{ts}, or \code{mts} object.
}
}
\value{
list
}
\author{
Oleh Komashko
}
\examples{
all_data <- merge_data(airmiles,AirPassengers)
head(all_data[1])
head(all_data[2])
}
