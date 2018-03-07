\name{find_sample}
\alias{find_sample}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
finding sample scripts which use a \emph{gretl} command specified by name  
}
\description{
Findis sample scripts which use a \emph{gretl} command specified by name. Sample scripts which are present in \pkg{Rgretl} as data are searched.
}
\usage{
find_sample(cmd, dir = NULL)
# find_sample("panel")
# find_sample("wls","ramanathan")

}

\arguments{
  \item{cmd}{
\emph{gretl} command as character string, execute \code{run_grcli("help")} for the list of valid \emph{gretl} commands.}

  \item{dir}{
\emph{gretl} character string, can be \code{"gretl"}, \code{"greene"}, or \code{"ramanathan"}; specifies a name of scripts collection to search; if missing, all three are searched.}
}

\value{
character vector): contains the name(s) of found sample scripts as components of corresponding data sets.

}
\author{
Oleh Komashko
}
\examples{

find_sample("panel")
cat(paste0(gretl$penngrow,"\n"))
run_grcli(gretl$penngrow)

}