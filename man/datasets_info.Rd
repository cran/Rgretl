\name{datasets_info}
\alias{datasets_info}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
extracting information on \emph{gretl} sample files  
}
\description{
Extracts information on \emph{gretl} sample data files which are include into \emph{gretl} installation. This information is ready available via \emph{gretl} GUI menu but not via \emph{gretlcli}. This function makes the use of \pkg{Rgretl} tmore independent. All possible output list the set of files which can be opened by \code{open_gdt} without indicating \var{neither} path no extension. 
}
\usage{
datasets_info(directory = NULL, prn = NULL)
# dirs = datasets_info()
# gretl = datasets_info(dirs[1])
}

\arguments{
  \item{directory}{ \code{void} or character vector, see Examples.
}
\item{prn}{ integer vector: determines what subset of files to print a short description. By default prints all available information; if it is set to zero printing is suppressed, see Examples.
}
}

\value{
character vector
}

\author{
Oleh Komashko
}
\examples{
dirs = datasets_info()
gretl = datasets_info(dirs[1],1:2)
head(gretl)
}