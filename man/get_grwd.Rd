\name{get_grwd}
\alias{get_grwd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
getting path to \emph{gretl} working directory 
}
\description{
Gets 'true' path to \emph{gretl} working directory.
}
\usage{
get_grwd()
# gretl_wd <- get_grwd()
}

\value{
character vector
}
\details{
Reproducing \emph{gretl} standard way of getting current working directory is \verb{run_grcli("eval $workdir")}. But it would be incorrect to write files to a 'foreign' directory by default. That's why running the code above will produce \R working directory instead. But a user may want to have a convenient way to access \emph{gretl} working directory at her own will. get_grwd() outputs 'true' \emph{gretl} working directory as running \verb{eval $workdir} in \emph{gretl} console/script editor.
}
\author{
Oleh Komashko
}
\examples{
gretl_wd <- get_grwd()
print(gretl_wd)

\dontrun{
path <- paste0(get_grwd(),"AirPassengers")
save_gdt(path, AirPassengers) # saving to gretl working directory
}
}