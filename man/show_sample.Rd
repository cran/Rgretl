\name{show_sample}
\alias{show_sample}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
printing \emph{gretl} sample scripts  
}
\description{
Prints \emph{gretl} sample scripts. Outputs script contents (invisible). It is convenient when the function argument is a name, rather then a script itself.
}
\usage{
show_sample(x)
# show_sample("gretl$nile")
# show_sample(gretl$nile)

}

\arguments{
  \item{x}{
character vector; a \emph{gretl} sample script, or its name.}
}
\value{
character vector}



\author{
Oleh Komashko
}
\examples{
mles <- find_sample("mle")
mle1 <- show_sample(mles[1])
\dontrun{
run_grcli(mle1)
}
}