\name{grmod}
\alias{grmod}
\title{
Interface for \emph{gretl} estimation commands
}
\description{The function provides interface for
gretl estimation commands using R data frames, or
gretl native data files.
}

\usage{
grmod(mdl,data,top = character(0),bottom = character(0),
      input = list(matr = list(),char = character()),
      output=list(matr=character(), char=character(),
                  series = character())) 
      
# With gretl native data file; outputs coeff, vcov, etc
# mod <- grmod("arima 0 1 1; 0 1 1; lg --nc", "bjg.gdt")
# names(mod)

# With AirPassengers from datasets package
# grmod("arima 0 1 1; 0 1 1; log(AirPassengers) --nc",AirPassengers)
}

\arguments{
  \item{mdl}{
quoted gretl estimation command, e.g. \code{"ols y const x"},
or command block, see also Examples.  
}
  \item{data}{can be one of two types: 
  (i) an R \code{data.frame}, \code{ts}, or \code{mts} object;
  (ii) quoted name of gretl data file, or name of a file gretl
  can open as data file. 
}
  \item{top}{quoted gretl script: it may contain e.g. code
  for preliminary data transformations, or unrestricted
  ols estimation before \code{restrict} block.
}
\item{bottom}{quoted \emph{gretl} script: designed for post-estimation,
e.g  

\code{"modtest --autocorr"}.
}
\item{input}{determines additional optional input, for example, the matrix of initial values for optimization (typically of likelihood maximization for \code{arima}, \code{mle}, \code{nls}, etc).}
  \item{output}{determines additional optional components of the output list, e.g. p-value for some diagnostic model test, see Value below and entry for \code{output} argument of \code{run_grcli}.}

}
\value{
a list containing imported output of correspondent \emph{gretl} \command{command} output; if available \code{$model} \var{bundle} is captured, otherwise available dollar-prefixed accessors are transmitted. Scalars, series, column and row vectors are rendered as \R numeric vectors. Matrices of other shapes are rendered as \R matrix objects. As always a user can execute run_grcli("help $vcv"), etc.
}
\details{
In the current version of the package, \var{all} \emph{gretl} estimation  \command{commands} except for \code{midasreg} are processed.  

\strong{Panel data}  

If \code{data} argument is a \emph{gretl} data file with panel settings nothing special is needed. Otherwise, the first line of of \code{top} argument should be \code{setobs id time_id --panel-vars} (use actual names!).

\strong{Command blocks}

Some of \emph{gretl} estimation \command{commands} use several lines of code, e.g. \code{restrict} (with \code{--full} option), \code{system}, \code{mle}, \code{gmm}, etc. 

\code{restrict}: \code{mdl} argument should consist of several lines encompassing all \code{restrict} block, see Examples below  

Other command blocks: \code{mdl} argument should contain only the last line of the block (typically the first "word" of the last line is "end", e.g. \code{end mle}); all previous lines shold be included into \code{top} argument.}



\references{
Cottrell, A., and Lucchetti, R. (2018) "Gretl User's Guide," \url{http://ricardo.ecn.wfu.edu/pub//gretl/manual/en/gretl-guide.pdf}
}
\author{
Oleh Komashko
}
\examples{
set.seed(13)
dfr = data.frame(y = rnorm(20), x = rnorm(20))
grmod("ols y 0 x --simple-print",dfr)
grmod("ols diff(LRY) 0 diff(LRM)","denmark.gdt")


\dontrun{
## command block example
# unrestricted linear regression
tp <- "ols diff(LRY) 0 diff(LRM) IBO -q" 
# restrict block code:
md <- 
"restrict --full
b[2] + b[3] = 0.3
end restrict"
# estimation
mod1 <- grmod(md,"denmark.gdt",top=tp)
names(mod1)

## prelimanary data manipulation example
pr <- 
"list DEMOG = age educ female black married
list INCOME = finc25 finc35 finc50 finc75 finc100 finc101"
# estimate ordered probit
grmod("probit pctstck choice DEMOG INCOME wealth89 prftshr",
      "pension.gdt",top=pr,type="ordered")

}
}

