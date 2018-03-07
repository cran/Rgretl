\name{ramanathan}
\docType{data}
\alias{ramanathan}
\title{sample scrips from \emph{gretl} 'Ramanathan' selection}
\description{sample scrips from \emph{gretl} 'Ramanathan' selection translated as \R character vectors; they can be executed via \code{run_grcli}.}
\usage{ramanathan
# names(ramanathan)
# show_sample(ramanathan$PS3.1)
# run_grcli(ramanathan$PS3.1)}
\format{
  List containing 73 sample scripts (= character vectors) which can be executed via \code{run_grcli}. Use \code{names(ramanathan)} to see the list of scripts; use \code{show_sample(ramanathan[[i]])} to print a script in intelligible format; use \code{run_grcli(ramanathan[[i]])} to execute it (i in 1:73). \emph{gretl} sample scripts are readily available via GUI but not via command line interface. Hence, this set of scripts make the use of \pkg{Rgretl} more self-sufficient. A printing of \code{show_sample(ramanathan[[i]])}  looks exactly the same as the original \emph{hansl} script (\emph{hansl} is the name of \emph{gretl} built-in language). Since \emph{hansl} scripts were translated as \R character vectors, modifications were made in using quotes and escapes: single quotes (') were prepended by backslashes (\\') in comments and and in code properly where it serves as matrix transposition sign, and  backslashes (\\) were substituted for double backslashes (\\\) for breaking long lines of code (it is very similar to typing this fragment of \file{ramanathan.Rd} file: I had to type triple backslash above for a reader would see the double one). This is exactly what to be done if a user wishes copy-paste printing into \R script editor  and create an \R character string enclosed by single quotes (say, to use it as a template for her own code).
  }
  \source{\emph{gretl} sample scripts}
 \keyword{datasets} 


