\name{run_grcli}
\alias{run_grcli}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
running gretl scripts
}
\description{
The function runs gretl scripts from inside R. Currently it only prints the results, but a user can easily save the required objects from incide the script and open them with a relevant R function
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
run_grcli(text)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text}{gretl script formed as a character, see Examples
%%     ~~Describe \code{text} here~~
}
}
\value{
Currently, this is a void function
}
\author{
Oleh Komashko
}
\examples{
# The differences with the native gretl script are:
# (i) name of character vector (gr.script <- here)
# (ii) quotes: for make hansl code a character string;
#      quotes inside script should be escaped, e.g. 
#      text <- "eval \"hello, word\""
gr.script <- 
"open denmark.gdt -q
ols diff(LRY) 0 diff(LRM) IBO --simple-print
restrict --silent
b[2] + b[3] = 0.3
end restrict
pv = $pvalue
if pv > 0.05
restrict --full
b[2] + b[3] = 0.3
end restrict
set warnings off
modtest --white-nocross 
endif"

run_grcli(gr.script)
}
