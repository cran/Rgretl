\name{run_grcli}
\alias{run_grcli}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
running gretl scripts
}
\description{
The function runs gretl scripts from inside R. Currently it outputs a list
with components specified by \code{output} argument.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}



\usage{
run_grcli(text, 
          input = list(matr = list(), char = character()),
          output=list(matr=character(), char=character(),
                      series = character()),
          data=NULL)
# run_grcli(text) # only prints
# run_grcli(text, data = data_frame)
# run_grcli(text, data = "gret_file.gdt")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text}{gretl script formed as a character vector, see Examples.}
  \item{input}{determines additional components of input: scalars,
  vectors, matrices, and character strings:
  \describe{\item{}{\code{matr = list(name1 = value1,name2 = value2,...)}}
  \item{}{\code{char = c(name1 = value1,name2 = value2,...)}}
  }
  For '\code{matr}' values can be numeric vectors, or matrices. For example, \code{run_grcli('print x',input = list(matr = list(x=pi/4)))} will print \code{x =  0.78539816}. Numeric vectors will be translated as [\emph{gretl}] column matrices. If \code{x} is explicitly created in \R as a \code{matrix} object its dimentions will be preserved.
  
  '\code{char}' values are one-element character vectors.
  }
  \item{output}{determines components of the output list: it should
  contain  names of gretl script objects (in the \code{text} argument)
  as character vectors; \code{matr} can contain names of \emph{gretl} scalars
  and matrices; \code{char} can contain names of \emph{gretl} \code{string}
  objects; \code{series} can contain names of \emph{gretl} \code{series}. Series are broadcasted back to \R as numeric vectors.}
  \item{data}{a name (without quotes) of an \R \code{data.frame}, \code{ts}, or \code{mts} object; a name of \emph{gretl} (or \emph{gretl}-supported, e.g. Stata, Eviews, SAS, SPSS, etc) data file as character string.
  
}
}
\value{
A list with components specified by \code{output} argument.
Output components could be numeric atomic vectors, matrices, or character vectors; \code{gretl} \code{scalars},
row and column \code{matrices}, and \code{series} are converted to
atomic vectors.
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

Rz = run_grcli('print w v x\n eval x**y\n z = mexp(y)\n print z',
           input = list(matr = list(w = 1,v = 1:4, x= matrix(1:2,1),
           y = matrix(rep(1,9),3))),
           output = list(matr = "z"))
Rz$z

\dontrun{
# After gretl is properly installed one can
# safely copy-past and run this example

scr <-
"nulldata 10
set seed 13
series y = normal()
matrix x = seq(1,7)
matrix z = mshape({y},5,2)
string str = \"Hello, user!\"
print str"
ou <- run_grcli(scr,output = list(matr=c("x","z"),char="str",series = "y"))
ou}
}
