\name{Rgretl-package}
\alias{Rgretl-package}
%\alias{Rgretl}
\docType{package}
\title{
Interface to \emph{gretlcli}
%\packageTitle{Rgretl}
}
\description{
An interface to \emph{GNU gretl}: running \emph{gretl} scripts from \R, estimating econometric  models with backward passing of model results, opening \emph{gretl} data files \file{.gdt}. \emph{gretl} can be downloaded from \url{http://gretl.sourceforge.net}. This package could make life on introductory/intermediate econometrics courses much easier: \emph{gretl} provides full battery of the required regression diagnostics, including White's heteroskedasticity test, restricted \acronym{OLS} estimation, advanced weak instrument test after iv estimation, very convenient dealing  with lagged variables in econometric models, standard case treatment in unit root tests, vector autoregressions, and vector error correction  models. Datasets for 8 popular econometrics textbooks can be installed into \emph{gretl} from its server. All datasets can be easily imported using this package.
%\packageDescription{Rgretl}
}
\details{

\tabular{ll}{
Package: \tab Rgretl\cr
Type: \tab Package\cr
Version: \tab 0.2.2\cr
Date: \tab 2018-03-17\cr
Depends: \tab R (>= 3.4.0)\cr
SystemRequirements: \tab gretl (>= 2017c), gretlcli\cr
}

\packageIndices{Rgretl}  

\strong{Getting started}  

For the package to work properly \emph{gretl} should be installed. The installation instruction is in README file.  

The first thing to \var{run} is   

\code{run_grcli("help help")}

Yes! Run, run, help, help! Imagine, a hungry tiger is after you and there will be nothing for humdrum cramming. The output will show how to use \emph{gretl} help system from a script or console, hence using \code{run_gretlcli}. In contrast with \R \emph{gretl} has commands and functions. A command syntax is similar to that of shell commands. Run  

\verb{run_grcli("help")  
run_grcli("help functions")}  

to obtain lists of commands and functions. Let's turn to a slighly longer script:  

\verb{myfirstscript <-
'nulldata 50
eval 2 + 2
set seed 13
x1 = normal()
x2 = normal()
y = x1 + x2 + normal()
y = y > 0
printf "\\\nThis is my first gretl model:\\\n"
logit y 0 x1 x2
varlist --accessors'
run_grcli(myfirstscript)
}

It is convenient  to render a part of the output:  

\verb{This is my first gretl model:

Model 1: Logit, using observations 1-50
Dependent variable: y
Standard errors based on Hessian

             coefficient   std. error     z      slope  
  ------------------------------------------------------
  const       0.398073      0.360291    1.105           
  x1          1.15444       0.523459    2.205   0.280427
  x2          1.50154       0.521053    2.882   0.364742
}

  
  \dots  
  
\verb{model-related
 $ess (scalar: 4.92415)
 $T (scalar: 50)} 
 
Since \code{myfirstscript} is an \R character vector rather than a genuine \emph{gretl} script file it uses \verb{'\\\n'} instead of \verb{'\\n'} (as it should be in the native script) to indicate a new line in formatted printing. The single quote (\verb{'}) is used only inside strings in \emph{gretl}. Hence if a user does not apply sohpisticated string manipulation, there exists one by one correspondence. Otherwise, some additional experimenting with '\verb{\"}' may be needed. Instead of turning this subsection into a kind of textbook it should be sufficient to give a couple of hints. To understand the script above and its output one can proceed by running \code{run_grcli("help nulldata")}, \code{run_grcli("help eval")}, \code{run_grcli("help normal --func")}, etc. The output of \verb{varlist --accessors} is an analogue of output of \code{names(obj)} and/or \code{names(summary(obj))} where \code{obj} is an estimated model object, e.g. an object of class \code{"glm"}. In \emph{gretl} parlance dollar-prefixed objects are called \dfn{accessors}. Getting help on them follows the same pattern: \verb{run_grcli("help $ess")}.  

A user can broadcast named variables backward to \R:  

\verb{logitres <- run_grcli(myfirstscript, output = list(series = c("y","$uhat"),
                      matr = c("$coeff","$vcv","$rsq","$pd","$windows")))
names(logitres)
logitres$'$rsq'
class(logitres$y)
logitres$'$coeff'
logitres$'$vcv' }

Just reproduce and look at the output. Note that both scalars and matrices included into \code{matr} component of \code{output}. It worth mentioning that accessors listed by \verb{varlist --accessors} are present in the memory during \emph{gretl} session, so they needn't to be included into \code{text} argument of \code{run_grcli} to be broadcasted to \R. By contrast, in the example above the set of admissible 'non-dollar' series names includes only \code{x1}, \code{x2}, and \code{y}, since there are no other 'non-dollar' series names inside \code{myfirstscript}. If a data file is open, its series also can be broadcasted without mentioning them in the script:  

\verb{logmoney <- run_grcli("open denmark.gdt -q", output = list(series = "LRM"))
head(logmoney$LRM)}.

We end the subsection by reproducing a simple trick to estimate the 'calling' time:  

\verb{system.time(run_grcli('set stopwatch\n eval $stopwatch'))}  

\strong{Using \emph{gretl} sample data files and sample scripts}

\var{Sample data files}  

If \emph{gretl} is installed and is visible from inside \R all \emph{gretl} sample files are ready to be explored. Additional information is supplied by \code{Rgretl::datasets_info} and \code{Rgretl::description} functions.  

\var{Sample scripts}  

Three data files  in \pkg{Rgretl} (\code{gretl.rdata}, \code{greene.rdata}, and \code{ramanathan.rdata}) contain character string data only. Its contents is formed by adopted versions of \emph{gretl}-supplied \code{hansl} language sample scripts. In the first place, these scripts are easily searcheable and available from \emph{gretl} GUI but not from \emph{gretlcli}. Secon, since \code{run_grcli} deals with \R character strings, and not with the original \emph{hansl} scripts some minor changes are needed to compensate enclosing quotes. As a matter of fact, all changes appears to be inserting additional backslashes in several cases: (\verb{\\}) was substituted for \verb{(\\\)} to break long lines in code, \verb{(\\n)} -- for \verb{(\\\n)} in formatted printing (\code{prinf}) \command{command}, (') was substituted for (\\') in comments and for matrix transposition when single quotes were used to enclose character string input to \code{Rgretl::run_grcli}, etc. All scripts can be easily run via \code{run_grcli} and all have been tested in the batch mode having produced a neat nearly 1MB-sized text log file. See also help entries for these data files for the further information. 

\strong{Reading and saving data files}  

\var{Reading}

The \code{open_gdt} function can open data files of the following formats: \file{*.gdt, *.gdtb} (\emph{gretl} data files), \file{*.csv}, \file{*.txt}, \file{*.gnumeric} (Gnumeric), \file{*.ods} (Open Document), \file{*.xls, *.xlsx} (Excel), \file{*.dta} (Stata), \file{*.wf1} (Eviews), \file{*.sav} (SPSS), \file{*.xpt} (SAS xport), \file{*.dat} (JMulTi). For example, one can run

\code{denmark <- open_gdt("denmark")}  

# .gdt extension can be omitted in case (i) below 

\code{capm4 <- open_gdt("http://www.principlesofeconometrics.com/poe4/data/eviews/capm4.wf1")}

We need not indicating path to a file in two cases:

(i) for \emph{gretl} sample data files. The list of available files is at the bottom of README file. Additional sample files can be downloaded and installed via \emph{gretl GUI}. README file contains installation instruction and the list of data files. 

(ii) for files in \emph{gretl} working directory (as extracted from \emph{gretl} or by running \code{get_grwd}). 

The output is a \code{data.frame} object.  Curretntly, \dfn{time-series} structure is preserved (output data frame columns will be \code{ts} objects with the right attributes) only for yearly, quorterly, and monthly data. Otherwise, an additional column will be present for a user to recode it into time series structure by means of \R. \dfn{Panel data} structure is easily recovered if time and unit indicators are present. When there are no visible time/unit identifiers a user can run  

\code{my.frame <- open_gdt("a_file.gdt")}  

\code{time_n_id <- run_grcli('genr time',data = "a_file.gdt",output = list(series = c("time","$unit")))}  

\code{my.frame$time <- time_n_id$time}  

\code{my.frame$id <- time_n_id$'$unit'}

Now \code{my.frame} is ready. A user can load her favourite \R \var{package} for estimation of panel data models. 

Another useful cheat sheet: running  

\code{run_grcli('help $datatype\n eval $datatype', data = "denmark")}  

will give  

\verb{$datatype
Output:     scalar

Returns an integer value representing the sort of dataset that is currently
loaded: 0 = no data; 1 = cross-sectional (undated) data; 2 = time-series
data; 3 = panel data.

2}


\var{Saving}  

To this end the package have two functions: \code{save_gtd} for saving data frames and/or (multiple) time-series objects as \emph{gretl} data files (this should be useful mostly for \emph{gretl}-oriented users); for a wider target group the most interisting feature is possibility to export data to Stata format; t\code{save_bin} saves lists with time series components as \emph{gretl} binary data bases\file{*.bin}; the latter is supplied by a helper, \code{merge_data} for easy constructing lists with the appropriate structure to export, also this function supplies all series with unique names as required by \emph{gretl} for saving its binary data bases. The best way to render panel data structure is providing \R data frames with explicit unit and period identifiers before exporting.  

\strong{Plotting}  

\var{Simplified plotting}  

To this end \emph{gretl} has \code{textplot} command which outputs simple plots in console. To see a working example execute  

\code{data(ramanathan)}  

\code{cat(paste0(ramanathan$PS3.1,"\n"))}

\code{run_grcli(ramanathan$PS3.1)}  

\var{True plots}  

\emph{gretl} has several plotting commands: \command{boxplot}, \command{gnuplot}, \command{graphpg}, \command{hfplot}, \command{plot}, \command{qqplot}, \command{rmplot}, \command{scatters}. All commands above have \code{'--output=filename'} option, e.g. \code{'--output=myfile.pdf'}, or \code{'--output=display'}. Execute \code{run_grcli("help gnuplot")} to see the details on \code{'--output=filename'}. "Saving" options work as intended in \pkg{Rgretl}. At the other hand, \code{'--output=display'} option may output nothing on some systems if being called from \pkg{Rgretl}. To compensate this sad fact \pkg{Rgretl} has a special option for \emph{gretl} plotting commands: \code{--output=#R}. With this option plots will be rendered on \R standard graphic windows. A user can copy-paste and execute the code below to see how it works:

\verb{a_script <-
'nulldata 93
setobs 4 1995:1 --time-series
set seed 13
y = normal()
RandomWalk = cum(y)
x = normal()
gnuplot RandomWalk --time-series --with-lines --output=#R
qqplot y x --output=#R
'
run_grcli(a_script)
}  

Note that \code{'--output=#R'} option is not intended to work with \emph{gretl} \command{graphpg} command. Also, vector graphics is not retained currently. To insert publishing-quality plots in a tect editor use one of the "saving" options.


\strong{Estimation of econometric models}  

It is a wide practice inside \R to have several alternatives to estimating certain models. From the autor's subjective point of view the most valuable for elementary/intermediate econometrics courses features include post-estimation menu (execute \code{run_grcli("help modtest")} and references therein), heteroscedasticity toolbox (execute \code{run_grcli("help hsk"))}. \emph{gretl} provides a large toolbox for very easy working with dynamic regressions. Also \emph{gretl} provides full case treatment for unit root testing and cointegration analisys. Written on c commands for MLE estimation of models for limited and discrete dependent variable are very quick and reliable.  

The \code{grmod} function in this version by default outputs a sizable list of model output similar to that of \code{lm}, \code{glm}, etc. Still, not all estimation commands are processed by \code{grmod}. In such cases \code{run_grcli} provides an alternative; thought this way requires much more typing from a user. Some useful extensions are considered in README file.}  

\references{

{Cottrell, A., and Lucchetti, R. (2018) "Gretl User's Guide," \url{http://ricardo.ecn.wfu.edu/pub//gretl/manual/en/gretl-guide.pdf}}  

Cottrell, A., and Lucchetti, R. (2018) "Gretl Command Reference",
\url{http://ricardo.ecn.wfu.edu/pub/gretl/manual/PDF/gretl-guide.pdf}  

Cottrell, A., and Lucchetti, R. (2018) "A Hansl Primer", \url{http://ricardo.ecn.wfu.edu/pub/gretl/manual/PDF/hansl-primer.pdf} 
}
\author{
  Oleh Komashko  
  
  Maintainer: Oleh Komashko  <oleg_komashko@ukr.net>}


\keyword{ package }
