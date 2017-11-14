### Rgretl

The goal of `Rgretl` is to provide interface for GNU gretl
(for gretl commmand line interface, to be a purist). Gretl is a cross-platform software package for econometric analysis, written in the C programming language. It is free, open-source software under the terms of the GNU General Public License (GPL).

#### Gretl installation 

For the package to work as is intended, gretl should be
installed systemwide. In the case of absence of a searcheable
gretl installation on the OS, the package will be attached
with a diagnostic message, and all package functions will
print the diagnostics, follewed by `NULL` output. Gretl can
be downloaded from <http://gretl.sourceforge.net>. The minimal gretl version
required by the current package version is 2017c.

##### Linux

Most of mopular Linux distributions have gretl binaries at
official repositories, but in many cases reposes' versions
are outdated. It is recommended to compile gretl from the
sourses. One can download stand-alone User's guide
from <https://sourceforge.net/projects/gretl/files/manual/>.
Appendix C contains instructions on installing gretl dependencies
and on building the program.

##### Mac OS X

Gretl binaries can be downloaded from <http://gretl.sourceforge.net/osx.html>

##### Windows™

Gretl binaries for Windows™ can be downloaded from
<http://gretl.sourceforge.net/win32/>. During the
installation process "**include into PATH**" option
should be **swithed on**.

###### Known path issues
The code inside the package functions uses 
`system("gretlcli [options]",...")`. In some cases
R `system()` function does not work properly (i.e. prints
diagnostics followed by `NULL` output) when gretl
is installed in the default "Program files" *"folder"*.
This situation was detected on 32-bit Russian Windows 7.
In such cases it is recommended to install gretl into
a path that **does not contain spaces**. Tests on 64-bit Windows 8 an 10 
with gretl installed by default were ok.

#### Gretl function packages

Gretl properly roughly corresponds to `r-base`. Gretl
*addons* are cousins of `r-recommended`. The rest of *gretl
function packages* corresponds to cran or bioconductor
packages. Gretl command `install package_name.gfn` or
`install package_name.zip` is an analogue of `install.packages()`.
Gretl command `include package_name.gfn` (even on installing
the extension is `.zip` one should use `.gfn` on attaching) corresponds to
`library()`.
The list of gretl ofiicial packages can be accessed via gretl
menu or at <http://ricardo.ecn.wfu.edu/gretl/cgi-bin/gretldata.cgi?opt=SHOW_FUNCS>. You can run, for example [^1]  
`script <- "install tobit_mfx.gfn"`  
`run_grcli(script)`  

Then  

`tobscript <-`  
`"open greene22_2.gdt -q`  
`include tobit_mfx.gfn`  
`list X = const Z1 Z2 Z3 Z5 Z7 Z8`  
`bundle b1 = tobit_mfx(Y,X)`  
`tobit_mfx_print(&b1)"`  
`run_grcli(tobscript)`  

For an instance, `lagreg.zip` conveniently deals with
distributed lag models, `a_eff.gfn` computes marginal effects
with correct treatment of functions of independent variables and
effects of group dummies for multilevel factors.

#### Data files and Gretl data bases
Gretl's `open` command can open not only native `(*.gdt`) and (`*.gdtb`) data files but CSV files (`*csv`), ASCII files(`*csv`), Gnumeric files (`*.gnumeric`), Excel files (`*xls`) and (`*.xlsx`), Stata files (`*dta`),
Eviews files (`*.wf1`), SPSS files (`*sav`), SAS xport files (`*.xpt`),
Octave files (`*.m`), and jMulti files (`*.dat`). Files with all the extentions above can be imported into R via `Rgretl::open_gdt()`. Also, you can
save any gretl database file via gretl and import it as gretl data file.

#### Other foreign scripts
Gretl has interfaces to R, Ox, Octave, Stata, Python, and Julia. Hence,
using gretl `foreign language=Language` block you can run 
Ox, Octave, Stata, Python, and Julia scripts via `Rgretl::run_grcli()`
function provided the relevant soft is installed on your system.


[^1]:
The references for gretl function packages mentioned in this text
are : Oleh Komasko, gretl function package tobit_mfx.gfn, <http://ricardo.ecn.wfu.edu/gretl/cgi-bin/current_fnfiles/tobit_mfx.gfn>; 
Oleh Komasko, gretl function package lagreg.zip, <http://ricardo.ecn.wfu.edu/gretl/cgi-bin/current_fnfiles/lagreg.zip>; Oleh Komasko, gretl function package a_eff.gfn, <http://ricardo.ecn.wfu.edu/gretl/cgi-bin/current_fnfiles/a_eff.gfn>. 