## Rgretl

The goal of `Rgretl` is to provide interface for GNU gretl
(for gretl commmand line interface, to be a purist). Gretl is a cross-platform software package for econometric analysis, written in the C programming language. It is free, open-source software under the terms of the GNU General Public License (GPL).

#### Gretl installation 

For the package to work as is intended, gretl should be
installed systemwide. In the case of absence of a searcheable
gretl installation on the OS, the package will be attached
with a diagnostic message, and all package functions will
print the diagnostics, followed by `NULL` output. Gretl can
be downloaded from <http://gretl.sourceforge.net>. The minimal gretl version
required by the current package version is 2016c.

##### Linux

Most of popular Linux distributions have gretl binaries at
official repositories, but in many cases reposes' versions
are outdated. It is recommended to compile gretl from the
sources. One can download stand-alone User's guide
from <https://sourceforge.net/projects/gretl/files/manual/>.
Appendix C contains instructions on installing gretl dependencies
and on building the program.    


##### Mac OS X

Gretl binaries can be downloaded from <http://gretl.sourceforge.net/osx.html>  
The GUI program should have been run (main window/Tools/Gretl console:
run, e.g. help) beforehand and
this way a valid `~/.gretl2rc` file should be created in the user's
home directory.   
The default place for `gretlcli` is `/Applications/Gretl.app/Contents/Resources/bin/gretlcli`  
Then the path should be made searchable, e.g. by running in shell  
`export PATH=$PATH:~/Applications/Gretl.app/Contents/Resources/bin/`   
(the path should be adjusted if the actual path of `gretl` installation is different)  
Then in terminal  

`cd "/Library/Frameworks/R.framework/Resources"`  
`echo d > .Renviron`  
`nano .Renviron` 

In `nano` window one should type 

`PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin:/Applications/Gretl.app/Contents/Resources/bin/"`

After saving the file again in terminal:  

`cp .Renviron ~/`  

Check in `R` (`RStudio`)  

`z <- readRenviron("~/.Renviron")` # **must** be `TRUE`  

`system("echo $PATH")` # should output valid path to `gretlcli`  

It was not simple to get since I've never used `OS X` before.

Of course, R should be restarted after these operations.


##### Windows™

Gretl binaries for Windows™ can be downloaded from
<http://gretl.sourceforge.net/win32/>. During the
installation process "**include into PATH**" option
should be **switched on**.

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
menu or cliking 'contributed function packages' at Gretl main page. You can run, for example [^1]  
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

### Appendix
The files listed below can be opened without indicating pathes:  
`Rgretl::open_gdt("abdata")`  

#### List of sample data files
##### Gretl: various illustrative datafiles  
"abdata","Arellano and Bond (1991) panel data"  
"anscombe","Anscombe's Quartet (artificial data)"  
"arma","Artificial data for ARMA script example"  
"australia","Johansen's Australian macroeconomic data"  
"AWM","Euro area macroeconomic data, 1970-1998"  
"AWM17","Euro area macroeconomic data, 1970-2016"  
"banks91","Costs of Italian commercial banks"  
"b-g","Bollerslev and Ghysels exchange rate data"  
"bjg","Box and Jenkins Series G (airline passengers)"  
"brand_cassola","Euro area money demand data"  
"broiler","Epple and McCallum broiler chicken data"  
"CEL","Caselli, Esquivel and Lefort growth panel data"  
"chomage","D.S.G. Pollock's Swiss unemployment data"  
"credscore","Greene's credit scoring data"  
"denmark","Johansen's Danish macroeconomic data"  
"djclose","Daily close of Dow-Jones, 1980s"  
"ects_nls","Nonlinear least squares example"  
"engel","Engel's income and food expenditure data"  
"galton","Francis Galton's heights data"  
"gdp_midas","MIDAS data, quarterly GDP and monthly covariates"  
"gear","NIST data for variance tests"  
"griliches","Wages of young men and their determinants"  
"hall","Consumption and asset returns, U.S."  
"hamilton","Prices and exchange rate, U.S. and Italy"  
"hendry_jae","David Hendry, JAE 2001, UK macro data"  
"jgm-data","Interest rates and inflation, Canada"  
"keane","Keane and Wolpin career choice data"  
"kennan","Duration of contract strikes, U.S."  
"klein","US macro data for Klein's Model 1"  
"kmenta","US macro data plus artificial data"  
"longley","Annual U.S. labor-market data"  
"leverage","Illustrates detection of incorrect data"  
"mccullagh","Ship damage data used by William Greene"  
"mroz87","Women's labor force participation and pay"  
"mrw","Mankiw, Romer and Weil cross-country data"  
"murder_rates","McManus capital punishment data"  
"nc_crime","Cornwell and Trumbull crime panel data"  
"nile","Annual Nile flow rate data, 1871-1970"  
"np","Nelson and Plosser (JME, 1982) US macro data"  
"nysewk","Weekly NYSE closing price, 1996-2006"  
"ooballot","Country votes on Office Open XML as ISO standard"  
"penngrow","Nerlove cross-country growth data"  
"pension","Pension-plan participation (Papke, 2004)"  
"poisson","Test data for Poisson regression"  
"rac3d","Cameron-Trivedi doctor visits data"  
"recid","Wooldridge recidivism (duration) data"  
"rwm","German microdata with assorted socioeconomic variables"    
"sw_ch12","International macro data (Stock and Watson)"  
"sw_ch14","Unemployment and inflation (Stock and Watson)"  
"theil","Henri Theil's textile consumption data"  
"ukppp","Johansen and Juselius (1992) PPP data"  
"union_wooldridge","Unionization of workers in the US"  
"wgmacro","West German macro data from Helmut Lütkepohl"  
"wtp","Willingness to pay data from Verbeek"  

##### Greene: selected data sets from William Greene's Econometric Analysis
"greene5_1","U.S. macro data, 1950-2000"  
"greene7_8","Gasoline price and consumption"  
"greene8_3","Solow (1957) production function data"  
"greene9_1","Manufacturing of transportation equipment"  
"greene10_3","Interest rate, M2 and GNP"  
"greene11_3","Aggregate consumption and income data"  
"greene12_1","Micro income and expenditure data"  
"greene13_1","Grunfeld investment data"  
"greene14_1","Airline costs data (six-firm panel)"  
"greene18_1","Corporate bond yields"  
"greene18_2","GNP, money and price deflator"  
"greene19_1","Data on educational program effectiveness"  
"greene22_2","Fair data on extra-marital affairs"  
"greene25_1","Expenditure and credit-rating data"  

##### Ramanathan: datasets from Ramanathan's Introductory Econometrics
"data2-1","SAT scores"  
"data2-2","College and high school GPAs"  
"data2-3","Unemployment, inflation and wages"  
"data3-1","House prices and sqft"  
"data3-2","Income and health care spending"  
"data3-3","Patents and R&D expenditures"  
"data3-4","Gross Income and Taxes by States"  
"data3-5","Sealing compound shipment data"  
"data3-6","Disposable income and consumption"  
"data3-7","Toyota station wagon repairs"  
"data3-8","Tuition and salary gain for MBAs"  
"data3-9","Return on equity and assets"  
"data3-10","Profits and sales, 27 German companies"  
"data3-11","Professors' salaries at 7 universities"  
"data3-12","Population of the United Kingdom"  
"data3-13","Population of the USA"  
"data3-14","Personal income and travel expenditures"  
"data3-15","U.S. Population and GDP"  
"data4-1","Prices of single-family homes"  
"data4-2","U.S. incomes and consumption"  
"data4-3","Housing starts and their determinants"  
"data4-3a","Housing starts (updated)"  
"data4-4","Demand for bus travel and determinants"  
"data4-5","Women's labor force participation"  
"data4-6","County poverty rates, Calif."  
"data4-7","Deaths due to coronary heart disease"  
"data4-8","Systems in the top 40 TV markets"  
"data4-9","Early retirement and its determinants"  
"data4-10","Parental school choice in the U.S."  
"data4-11","Private housing units authorized"  
"data4-12","Mortality rates across states"  
"data4-13","Factors affecting baseball attendance"  
"data4-14","Tuition and salary gain for MBAs"  
"data4-15","Cross-country data on inequality"  
"data4-16","Private school enrolment and determinants"  
"data4-17","Percentage of population on AFDC, etc."  
"data6-1","Data on cost function"  
"data6-2","White tuna fishery production"  
"data6-3","United Kingdom income and consumption"  
"data6-4","Salary and employment characteristics"  
"data6-5","Softwood harvest in Oregon"  
"data6-6","U.S. farm population"  
"data7-1","Salaries and gender of 49 employees"  
"data7-2","Salary and employment characteristics"  
"data7-3","Sale price of single family homes"  
"data7-4","Women's labor force participation"  
"data7-5","Sealing compound shipment data"  
"data7-6","Poverty rates and determinants"  
"data7-7","Professors' salaries at 7 universities"  
"data7-8","Cross-country data on economic growth"  
"data7-9","First-year GPAs of students"  
"data7-10","Air quality and its determinants"  
"data7-11","Single family houses, prices etc."  
"data7-12","Prices of cars, 1995"  
"data7-13","Families receiving unemployment comp."  
"data7-14","Homicide across States"  
"data7-15","Re-election of congressmen"  
"data7-16","Number of college applications"  
"data7-17","Cross-country inequality data"  
"data7-18","County population differentials, Calif."  
"data7-19","Demand for cigarettes in Turkey"  
"data7-20","NBA players' salaries"  
"data7-21","Population of the United Kingdom"  
"data7-22","Cable systems in 1979 and 1994"  
"data7-23","Cross-country data: education, etc."  
"data7-24","Sale price and characteristics of homes"  
"data7-26","Parental school choice in the United States"  
"data8-1","Professors' salaries at 7 universities"  
"data8-2","Income and travel expenditures"  
"data8-3","Income and health-care spending"  
"data9-1","Demand for ice cream"  
"data9-2","U.S. discount rate, money supply"  
"data9-3","Domestic electricity demand"  
"data9-4","Corporate profits and sales"  
"data9-5","Farm inputs and output"  
"data9-6","Money, income and interest rates"  
"data9-7","New car sales"  
"data9-8","Domestic Revenue Passenger Miles"  
"data9-9","Quarterly data on new car sales"  
"data9-10","Supermarket sales"  
"data9-11","Volume of Stock market shares sold"  
"data9-12","Expenditures on new cars (monthly)"  
"data9-13","Monthly stock-return data, 1990-1998"  
"data10-1","U.S. monetary data (quarterly)"  
"data10-2","Hourly load and temperature data"  
"data10-3","Foreign exchange rate: Germany and U.S."  
"data10-4","U.S. military expenditures"  
"data10-5","Average earnings, U.S. and California"  
"data10-6","U.S. population, money and prices"  
"data10-7","Population of California"  
"data10-8","Exchange rate and determinants, Korea"  
"data11-1","Full-time nonagricultural workers"  
"data12-1","Individuals applying to UCSD med. school"  
"data13-1","Annual U.S. macroeconomic data"  


#### List of additional sample data files  
As of current `gretl` version, these can be downloaded and installed via GUI only. In the `gretl` GUI main window select `file`$\rightarrow$`Open data`$\rightarrow$`Sample files...`. In the pop-up window push  $\boxed{\uparrow\downarrow}$ button. In the next window select the preferable book title, right-click, left-click, restart `gretl`: voilà!  
The appendix is growing, so I'll restrict myself supplying only one listing out of this data sets facility. The choice is purely subjective. I use these data sets at my econometric classes, both in `R` and `gretl`.

##### POE 4th ed.

"andy","Big Andy's Burger Barn"  
"bangla","Price and Area for farmers"  
"beer",**"Mmmmm!"**  
"bond","AA railroad bond yields"  
"br","1080 home sales in Baton Rouge, LA during mid-2005"  
"br2","1080 home sales in Baton Rouge, LA during mid-2005"  
"brumm","Brumm's Money Growth, Output Growth, and Inflation"  
"byd","returns to shares in BrightenYourDay (BYD) Lighting"  
"canada","Canada / U.S. Foreign Exchange Rate"  
"capm4","monthly rates of return"  
"cars","Data on 392 cars taken from consumer choice magazines."  
"cattle","27 annual time series observations"  
"ces","Panel Data for CES production"  
"cespro","Cross section for CES production"  
"ch10","Chapter 10"  
"ch4sim1", "Simulated Data"  
"ch4sim2", "Simulated Data"  
"chard","Production data"  
"cloth","annual time series observations on two clothing firms"  
"cobb","Production data for Cobb-Douglas"  
"cocaine","56 sales of cocaine"  
"coke","Drink Choices"  
"coke_grouped","50 store data"  
"cola","Drink choices in stacked form"  
"cola2","Drink choice, unstacked"  
"commute","Commute times"  
"consumptn","Consumption"  
"cps","labor market data"  
"cps2","labor market data"  
"cps_small","labor market data"  
"cps4","labor market data"  
"cps4_small","labor market data"  
"cps4c_small","labor market data"  
"cps5","labor market data"  
"crime","Crime rates, panel data"  
"csi","Consumer sentiment Index"  
"demand","Food demand"  
"edu_inc","Education and Income "  
"equity","S&P and dividends"  
"euro","Returns to Euro"  
"ex9_13","sales and advertising"  
"ex9_2","sales and advertising"  
"exrate","US/AUS exchange rate"  
"fair4","Fair data"  
"food","Food expenditures and income"  
"fred","Consumption and Income"  
"fullmoon","Emergency room stats"  
"fultonfish","Kathryn Graddy's Fulton Fish Market data"  
"gascar","Gasoline demand in OECD"  
"gasga","Gasoline demand in OECD for two counties"  
"gdp","US and Australian GDP"  
"gfc","Euro Area and US GDP"  
"gold","gold"  
"golf","golf"  
"growth47","GDP growth"  
"grunfeld11","Grunfeld's data"  
"grunfeld2","Subset of Grunfeld's data"  
"hip","Appendix C"  
"homes","Homes and mortgage rate"  
"hwage","subset of Mroz data"  
"infln_wage","Inflation and wage growth"  
"insur","Insurance and Income"  
"inter2","Simulated Sample"  
"ivreg1","Simulated Sample"  
"ivreg2","Simulated Sample"  
"kernel","Draws from a normal and a mixture"  
"lasvegas","Las Vegas real estate"  
"liquor","liquor expenditures"  
"lon1","subsample of london.dat containing only households with 1 child"  
"lon2","subsample of london.dat containing only households with 2 child"  
"london","Richard Blundell, Alan Duncan and Krishna Pendakur"  
"lond_small","Subset of london"  
"manuf","25 annual observations on aggregate quantities for the U.S. manufacturing sector"  
"mc1","simulated data"  
"mc2","simulated data"  
"means","Tom Means"  
"metrics","Performance and econometrics"  
"mexican","Paul Gertler, Manisha Shah and Stefano Bertozzi"  
"mexico","US and Mexico GDP"  
"motel","Motel rates"  
"mroz","Labor markets"  
"nels","National Education Longitudinal Study of 1988 -- 6649 obs"  
"nels_small","National Education Longitudinal Study of 1988 -- 1000 obs"  
"newbroiler","Ag demand"  
"njmin3","New Jersey min wage data from David Card"  
"nls_panel","3580 obs on 716 individuals"  
"nls_panel_devn","Subset of nls_panel10"  
"nls_panel10","10 individuals from NLS panel"  
"nls_panel2","1432 obs from NLS Panel"  
"nls","13,548  obs"  
"oil","Oil Prices"  
"okun","GDP and unemployment"  
"olympics","Medal counts by country"  
"oz","Consumption and Income in OZ"  
"phillips","Unemployement and inflation (changes)"  
"phillips_aus","AUS Phillips curve"  
"pizza4","Pizza expenditures"  
"pubexp","Education expenditures and gdp"  
"qtm","Money and GDP"  
"returns","Stock returns, various indices"  
"rice","Rice Production"  
"savings","Savings and income"  
"share","Time Share prices"  
"sirmans","Mortgage Data"  
"sp","Returns for S&P500"  
"spurious","Random Walk Processes"  
"star","School Performance data"  
"sterling","Euro/Dollar and Sterling/Dollar exchange rates"  
"stockton","Home Price data 1992-96"  
"stockton2","Home Price data"  
"stockton3","Home Price data"  
"stockton4","Home Price data"  
"stockton96","Home Prices"  
"table_c3","Appendix C"  
"table_c4","Appendix C"  
"table2_2","10 samples of food expenditure"  
"term","Difference between 180 and 90 day bank rates"  
"texas","Oil prices and employment"  
"tobit","Censored sample -- simulated"  
"tobitmc","Monte Carlo results"  
"toodyay","Ag production sample"  
"transport","Commute times"  
"truffles","Supply and demand for Truffles"  
"tuna","Weekly data -- 52 obs"  
"tunafish","Tuna data -- 6000 obs"  
"tunafish_small","Tuna data -- 1000 obs"  
"uk","UK CPI"  
"ukpi","UK and Euro price indices"  
"uniform1","Random Uniforms"  
"uniform2","Random Uniforms"  
"uniform3","Random Uniforms"  
"unit","AR and unit root processes"  
"usa","US macro sample"  
"utown","Housing data"  
"vacation","Chicago households"  
"var","Random Walk time series"  
"vec","Random Walk time series"  
"wa-wheat","Western Australian wheat yields"  
"warner","daily returns on shares in Time Warner Inc."


[^1]:
The references for gretl function packages mentioned in this text
are :  
Oleh Komasko, gretl function package tobit_mfx.gfn,  
  <http://ricardo.ecn.wfu.edu/gretl/cgi-bin/current_fnfiles/tobit_mfx.gfn>;   
Oleh Komashko, gretl function package lagreg.zip,    
  <http://ricardo.ecn.wfu.edu/gretl/cgi-bin/current_fnfiles/lagreg.zip>;    
Oleh Komasko, gretl function package a_eff.gfn    
  <http://ricardo.ecn.wfu.edu/gretl/cgi-bin/current_fnfiles/a_eff.gfn>. 