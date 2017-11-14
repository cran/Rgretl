#if(getRversion() >= "2.15.1")  utils::globalVariables("gretl.status")
.grscr <- function(text) strsplit(text,"\n",fixed = TRUE)[[1]]

#.fu <- function() x
.run_grcli <- function (text)
{ path = paste0(tempdir(),.Platform$file.sep)
  fname = paste0(path,"pkgstmsg894658426289294q8.csv")
   startcode = as.numeric(read.csv(fname))
  #print(startcode)
if (startcode > 0)
{
  cat("\nNo valid gretl executable: no output is produced\n")
  return(invisible(NULL))
}
  txt = .grscr(text)
#z = try(gretl.workdir,silent = T)
z = paste0(tempdir(),.Platform$file.sep)
nam = file.path(z,"rfile01356456546.inp")
#print(nam)
fileConn<-file(nam)
writeLines(txt, fileConn)
close(fileConn)
texto = paste("gretlcli -b -e -t",nam)
system(texto,ignore.stderr = TRUE)
file.remove(nam)
zzz = 2.34
}

.onDetach <- function(libpath) 
  
  {path = paste0(tempdir(),.Platform$file.sep)
  filenam = paste0(path,"pkgstmsg894658426289294q8.csv")
  file.remove(filenam)
  
  z = 3.62}
.onAttach <- function(libname, pkgname){
  z2 = suppressWarnings(try(system("gretlcli -e -v -b -t", intern = T,ignore.stderr =T),silent = TRUE))
  path = paste0(tempdir(),.Platform$file.sep)
  filenam = paste0(path,"pkgstmsg894658426289294q8.csv")
  if (class(z2) == "try-error")
  {
    packageStartupMessage("No gretl executable was detected on your system.\nInstall gretl systemwide first, see README for details.")
    x <- 1
    utils::write.csv(x, file = filenam,row.names = F)
  }
  else
  {
    z3 = strsplit(z2[1]," ")[[1]][3]
    if (z3 < "2016c")
    {
      packageStartupMessage("Your current gretl version is older than 2017c. Install a newer version")
      x <- 2
      utils::write.csv(x, file = filenam,row.names = F)
    }
    else
    {
      x <- 0
      utils::write.csv(x, file = filenam,row.names = F)
    }
  }
}

.grmod <- function (text, data = NULL, output = NULL)
  
{ #startcode = as.numeric(read.csv("pkgstmsg894658426289294q8.csv"))
path = paste0(tempdir(),.Platform$file.sep)
fname = paste0(path,"pkgstmsg894658426289294q8.csv")
startcode = as.numeric(read.csv(fname))
 if (startcode > 0)
 {
  cat("\nNo valid gretl executable: no output is produced\n")
   return(invisible(NULL))
 }
  text = .grscr(text)
  #z = try(gretl.workdir,silent = T)
  z = paste0(tempdir(),.Platform$file.sep)
  # ize = all(class(zz) == "try-error")
  # if (ize)
  # {sss = suppressWarnings(system("gretlcli -q 'xrumpelstiltskin'", intern = T))
  # z = sss[grepl("workdir", sss)]
  # z = strsplit(z, "workdir: ")[[1]][2]}
  # else z = zz
  #z = zz
  if (missing(data))
  { nam = file.path(z,"rfile01353454354.inp")
  fileConn <- file(nam)
  writeLines(text, fileConn)
  close(fileConn)
  system("gretlcli -b -t -e nam")
  file.remove(nam)
  ret = list()
  
  }
  
  if (!missing(data))
  { idfrd = FALSE
    if (!is.character(data))
    { idfrd = TRUE
      ists = FALSE
      dattxt = ""
      nnn = deparse(substitute(data))
      #idfrd = is.data.frame(data)
      if (!is.data.frame(data))
      {
        ists = is.ts(data)
        ismts = is.mts(data)
        frd = frequency(data)
        st = start(data)
        data = data.frame(data)
        if (!ismts)
          names(data) = nnn
      }
      else
      {
        ists = is.ts(data[, 1])
        if (ists)
        {
          frd = frequency(data[, 1])
          st = start(data[, 1])
        }
      }
      if (ists)
      {
        if (frd > 1)
          tst = paste0(st[1], ":", st[2])
        else
          tst = st[1]
        dattxt = paste("setobs", frd, tst, " --time-series")
      }
      #newpath = file.path(z,"rfile02353454354.csv")
      newpath = paste0(z,"rfile02353454354.csv")
     # print(newpath)
      write.csv(data, newpath)
      coord = paste("open", newpath, "--quiet")
      #txt2 = c("set verbose off", coord)
      txt2 = c("set echo off","set messages off",coord)
      if (ists)
        txt2 = c(txt2, dattxt)
      txt2 = c(txt2, text)
    }
    else
    {
      #txt2 = c("set verbose off", paste("open", data, "--quiet"))
      c("set echo off","set messages off", paste("open", data, "--quiet"))
      txt2 = c(txt2, text)
    }
    hhh = 1
    if (!missing(output))
    {
      if (output == "std")
      {txt4 = 
        "matrix vcvf = $coeff~$stderr~$vcv
      matrix uhat = {misszero($uhat)}
      matrix yhat = {misszero($yhat)}
      matrix hati = uhat~yhat
      mwrite(vcvf,\"grm_outfile_01234.txt\")
      mwrite(hati,\"grm_outfile_012345.txt\")
      "
      txt4 = .grscr(txt4)  
      #print(txt4)
      #print(grepl("mwrite",txt4))
      fgfg = (1:length(txt4))[grepl("mwrite",txt4)]
      #print(fgfg)
      oupa = paste0("mwrite(vcvf, ","\"",z,"grm_outfile_01234.txt","\"",")")
      #print(oupa)
      txt4[fgfg[1]] = oupa
      oupa2 = paste0("mwrite(hati, ","\"",z,"grm_outfile_012345.txt","\"",")")
      txt4[fgfg[2]] = oupa2
      
      hhh = 2
      txt2 = c(txt2,txt4)
      print(txt2)
      }
      else
      {
        txt3 = paste("matrix rrr =",output)
        oupa = paste0("mwrite(rrr, ","\"",z,"grm_outfile_01234.txt","\"",")")
        txt3 = c(txt3, oupa)
        txt2 = c(txt2,txt3)  
      }
    }
    #print(txt2)
    # fileConn <- file(paste0(z, "rfile01.inp"))
    # writeLines(txt2, fileConn)
    # close(fileConn)
    # system("gretlcli -b -t -e rfile01.inp")
    #nam = file.path(z,"rfile01353454354.inp")
    nam = paste0(z,"rfile01353454354.inp")
    fileConn <- file(nam)
    writeLines(txt2, fileConn)
    close(fileConn)
    largetext = paste("gretlcli -b -t -e",nam)
    #system("gretlcli -b -t -e rfile01353454354.inp")
    system(largetext)
    file.remove(nam)
    if (idfrd) file.remove(newpath)
    if (!missing(output))
    {oufi = file.path(z, "grm_outfile_01234.txt")
    fifi = unname(as.matrix(read.table(oufi,skip = 1)))
    file.remove(oufi)
    if (hhh > 1)
    {
      oufi = file.path(z, "grm_outfile_012345.txt") 
      fifi2 = unname(as.matrix(read.table(oufi,skip = 1)))
      file.remove(oufi)
      #print(head(fifi2))
      ret = list()
      ret$coeff = fifi[,1]
      ret$se = fifi[,2]
      ret$vcov = fifi[,-(1:2)]
      ret$residuals = fifi2[,1]
      ret$fitted = fifi2[,2]
    }
    else
    {
      nam = as.character(substitute(output))
      isdollar = substr(nam,1,1) == "$"
      if (isdollar) nam = substring(nam,2)
      ret = list()
      ret[[1]] = fifi
      names(ret) = nam
    }
    
    }
    else ret = list()
  }
  invisible(ret)
}

.open_gdt <- function(fpath, mkstruct = TRUE)
{  #z = try(gretl.workdir,silent = T)
  #startcode = as.numeric(read.csv("pkgstmsg894658426289294q8.csv"))
  path = paste0(tempdir(),.Platform$file.sep)
  fname = paste0(path,"pkgstmsg894658426289294q8.csv")
  startcode = as.numeric(read.csv(fname))
  if (startcode > 0)
  {
    cat("\nNo valid gretl executable: no output is produced\n")
    return(invisible(NULL))
  }
z = paste0(tempdir(),.Platform$file.sep)
txt1 = paste("open",fpath,"-q")
storep = file.path(z,"grm_outfile_01234.csv")
txt2 = paste("store",storep, "--csv")
#text = c(txt1,"set verbose off",txt2) 
text = c("set echo off","set messages off",txt1,txt2) 
# zz = try(gretl.workdir,silent = T)
# ize = all(class(zz) == "try-error")
# if (ize)
# {sss = suppressWarnings(system("gretlcli -q 'xrumpelstiltskin'", intern = T))
# z = sss[grepl("workdir", sss)]
# z = strsplit(z, "workdir: ")[[1]][2]}
# else z = zz
nam1 = file.path(z, "rfile01353454354.inp")
fileConn <- file(nam1)
writeLines(text, fileConn)
close(fileConn)
#print(text)
#print(nam1)
texto = paste("gretlcli -b -e -t",nam1)
try(system(texto,intern = T),silent = T)
#nam2 = file.path(z, "grm_outfile_01234.csv")
dfr = read.csv(storep)
file.remove(nam1)
file.remove(storep)
if (mkstruct)
{
  if ("obs"%in%names(dfr))
  {
    sobs = as.character(with(dfr,obs[1]))
    quart = strsplit(sobs,"Q")[[1]]
    mon = strsplit(sobs,"M")[[1]]
    if (length(quart) == 2)
    {starty = as.integer(quart[1])
    startp = as.integer(quart[2])
    freq = 4}
    if (length(mon) == 2)
    {starty = as.integer(mon[1])
    startp = as.integer(mon[2])
    freq = 12}
    if ((length(quart) == 2) || (length(mon) == 2))
    {
      smallnms = setdiff(names(dfr),"obs")
      dfr = dfr[smallnms]
      k = length(smallnms)
      for (i in 1L:k)
      {
        dfr[,i] = ts(dfr[,i],start = c(starty,startp),frequency = freq) 
      }
    }
  }
}
invisible(dfr)
}
#ds = open_gdt("denmark.gdt")

#init.gr <- cmpfun(.init.gr)
run_grcli <- cmpfun(.run_grcli)
grmod <- cmpfun(.grmod)
open_gdt <- cmpfun(.open_gdt)
