.grscr <- function(text) 
  {ret = strsplit(text,"\n",fixed = TRUE)[[1]]
   ret[ret != ""]}

.fp0 <- function(x) grepl("output=#R",x,fixed=TRUE)&&(substr(x,1,1)!="#")
.fp1 <-Vectorize(.fp0)
.mysub <- function(pattern, replacement, x) sub(pattern, replacement, x, fixed = TRUE)
.mysubs <- Vectorize(.mysub,vectorize.args = c("replacement","x"))

.find_plots <- function(tekst)
{
  ltk = length(tekst)
  ouinds = .fp1(tekst)
  isi = sum(as.integer(ouinds))
  ret <- vector("list", 3)
  names(ret) <- c("script", "nplots", "fnames")
  if (isi == 0)
  {
    ret[[1]] <- tekst
    ret[[2]] <- 0
    return(ret)
  }
  zs = tempfile(rep("plot", isi), fileext = ".png")
  ret$fnames = zs
  whiches = (1:ltk)[ouinds]
  tekst[whiches] <- .mysubs("#R",zs,tekst[whiches])
  ret[[1]] <- tekst
  ret[[2]] <- isi
  return(ret)
}

.find_insample <- function(script, cmd)
{
  .com <- function(x)
    substr(x, 1, 1) == "#"
  .coms <- Vectorize(.com, USE.NAMES = F)
  script = script[!.coms(script)]
  one <- (grepl(cmd, script, fixed = T))
  precmd = paste0("[A-z,0-9,_,-]", cmd)
  postcmd = paste0(cmd, "[A-z,0-9,_,-,(]")
  two <- (!grepl(precmd, script))
  three <- (!grepl(postcmd, script))
  any(one & two & three)
}

.b2l <- function(bu2l)
{
  
  
  ##### keys == names 
  keys =sapply(bu2l,FUN=function(x) attr(x,"key"))
  names(keys) = NULL
  
  #### types == gretl data types
  types =sapply(bu2l,FUN=function(x) attr(x,"type"))
  names(types) = NULL
  
  ##### matrices
  matril = bu2l[(1:length(bu2l))[types%in%"matrix"]]
  lma = length(matril)
  manams = keys[types%in%"matrix"]
  mrows = sapply(matril, FUN = function(x) attr(x[[1]],"rows"))
  names(mrows) = NULL
  mrows = as.integer(mrows)
  mcols = sapply(matril, FUN = function(x) attr(x[[1]],"cols"))
  names(mcols) = NULL
  mcols = as.integer(mcols)
  mdata = unname(lapply(matril,unlist))
  umdata = unlist(mdata)
  umdata2 = substring(umdata,2)
  umdata2l =strsplit(umdata2,"\n")
  #print(umdata2l)
  umdata2lchv = sapply(umdata2l,FUN=function(x) paste(x,collapse=""))
  #print(class(umdata2lchv))
  umdata2lchvl = strsplit(umdata2lchv,split = " ")
  umdata2lchvln = suppressWarnings(sapply(umdata2lchvl,as.numeric))
  names(umdata2lchvln) = manams
  for (i in 1L:lma)
  {
    if (min(mrows[i],mcols[i]) > 1)
      umdata2lchvln[[i]] = matrix(umdata2lchvln[[i]],mrows[i],mcols[i])
    
  }
  ### matrix ret part :umdata2lchvln
  #################################
  
  ############## scalars
  
  scanamas = keys[types%in%c("int","scalar")]
  scail = bu2l[(1:length(bu2l))[types%in%c("int","scalar")]]
  scav = unlist(scail)
  names(scav) = NULL
  scav = substring(scav,2)
  nscav = suppressWarnings(as.numeric(scav))
  names(nscav) = scanamas
  ### scalar ret part: nscav
  
  ########################## series
  
  nser = (1:length(bu2l))[types%in%"series"]
  sernamas = keys[types%in%"series"]
  serail = bu2l[nser]
  serav = unlist(serail)
  names(serav) = NULL
  serav = substring(serav,2)
  lserav = strsplit(serav," ")
  nlserav = suppressWarnings(lapply(lserav,as.numeric))
  names(nlserav) = sernamas
  
  ### series ret part: nlserav
  
  ######################## strings
  nstri = (1:length(bu2l))[types%in%"string"]
  strinamas = keys[types%in%"string"]
  striil = bu2l[nstri]
  striv = unlist(striil)
  names(striv) = strinamas
  
  ############# striv == string ret part
  #c(nscav,umdata2lchvln,nlserav,striv)
  
  nli = (1:length(bu2l))[types%in%"list"]
  linamas = keys[types%in%"list"]
  liil = bu2l[nli]
  liv = unlist(liil)
  liv = substring(liv,2)
  names(liv) = NULL
  lliv = strsplit(liv," ")
  #lliv = sapply(lliv,FUN=function(x) x[-1])
  nlliv = sapply(lliv,as.numeric)
  names(nlliv) = linamas
  #nlliv
  c(nscav,umdata2lchvln,nlserav,striv,nlliv)
}


.find_indir <- function(dir, cmd)
{
  is_by_sample <- sapply(
    dir,
    FUN = function(x)
      .find_insample(x, cmd)
  )
  if (!any(is_by_sample))
    return(character(0))
  dname = paste0(deparse(substitute(dir)), "$")
  paste0(dname, names(is_by_sample)[is_by_sample])
}



.ts2frame <- function(x,name=NULL)
{
  nomatr <- is.null(dim(x))
  if (!nomatr) return(x)
z = data.frame(x)
names(z) = name
z[1] = x
z
}

.merge_data <- function(...)
{
  path = paste0(tempdir(), .Platform$file.sep)
  fname = paste0(path, "pkgstmsg894658426289294q8.csv")
  startcode = as.numeric(read.csv(fname))
  if (startcode > 0)
  {
    cat("\nNo valid gretl executable: no output is produced\n")
    return(invisible(NULL))
  }
  cl <- sapply(list(...),FUN=function(x)is.null(dim(x)))
  dots <- match.call(expand.dots = F)$"..."
  argnames <- sapply(dots, deparse)
  nodims = dots[cl] 
  numnodims = lapply(nodims,FUN=eval)
  names(numnodims) = argnames[cl]
  dims = dots[!cl]
  dims2 = lapply(dims, FUN = function(x) eval(x))
  dims4 = unlist(dims2, recursive=FALSE)
  alls = c(numnodims,dims4)
  nal = names(alls)
  names(alls) = make.unique(nal,sep="_")
  alls
}

.run_grcli <- function (text,
                        input = list(matr = list(),char = character()),
                        output = list(matr = character(),
                                      char = character(),
                                      series = character()),
                        data = NULL)
{
  path = paste0(tempdir(), .Platform$file.sep)
  fname = paste0(path, "pkgstmsg894658426289294q8.csv")
  startcode = as.numeric(read.csv(fname))
  if (startcode > 0)
  {
    cat("\nNo valid gretl executable: no output is produced\n")
    return(invisible(NULL))
  }
  z = paste0(tempdir(), .Platform$file.sep)
  
  
  if (length(text) == 1)
    txt = .grscr(text)
  else
    txt = text
  #txt = .grscr2(text)
  #print(txt)
  teksty <- .find_plots(txt)
  txt <- teksty$script
  nplots <- teksty$nplots
  plot_fnames <- teksty$fnames
  
  
  txtPREF = ""
  if (!missing(data))
    
  {
    if (is.character(data))
    {
      coord = paste("open", data, "--quiet")
      txtPREF = c("set echo off", "set messages off", coord)
      #txt = c(txtPREF, txt)
    }
    else
    {
      idfrd = TRUE
      ists = FALSE
      dattxt = ""
      nnn = deparse(substitute(data))
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
      newpath = paste0(z, "rfile02353454354.csv")
      write.csv(data, newpath)
      coord = paste("open", newpath, "--quiet")
      txtPREF = c("set echo off", "set messages off", coord)
      if (ists)
        txtPREF = c(txtPREF, dattxt)
      #txt = c(txtPREF, txt)
    }
  }
  #print(txt)
  if (all(nchar(txtPREF)==0)) txtPREF = c("set echo off", "set messages off")
  
  if (length(input$char) > 0)
  {chacha = sapply(input$char,FUN = function(x) strsplit(x,"="),USE.NAMES = FALSE)
  #rint(chacha)}
  
  strinames = names(chacha)
  strivalues = unlist(chacha)
  #print(strivalues)
  strivalues = paste0("\"",strivalues,"\"")
  
  txtPREF = c(txtPREF,paste(strinames,"=",strivalues))
  }
  linma <- length(input$matr)
  if (linma > 0)
  { #print(z)
    #print() 
    namixmatri = names(input$matr)
    matrPREF = character(linma)
    
    for (i in 1L:(linma))
    {
      inpathi <- paste0(z,"inma_oLeGkOmAsHk", i,".txt")
      #print(inpathi)
      #print(data.frame(input$matr[i]))
      #write.table(input$matr[i], inpathi)
      #m = input$matr[i]
      m = as.matrix(data.frame(input$matr[i]))
      #print(m)
      
      m1 = matrix(c(nrow(m),ncol(m)),1)
      m1text = paste0("table10",i,".txt")
      write.table(m1, inpathi,row.names = F,col.names=F)
      write.table(m, inpathi,row.names = F,col.names=F,append = T)
      
      
      matrPREF[i] = paste0(namixmatri[i],"=","mread(\"",inpathi,"\")")
      
    }
    #print(matrPREF)
    txtPREF = c(txtPREF,matrPREF)
  }
  
  txt = c(txtPREF, txt)
  nmatr = length(output$matr)
  nchars = length(output$char)
  nsers = length(output$series)
  if (nmatr > 0)
  {
    txt3 = character(2 * nmatr)
    filenamsi = character(nmatr)
    for (i in 1L:(nmatr))
    {
      matrinami = paste0("matrix oLeGkOmAsHkOgavgav_urknet", i)
      txti = paste0(matrinami, " = ", output$matr[i])
      filenami = paste0(
        "mwrite(oLeGkOmAsHkOgavgav_urknet",
        i,
        " ,",
        "\"",
        z,
        "grm_outfile_01234",
        i,
        ".txt",
        "\"",
        ")"
      )
      filenamsi[i] = paste0(z, "grm_outfile_01234", i, ".txt")
      txt3[2 * i - 1] = txti
      txt3[2 * i] = filenami
    }
    txt = c(txt, txt3)
  }
  
  if (nchars > 0)
  {
    txt5 = character(3 * nchars)
    filenamsi2 = character(nchars)
    texti3 = "outfile --close"
    for (i in 1L:(nchars))
    {
      titi = paste0(z, "grmtxt_outfile_01234", i, ".txt")
      filenamsi2[i] = titi
      texti1 = paste("outfile", titi, "--write --quiet")
      texti2 = paste("print", output$char[i])
      txt5[3 * (i - 1) + 1] = texti1
      txt5[3 * (i - 1) + 2] = texti2
      txt5[3 * i] = texti3
    }
    #print(txt5)
    txt = c(txt, txt5)
  }
  
  if (nsers > 0)
  {
    txt17 = "set skip_missing off"
    txt7 = character(2 * nsers)
    filenamsi3 = character(nsers)
    for (i in 1L:(nsers))
    {
      matrinami = paste0("matrix oLeGkOmAsHkOgavgav_urknet", i)
      txti = paste0(matrinami, " = {", output$series[i], "}")
      filenami = paste0(
        "mwrite(oLeGkOmAsHkOgavgav_urknet",
        i,
        " ,",
        "\"",
        z,
        "grmser_outfile_01234",
        i,
        ".txt",
        "\"",
        ")"
      )
      filenamsi3[i] = paste0(z, "grmser_outfile_01234", i, ".txt")
      txt7[2 * i - 1] = txti
      txt7[2 * i] = filenami
    }
    txt27 = "set skip_missing on"
    txt = c(txt, txt17, txt7, txt27)
    #print(txt7)
  }
  nam = file.path(z, "rfile01356456546.inp")
  fileConn <- file(nam)
  writeLines(txt, fileConn)
  close(fileConn)
  texto = paste("gretlcli -b -e -t", nam)
  system(texto, ignore.stderr = TRUE)
  if (nmatr > 0)
  {
    ret = list()
    for (i in 1L:nmatr)
    {
      oufi = filenamsi[i]
      fifi2 = unname(as.matrix(read.table(oufi, skip = 1)))
      if (min(dim(fifi2)) == 1)
        fifi2 = as.numeric(fifi2)
      file.remove(oufi)
      ret[[i]] = fifi2
    }
    names(ret) = output$matr
  }
  #print("gav gav gav")
  if (nchars > 0)
  {
    ret2 = list()
    for (i in 1L:nchars)
    {
      oufi = filenamsi2[i]
      # print(oufi)
      z = readLines(oufi)
      file.remove(oufi)
      ret2[[i]] = z
    }
    if (nmatr == 0)
    {
      ret = ret2
      names(ret) = output$char
    }
    else
    {
      nn = c(names(ret), output$char)
      ret = c(ret, ret2)
      names(ret) = nn
    }
  }
  if (nsers > 0)
  {
    ret3 = list()
    for (i in 1L:nsers)
    {
      oufi = filenamsi3[i]
      fifi2 = unname(as.matrix(read.table(oufi, skip = 1)))
      if (min(dim(fifi2)) == 1)
        fifi2 = as.numeric(fifi2)
      file.remove(oufi)
      ret3[[i]] = fifi2
    }
    #print(ret3)
    if (nmatr + nchars == 0)
    {
      ret = ret3
      names(ret) = output$series
    }
    else
    {
      nn = c(names(ret), output$series)
      ret = c(ret, ret3)
      names(ret) = nn
    }
  }
  file.remove(nam)
  #print(nplots)
  if (nplots > 0)
  {
    #plot_fnames 
    
    img= png::readPNG(plot_fnames[1])
    rimg= grDevices::as.raster(img)
    drimg = dim(rimg)
    mymai = rep(10^-4,4)*rep(c(drimg[2],drimg[1]),2)
    op1 <-par(mai=mymai)
    p1 <- plot(c(0, drimg[2]), c(0, drimg[1]),axes=FALSE,
               type = "n", xlab = "", ylab = "",bty="n",asp=1)
    graphics::rasterImage(rimg, 0, 0, drimg[2], drimg[1])
    file.remove(plot_fnames[1])
    #par(op)
    
    par(op1)
    if (nplots > 1)
    {
      
      grDevices::devAskNewPage(ask = T)
      for (i in 2L:nplots)
      {
        img= png::readPNG(plot_fnames[i])
        rimg= grDevices::as.raster(img)
        drimg = dim(rimg)
        mymai = rep(10^-4,4)*rep(c(drimg[2],drimg[1]),2)
        op <-par(mai=mymai)
        p2<- plot(c(0, drimg[2]), c(0, drimg[1]),axes=FALSE,
                  type = "n", xlab = "", ylab = "",bty="n",asp=1)
        graphics::rasterImage(rimg, 0, 0, drimg[2], drimg[1])
        
      }
      
    }
    par(op1)
    grDevices::devAskNewPage(ask = F)
    
    
  }
  
  if (nmatr + nchars + nsers > 0)
    return(invisible(ret))
  else
    return(invisible(NULL))
  
}


.save_gdt <- function(fname, ...)
{
  # dots: data objects; can be 'data.frame', 'ts', 'mts'
  a_list2 = list(...)
  dots2 <- match.call(expand.dots = F)$"..."
  argnames <- sapply(dots2, deparse)
  ledatas <- length(argnames)
  lenames <- length(fname)
  if (lenames != ledatas)
    stop("Inconsistent quantities of files names and data objects")
  
  names(a_list2) <- argnames
  classes = sapply(a_list2, class)
  ar.classes = as.array(classes)
  isclasses <- Vectorize(
    FUN = function(x)
      any(is.element(x, c(
        "ts", "data.frame", "mts"
      )))
  )(classes)
  bad <- which(!isclasses)
  
  if (length(bad) > 0)
  {
    badname <- argnames[bad]
    badclasses = sapply(
      ar.classes[bad],
      FUN = function(x)
        x[1]
    )
    bc2 <- paste0("('", badclasses, "')")
    txt <- paste0(badname, bc2)
    txt <- paste(txt, collapse = ", ")
    txt2 <- ifelse(length(bad) == 1,
                   "Wrong data object detected: ",
                   "Wrong data objects detected: ")
    txt <- paste0(txt2, txt)
  }
  
  names(a_list2) <- argnames
  isopt = grepl("--", fname)
  fnames = Vectorize(
    FUN = function(x)
      strsplit(x, "--", fixed = T),
    USE.NAMES = FALSE
  )(fname)
  shortfnames = sapply(
    fnames,
    FUN = function(x)
      x[1]
  )
  opts = sapply(
    fnames,
    FUN = function(x)
      ifelse(length(x) > 1,
             paste0(
               "--", paste0(.strstrip(x[-1]), collapse = " --")
             ), "")
  )
  shfn = .strstrip(shortfnames)
  script <- paste0("store ", "\"", shfn, "\"")
  script <- .strstrip(paste(script, opts))
  
  for (i in 1L:ledatas)
   {#print(class(a_list2[[i]]))
    #print(class(a_list2[i])) 
    if (is.null(dim(a_list2[[i]])))
    {
      if (class(a_list2[[i]]) == "ts") .run_grcli2(script[i], data = .ts2frame(a_list2[[i]],argnames[i]))
      else .run_grcli2(script[i], data = data.frame(a_list2[[i]]))
      
      
      }
  else .run_grcli2(script[i], data = a_list2[[i]])}
}


.save_bin <-function(f_name, data_list, overwrite = FALSE, select = NULL)
{
  n <- length(data_list)
  f_name = paste0(f_name," --database")
  if (overwrite) f_name = paste0(f_name," --overwrite") 
  if (is.null(select)) select <- 1L:n
  for (j in select) save_gdt(f_name,data_list[j])
}

..strstrip <- function (x)
{
liposi =  gregexpr(" ", x, fixed=TRUE)
posi = liposi[[1]]
if (posi[1] < 1) return(x)
lex = nchar(x)
noposi = (1:lex)[-posi]
lnoposi = length(noposi)
if (lnoposi == 0) return("")
else return(substr(x,noposi[1],noposi[length(noposi)]))
}
.strstrip <- Vectorize(..strstrip, USE.NAMES = FALSE)


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


.grmod <- function(mdl,data,top = character(0),
                   bottom = character(0),
                   input = list(matr = list(),char = character()),
                   output = list(matr = character(),
                                 char = character(),
                                 series = character()))
{ txt = mdl
# ,
# type = character(0))
# if (length(type)==0) isord <- FALSE
# else isord <- (type == "ordered")
# ismultin = grepl("--multinomial",mdl,fixed = TRUE)
blocktxt = .grscr(mdl)
nblo = length(blocktxt)
if (nblo == 1)
{if (length(top)!=0)
{
  if (length(top) == 1) top = .grscr(top) 
  txt = c(top,txt)
}
  
  cmd = strsplit(mdl,split = " ")[[1]][1]
}
else
{
  lastline = blocktxt[nblo]
  cklines = strsplit(lastline," ")[[1]]
  cklines = cklines[cklines != ""]
  ck1 = (cklines[1] == "end")
  cmd = cklines[2]
  #print(cmd)
  firstline = blocktxt[1]
  ck2 = grepl(cmd,firstline,fixed = TRUE)
  #print(ck2)
  ck3 = TRUE
  if (cmd == "restrict") ck3 = grepl("--full",firstline,fixed = TRUE)
  #print()
  if ((!ck1)||(!ck2)||(!ck3)) stop("wrong model block")
  txt = blocktxt
  if (length(top)!=0)
  {
    if (length(top) == 1) top = .grscr(top) 
    txt = c(top,txt)
  }
  
  
}
#print(cmd)

#print(cmd == "end")
#print(grep("system",mdl))
cksur = (cmd == "end")&&(grepl("system",mdl))
#print(cksur)
if ((cmd != "estimate")&&(!cksur)&&(cmd != "var")&&(cmd != "vecm"))
{buname = tempfile("mbundle",fileext = ".xml")
prepreposttext = c('bwrite($model,"mybund")')
prepreposttext = sub("mybund",buname,prepreposttext,fixed=TRUE)
if (length(bottom) > 0)
{if (length(bottom) == 1)
  postposttext = .grscr(bottom)
else
  postposttext = bottom}
else postposttext = character(0)
txt = c(txt,prepreposttext,postposttext)
z = run_grcli(txt,input=input,output=output,data=data)
bu2 = read_xml(buname)
file.remove(buname)

if (packageVersion("xml2") < "1.2.0") bu23l = as_list(bu2)
else bu23l = as_list(bu2)[[1L]]
#print(bu23l)
zm = .b2l(bu23l)
return(invisible(c(zm,z)))}

if ((cmd != "var")&&(cmd != "vecm"))
{
  if (!cksur)
  {matri = c("$ess","$T","$df","$lnl","$diagtest","$diagpval",
             "$coeff","$stderr", "$vcv","$sysGamma","$sysA","$sysB") }
  else
  {matri = c("$ess","$T","$df","$diagtest","$diagpval",
             "$coeff","$stderr", "$vcv","$sysGamma","$sysB") } 
}


if (cmd == "var")
{
  matri = c("$T","$df","$ncoeff","$lnl","$aic","$bic","$hqc","$coeff",
            "$stderr","$compan","$xtxinv","$pmanteau","$vma","$fevd") 
}

if (cmd == "vecm")
{
  matri = c("$T","$df","$ncoeff","$lnl","$aic","$bic","$hqc","$coeff",
            "$stderr","$compan","$vecGamma","$evals","$jalpha","$jbeta",
            "$jvbeta","$s00","$s11","$s01","$ec","$vma","$fevd") 
}

#seri = c("$uhat","$yhat")
posttxt = c("catch string x_list2347823547825 = varname($xlist)",
            "erro_xlist = $error",
            "if erro_xlist",
            "string x_list2347823547825 = \"empty\"",
            "endif",
            "string y_list2347823547825 = varname($ylist)")
preposttext = c("set warnings off")
posttxt = c(preposttext,posttxt)

if (length(bottom) > 0)
{if (length(bottom) == 1)
  postposttext = .grscr(bottom)
else
  postposttext = bottom}
else postposttext = character(0)
#print(postposttext)
matri = union(matri,output$matr)
chari = union(c("x_list2347823547825","y_list2347823547825"),output$char)
seri = output$series
outpi = list(matr=matri,char=chari,series =seri)
# print(txt)
txt = c(txt,posttxt,postposttext) 
z = run_grcli(txt,input=input,output=outpi,data=data)

nn = gsub("$","",names(z),fixed=TRUE)
nn = gsub("x_list2347823547825","xlist",nn)
nn = gsub("y_list2347823547825","ylist",nn)
nn[nn=="vcv"] = "vcov"
nn[nn=="coeff"] = "coef"
nn[nn=="uhat"] = "residuals"
nn[nn=="yhat"] = "fitted"
names(z) = nn
z$xlist = strsplit(z$xlist,",")[[1]]
invisible(z)

}


.run_grcli2 <- function (text,
                        output = list(matr = character(),
                                      char = character(),
                                      series = character()),
                        data = NULL)
{
  path = paste0(tempdir(), .Platform$file.sep)
  fname = paste0(path, "pkgstmsg894658426289294q8.csv")
  startcode = as.numeric(read.csv(fname))
  if (startcode > 0)
  {
    cat("\nNo valid gretl executable: no output is produced\n")
    return(invisible(NULL))
  }
  z = paste0(tempdir(), .Platform$file.sep)
  if (length(text) == 1)
    txt = .grscr(text)
  else
    txt = text
  
  if (!missing(data))
    
  {
    if (is.character(data))
    {
      coord = paste("open", data, "--quiet")
      txtPREF = c("set echo off", "set messages off", coord)
      txt = c(txtPREF, txt)
    }
    else
    {
      idfrd = TRUE
      ists = FALSE
      dattxt = ""
      nnn = deparse(substitute(data))
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
      newpath = paste0(z, "rfile02353454354.csv")
      write.csv(data, newpath)
      coord = paste("open", newpath, "--quiet")
      txtPREF = c("set echo off", "set messages off", coord)
      if (ists)
        txtPREF = c(txtPREF, dattxt)
      txt = c(txtPREF, txt)
    }
  }
  
  nmatr = length(output$matr)
  nchars = length(output$char)
  nsers = length(output$series)
  if (nmatr > 0)
  {
    txt3 = character(2 * nmatr)
    filenamsi = character(nmatr)
    for (i in 1L:(nmatr))
    {
      matrinami = paste0("matrix oLeGkOmAsHkOgavgav_urknet", i)
      txti = paste0(matrinami, " = ", output$matr[i])
      filenami = paste0(
        "mwrite(oLeGkOmAsHkOgavgav_urknet",
        i,
        " ,",
        "\"",
        z,
        "grm_outfile_01234",
        i,
        ".txt",
        "\"",
        ")"
      )
      filenamsi[i] = paste0(z, "grm_outfile_01234", i, ".txt")
      txt3[2 * i - 1] = txti
      txt3[2 * i] = filenami
    }
    txt = c(txt, txt3)
  }
  
  if (nchars > 0)
  {
    txt5 = character(3 * nchars)
    filenamsi2 = character(nchars)
    texti3 = "outfile --close"
    for (i in 1L:(nchars))
    {
      titi = paste0(z, "grmtxt_outfile_01234", i, ".txt")
      filenamsi2[i] = titi
      texti1 = paste("outfile", titi, "--write --quiet")
      texti2 = paste("print", output$char[i])
      txt5[3 * (i - 1) + 1] = texti1
      txt5[3 * (i - 1) + 2] = texti2
      txt5[3 * i] = texti3
    }
    #print(txt5)
    txt = c(txt, txt5)
  }
  
  if (nsers > 0)
  {
    txt17 = "set skip_missing off"
    txt7 = character(2 * nsers)
    filenamsi3 = character(nsers)
    for (i in 1L:(nsers))
    {
      matrinami = paste0("matrix oLeGkOmAsHkOgavgav_urknet", i)
      txti = paste0(matrinami, " = {", output$series[i], "}")
      filenami = paste0(
        "mwrite(oLeGkOmAsHkOgavgav_urknet",
        i,
        " ,",
        "\"",
        z,
        "grmser_outfile_01234",
        i,
        ".txt",
        "\"",
        ")"
      )
      filenamsi3[i] = paste0(z, "grmser_outfile_01234", i, ".txt")
      txt7[2 * i - 1] = txti
      txt7[2 * i] = filenami
    }
    txt27 = "set skip_missing on"
    txt = c(txt, txt17, txt7, txt27)
    #print(txt7)
  }
  nam = file.path(z, "rfile01356456546.inp")
  fileConn <- file(nam)
  writeLines(txt, fileConn)
  close(fileConn)
  texto = paste("gretlcli -b -e -t", nam)
  system(texto, ignore.stderr = TRUE, intern = TRUE)
  if (nmatr > 0)
  {
    ret = list()
    for (i in 1L:nmatr)
    {
      oufi = filenamsi[i]
      fifi2 = unname(as.matrix(read.table(oufi, skip = 1)))
      if (min(dim(fifi2)) == 1)
        fifi2 = as.numeric(fifi2)
      file.remove(oufi)
      ret[[i]] = fifi2
    }
    names(ret) = output$matr
  }
  #print("gav gav gav")
  if (nchars > 0)
  {
    ret2 = list()
    for (i in 1L:nchars)
    {
      oufi = filenamsi2[i]
      # print(oufi)
      z = readLines(oufi)
      file.remove(oufi)
      ret2[[i]] = z
    }
    if (nmatr == 0)
    {
      ret = ret2
      names(ret) = output$char
    }
    else
    {
      nn = c(names(ret), output$char)
      ret = c(ret, ret2)
      names(ret) = nn
    }
  }
  if (nsers > 0)
  {
    ret3 = list()
    for (i in 1L:nsers)
    {
      oufi = filenamsi3[i]
      fifi2 = unname(as.matrix(read.table(oufi, skip = 1)))
      if (min(dim(fifi2)) == 1)
        fifi2 = as.numeric(fifi2)
      file.remove(oufi)
      ret3[[i]] = fifi2
    }
    #print(ret3)
    if (nmatr + nchars == 0)
    {
      ret = ret3
      names(ret) = output$series
    }
    else
    {
      nn = c(names(ret), output$series)
      ret = c(ret, ret3)
      names(ret) = nn
    }
  }
  file.remove(nam)
  if (nmatr + nchars + nsers > 0)
    return(invisible(ret))
  else
    return(invisible(NULL))
  
}


.open_gdt <- function(fpath, mkstruct = TRUE, info = TRUE)
{   path = paste0(tempdir(),.Platform$file.sep)
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
# storep = path to store
nmatr = 2
new_m_nami = c("$pd","$datatype")
txt = ""
txt3 = character(2 * nmatr)
filenamsi = character(nmatr)
for (i in 1L:(nmatr))
{
  matrinami = paste0("matrix oLeGkOmAsHkOgavgav_urknet", i)
  txti = paste0(matrinami, " = ", new_m_nami[i])
  filenami = paste0(
    "mwrite(oLeGkOmAsHkOgavgav_urknet",
    i,
    " ,",
    "\"",
    z,
    "grm_outfile_01234",
    i,
    ".txt",
    "\"",
    ")"
  )
  filenamsi[i] = paste0(z, "grm_outfile_01234", i, ".txt")
  txt3[2 * i - 1] = txti
  txt3[2 * i] = filenami
}
txt = c(txt, txt3)


nchars = 1
txt3 = c(txt3,"OlEhStRiNg = obslabel($t1)")
#print(txt3)
outputchar = "OlEhStRiNg"

txt5 = character(3 * nchars)
filenamsi2 = character(nchars)
texti3 = "outfile --close"
for (i in 1L:(nchars))
{
  titi = paste0(z, "grmtxt_outfile_01234", i, ".txt")
  filenamsi2[i] = titi
  texti1 = paste("outfile", titi, "--write --quiet")
  texti2 = paste("print", outputchar[i])
  txt5[3 * (i - 1) + 1] = texti1
  txt5[3 * (i - 1) + 2] = texti2
  txt5[3 * i] = texti3
}

# print("z:")
# cat(z,"\n")


#infopath = paste0(z,oLeGkOmAsH_info_321.txt)


txt2 = paste("store",storep, "--csv")
#text = c("set echo off","set messages off",txt1,txt3,txt5,txt2) 

#infopath = "denmark.txt"
if (info)
{infopath = paste0(z,"oLeGkOmAsH_info_321.txt")
line00 <- c('sLHOoijJOIji67243 = ""')
line001 <- c(line00,paste("outfile",infopath,"--write --quiet"))
txt0 = c(line001,"info","print sLHOoijJOIji67243","labels","outfile --close")
text = c("set echo off","set messages off",txt1,txt0,txt3,txt5,txt2)
}
else text = c("set echo off","set messages off",txt1,txt3,txt5,txt2) 


nam1 = file.path(z, "rfile01353454354.inp")
fileConn <- file(nam1)
writeLines(text, fileConn)
#print(text)
close(fileConn)
texto = paste("gretlcli -b -e -t",nam1)
system(texto,intern = TRUE,ignore.stdout =TRUE,ignore.stderr=TRUE)
#?system
#print("gavgavggav")

dfr <- read.csv(storep)
file.remove(nam1)
file.remove(storep)
oufiinfo = infopath
# print(oufi)
descri = readLines(oufiinfo)
file.remove(oufiinfo)
#print(descri)
#cat(descri)


if (nmatr > 0)
{
  ret = list()
  for (i in 1L:nmatr)
  {
    oufi = filenamsi[i]
    fifi2 = unname(as.matrix(read.table(oufi, skip = 1)))
    if (min(dim(fifi2)) == 1)
      fifi2 = as.numeric(fifi2)
    file.remove(oufi)
    ret[[i]] = fifi2
  }
  names(ret) = c("freq","datatype")
}
#print(ret)

if (nchars > 0)
{
  ret2 = list()
  for (i in 1L:nchars)
  {
    oufi = filenamsi2[i]
    # print(oufi)
    z = readLines(oufi)
    file.remove(oufi)
    ret2[[i]] = z
  }
  
  nn = c(names(ret), "fio")
  ret = c(ret, ret2)
  names(ret) = nn
  
}
#print(nn)
if (mkstruct)
{
  if (ret$datatype == 2)
  { if (!grepl("-",ret$fio))
  {if (ret$freq  > 1)
  {starts = strsplit(ret$fio,":")[[1]]
  if (length(starts)==2)
  {starty <- as.integer(starts[1])
  
  startp <- as.integer(starts[2])}
  if (length(starts)==1)
  {starty <- 0L
  startp <- as.integer(starts[1])
  }
  
  
  }
    else
    {
      starty <- as.integer(ret$fio)
      startp <- as.integer(1)  
    }
    smallnms = setdiff(names(dfr),"obs")
    dfr = dfr[smallnms]
    k = length(smallnms)
    for (i in 1L:k)
    {
      dfr[,i] = ts(dfr[,i],start = c(starty,startp),frequency = ret$freq)
    }
  }
    else
    {
      
      if (ret$freq ==52)
      {
        
        starty <- lubridate::year(ret$fio)
        startp <- lubridate::week(ret$fio)
        smallnms = setdiff(names(dfr),"obs")
        dfr = dfr[smallnms]
        k = length(smallnms)
        for (i in 1L:k)
        {
          dfr[,i] = ts(dfr[,i],start = c(starty,startp),frequency = ret$freq)
        }
        
      }
      
    }
  }
  
}
if (info) attributes(dfr)$description <-invisible(descri)
invisible(dfr)
}


datasets_info <-function(directory=NULL,prn = NULL)
{ nodir = is.null(directory)
  #nofile = is.null(file.name)
#print(nodir)
#print(nofile)
dirs = c("Gretl","Greene","Ramanathan")
if (nodir)
{
if (is.null(prn)||any(prn%in%1:3))
  cat("\n","Available data directories are",'"Gretl",','"Greene",',"and",'"Ramanathan"',"\n\n" )
  return(invisible(dirs))
}
trydir <- directory%in%dirs
trydir2 <- which(directory==dirs)
#print(trydir)
if (!trydir) stop("Wrong directory name.\n Run 'datasets_info()' for the list of available directories")
#print(trydir2)
if (trydir2 == 2)
{green.script <-
' Greene: selected data sets from William Greene\'s Econometric Analysis
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
"greene25_1","Expenditure and credit-rating data"'
backup = green.script
green.script = gsub('"',"",green.script)
for (i in 0:9)
{green.script = gsub(paste0(i,","),paste0(i,": "),green.script)}
green.script = gsub(",",", ",green.script,fixed =T)
green.script = gsub("  "," ",green.script)
te = .grscr(green.script) 
te2 = paste0("\n",.strstrip(te))


if (is.null(prn)) cat(te2,"\n\n")
else 
{
    select = intersect(prn,1:(length(te2)-1))
    isselect = any(prn%in%(1:(length(te2)-1)))
if (isselect) cat(te2[(select+1)],"\n\n")

}

te3 = .grscr(backup) 
greenes = strsplit(te3[-1],split=",")
sg = sapply(greenes, FUN=function(x) x[1])
sg = gsub("\"","",sg)
return(invisible(sg))} 



if (trydir2 == 1)
{green.script <-
    'Gretl: various illustrative datafiles
"abdata","Arellano and Bond (1991) panel data"
"anscombe","Anscombe\'s Quartet (artificial data)"
"arma","Artificial data for ARMA script example"
"australia","Johansen\'s Australian macroeconomic data"
"AWM","Euro area macroeconomic data, 1970-1998"
"AWM17","Euro area macroeconomic data, 1970-2016"
"banks91","Costs of Italian commercial banks"
"b-g","Bollerslev and Ghysels exchange rate data"
"bjg","Box and Jenkins Series G (airline passengers)"
"brand_cassola","Euro area money demand data"
"broiler","Epple and McCallum broiler chicken data"
"CEL","Caselli, Esquivel and Lefort growth panel data"
"chomage","D.S.G. Pollock\'s Swiss unemployment data"
"credscore","Greene\'s credit scoring data"
"denmark","Johansen\'s Danish macroeconomic data"
"djclose","Daily close of Dow-Jones, 1980s"
"ects_nls","Nonlinear least squares example"
"engel","Engel\'s income and food expenditure data"
"galton","Francis Galton\'s heights data"
"gdp_midas","MIDAS data, quarterly GDP and monthly covariates"
"gear","NIST data for variance tests"
"griliches","Wages of young men and their determinants"
"hall","Consumption and asset returns, U.S."
"hamilton","Prices and exchange rate, U.S. and Italy"
"hendry_jae","David Hendry, JAE 2001, UK macro data"
"jgm-data","Interest rates and inflation, Canada"
"keane","Keane and Wolpin career choice data"
"kennan","Duration of contract strikes, U.S."
"klein","US macro data for Klein\'s Model 1"
"kmenta","US macro data plus artificial data"
"longley","Annual U.S. labor-market data"
"leverage","Illustrates detection of incorrect data"
"mccullagh","Ship damage data used by William Greene"
"mroz87","Women\'s labor force participation and pay"
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
"theil","Henri Theil\'s textile consumption data"
"ukppp","Johansen and Juselius (1992) PPP data"
"union_wooldridge","Unionization of workers in the US"
"wgmacro","West German macro data from Helmut Lutkepohl"
"wtp","Willingness to pay data from Verbeek"'

# bubu = strsplit(green.script,split=",")
# bubutail = bubu[-1]
# print(bubu)

backup = green.script
green.script = gsub('"',"",green.script)

# for (i in 0:9)
# {green.script = gsub(paste0(i,","),paste0(i,": "),green.script)}
# green.script = gsub(",",", ",green.script,fixed =T)
# green.script = gsub("  "," ",green.script)
te = .grscr(green.script) 
te = sub(",",": ",te)
te2 = paste0("\n",.strstrip(te))

#if (prn) cat(te2,"\n\n")
if (is.null(prn)) cat(te2,"\n\n")
else 
{
  select = intersect(prn,1:(length(te2)-1))
  isselect = any(prn%in%(1:(length(te2)-1)))
  if (isselect) cat(te2[(select+1)],"\n\n")
  
}


te3 = .grscr(backup) 
greenes = strsplit(te3[-1],split=",")
sg = sapply(greenes, FUN=function(x) x[1])
sg = gsub("\"","",sg)
return(invisible(sg))} 

if (trydir2 == 3)
{green.script <-
  'Ramanathan: datasets from Ramanathan\'s Introductory Econometrics
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
"data3-11","Professors\' salaries at 7 universities"
"data3-12","Population of the United Kingdom"
"data3-13","Population of the USA"
"data3-14","Personal income and travel expenditures"
"data3-15","U.S. Population and GDP"
"data4-1","Prices of single-family homes"
"data4-2","U.S. incomes and consumption"
"data4-3","Housing starts and their determinants"
"data4-3a","Housing starts (updated)"
"data4-4","Demand for bus travel and determinants"
"data4-5","Women\'s labor force participation"
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
"data7-4","Women\'s labor force participation"
"data7-5","Sealing compound shipment data"
"data7-6","Poverty rates and determinants"
"data7-7","Professors\' salaries at 7 universities"
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
"data7-20","NBA players\' salaries"
"data7-21","Population of the United Kingdom"
"data7-22","Cable systems in 1979 and 1994"
"data7-23","Cross-country data: education, etc."
"data7-24","Sale price and characteristics of homes"
"data7-26","Parental school choice in the United States"
"data8-1","Professors\' salaries at 7 universities"
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
"data13-1","Annual U.S. macroeconomic data"'

# bubu = strsplit(green.script,split=",")
# bubutail = bubu[-1]
# print(bubu)

backup = green.script
green.script = gsub('"',"",green.script)

# for (i in 0:9)
# {green.script = gsub(paste0(i,","),paste0(i,": "),green.script)}
# green.script = gsub(",",", ",green.script,fixed =T)
# green.script = gsub("  "," ",green.script)
te = .grscr(green.script)

te = sub(",",": ",te)
te2 = paste0("\n",.strstrip(te))

#if (prn) cat(te2,"\n\n")
if (is.null(prn)) cat(te2,"\n\n")
else 
{
  select = intersect(prn,1:(length(te2)-1))
  isselect = any(prn%in%(1:(length(te2)-1)))
  if (isselect) cat(te2[(select+1)],"\n\n")
  
}


te3 = .grscr(backup) 
greenes = strsplit(te3[-1],split=",")
sg = sapply(greenes, FUN=function(x) x[1])
sg = gsub("\"","",sg)
return(invisible(sg))} 



}  

description <- function(x)
{
  if (!"description"%in%names(attributes(x))) return(character(0))
  else
  {cat("\n",paste0(attributes(x)$description,"\n"))
    
    return(invisible(attributes(x)$description)) }
}

find_sample <- function(cmd,dir = NULL)
{
  gretl = eval(parse(text="gretl"))
  greene = eval(parse(text="greene"))
  ramanathan = eval(parse(text="ramanathan"))
if (is.null(dir))
{#data("gretl","greene","ramanathan",package="Rgretl")
  return(c(.find_indir(gretl,cmd),.find_indir(greene,cmd),.find_indir(ramanathan,cmd)))
}
  whichis = (1:3)[dir==c("gretl","greene","ramanathan")] 
  if (whichis == 1)
  {
    #data("gretl",package="Rgretl")
    return(.find_indir(gretl,cmd))
  }
  if (whichis == 2)
  {
    #data("greene",package="Rgretl")
    return(.find_indir(greene,cmd))
  }
  
  if (whichis == 3)
  {
    #data("ramanathan",package="Rgretl")
    return(.find_indir(ramanathan,cmd))
  }
  return(character(0))
  
}

show_sample <- function(x)
{
  if (length(x) == 1) x = eval(parse(text=x))
  cat(paste0(x,"\n"))
  invisible(x)
}

save_bin <- cmpfun(.save_bin)
merge_data <- cmpfun(.merge_data)
save_gdt <- cmpfun(.save_gdt)
run_grcli <- cmpfun(.run_grcli)
grmod <- cmpfun(.grmod)
open_gdt <- cmpfun(.open_gdt)
