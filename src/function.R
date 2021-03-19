abrirArquivo <- function(fn = NULL){
  if(is.null(fn)){
    setStatusbar('Escolha o arquivo...')
    fn <- openFileDialog() 
  }
  
  if((length(fn) > 0) && file.exists(fn)){
    setStatusbar('Carregando carteira...')
    n <- gtkNotebookGetNPages(nb)
    for(i in 1:n){
      gtkNotebookRemovePage(nb, 0)
    }

    if(exists('carteira', globalenv())){ rm(carteira) }
    load(fn, envir = globalenv())
    changed <<- FALSE
    setFilename(fn)
    gerenciaRecentes(filename)
    setTitleJanelaPrincipal(filename)
    
    tot <- length(ls(carteira))
    i <- 1
    for(simbolo in ls(carteira)){
      da <- gtkDrawingArea(show=FALSE)
      gSignalConnect(da, 'motion-notify-event', callbackPointerInfo)
      da$setEvents(GdkEventMask['pointer-motion-mask'] + GdkEventMask['pointer-motion-hint-mask'])
      asCairoDevice(da)
      gtkNotebookAppendPage(nb, da, gtkLabel(simbolo))
      if(attr(carteira[[simbolo]], 'manterAtualizado') && checkInternetConnection()){
        z <- getZoomSimbolo(simbolo)
        de <- z[1]
        ate <- as.character(as.Date(Sys.time()))
        getSimbolo(simbolo, de, ate, TRUE)
        changed <<- TRUE
      }
      analise.tecnica(simbolo)
      da$show()
      setStatusbar('Carregando carteira: %s%%', 100*i/tot)
      Sys.sleep(.5)
    }
  } else {
    setStatusbar(paste('Carregamento cancelado.', 'Arquivo inexistente.', collapse=' '))
  }
}

analise.tecnica <- function(simbolo, da){
  Sys.sleep(.1)
  de <- as.Date(attr(carteira[[simbolo]], 'zoom.de'))
  ate <- as.Date(attr(carteira[[simbolo]], 'zoom.ate'))
  
  # Suporte e Resistência
  p <- pivot(carteira[[simbolo]])
  attr(carteira[[simbolo]],'pivot') <- p

  estudos <- ''
  
  if(exists('statusEstudos')){
    if(statusEstudos['opcaoVF']){
      estudos <- sprintf("%s;addVo()", estudos)
    }
    if(statusEstudos['opcaoSupRes']){
      estudos <- sprintf("%s;addTA(Resistencia1(), on = 1, col='red', lwd=2);addTA(Suporte1(), on = 1, col='green', lwd=2)", estudos)
    }
    if(statusEstudos['opcaoBB']){
      estudos <- sprintf("%s;addTA(xts(matrix(BBands(HLC(quantmod:::get.current.chob()@xdata),n=20)[,c(1,3)], ncol=2,dimnames=list(NULL,c('Bollinger Sup 20','Bollinger Inf 20'))),index(quantmod:::get.current.chob()@xdata)), on = 1, col=c('gray','gray'), lwd=c(2,2))", estudos)
    }
    if(statusEstudos['opcaoMME']){
      estudos <- sprintf("%s;addEMA(9, col = 'cyan');addEMA(21, col = 'red')", estudos)
    }
    if(statusEstudos['opcaoMMVF'] && statusEstudos['opcaoVF']){
      estudos <- sprintf("%s;addTA(SMA(Vo(quantmod:::get.current.chob()@xdata)/quantmod:::get.current.chob()@passed.args$TA[[1]]@params$vol.scale[[1]], n = 21), legend = 'MMA 21', on = 2, col=c('cyan'), lwd=2)", estudos)
    }
    if(statusEstudos['opcaoIFR']){
      estudos <- sprintf("%s;addTA(RSI(Cl(quantmod:::get.current.chob()@xdata),n=2), yrange = c(0,100), legend = paste(paste(rep(' ', 30), collapse=''), 'IFR 2'),col='blue', lwd=2);addTA(ifr.lim.sup(), legend='', on = 3, col='red');addTA(ifr.lim.med(), legend='',  on = 3, col='black');addTA(ifr.lim.inf(), legend='', on = 3, col='green')", estudos)
    }
    if(statusEstudos['opcaoChAD']){
      estudos <- sprintf("%s;addChAD()", estudos)
    }
    if(statusEstudos['opcaoDPO']){
      estudos <- sprintf("%s;addDPO()", estudos)
    }
    
    if(estudos == ''){ estudos <- NULL }
    
    estudos <- gsub('^ *;', '', estudos)
  } else {
    estudos <- "addVo();addTA(Resistencia1(), on = 1, col='red', lwd=2);addTA(Suporte1(), on = 1, col='green', lwd=2);addTA(xts(matrix(BBands(HLC(quantmod:::get.current.chob()@xdata),n=20)[,c(1,3)], ncol=2,dimnames=list(NULL,c('Bollinger Sup 20','Bollinger Inf 20'))),index(quantmod:::get.current.chob()@xdata)), on = 1, col=c('gray','gray'), lwd=c(2,2));addEMA(9, col = 'cyan');addEMA(21, col = 'red');addTA(SMA(Vo(quantmod:::get.current.chob()@xdata)/quantmod:::get.current.chob()@passed.args$TA[[1]]@params$vol.scale[[1]], n = 21), legend = 'MMA 21', on = 2, col=c('cyan'), lwd=2);addTA(RSI(Cl(quantmod:::get.current.chob()@xdata),n=2), yrange = c(0,100), legend = paste(paste(rep(' ', 30), collapse=''), 'IFR 2'),col='blue', lwd=2);addTA(ifr.lim.sup(), legend='', on = 3, col='red');addTA(ifr.lim.med(), legend='',  on = 3, col='black');addTA(ifr.lim.inf(), legend='', on = 3, col='green');addChAD();"
  }

  chartSeries(carteira[[simbolo]], type='candlesticks', theme = 'white', multi.col=FALSE, show.grid=FALSE, name = simbolo,
              subset=paste(de,ate, sep='::'),
              TA=estudos)
  gdkWindowSetCursor(gtkWidgetGetWindow(da),gdkCursorNewFromName(gtkWidgetGetDisplay(da), "default"))
}

checkInternetConnection <- function(){
  #!is.null(suppressWarnings(nsl('br.finance.yahoo.com')))
  TRUE
}

gerenciaRecentes <- function(fn){
  submenu <- gtkMenuItemGetSubmenu(gtkMenuGetAttachWidget(subMenuCarteiraRecentes))
  
  insert.pos <- 0
  
  for(i in 1:length(submenu$getChildren())){
    if(submenu[[i]]$label == fn){
      insert.pos <- i
      break;
    }
  }

  mi <- gtkImageMenuItem(fn)
  subMenuCarteiraRecentes$insert(mi,0)
  
  gSignalConnect(mi, 'activate', function(...){ abrirArquivo(fn) })
  
  if(insert.pos != 0){
    submenu[[insert.pos+1]]$destroy()
  }
  
  while(length(submenu$getChildren()) > (NRECENTES+2)){
    submenu[[NRECENTES+1]]$destroy()
  }
}

getCandleByLocator <- function(simbolo, coord){
  usr <- par('usr') # User System Coordinate
  mai <- par('mai') # Inches
  mai <- grconvertX(mai, 'inches', 'user') # Inches -> User System Coordinate
  width <- usr[2]+mai[4]
  de <- as.Date(attr(carteira[[simbolo]], 'zoom.de'))
  ate <- as.Date(attr(carteira[[simbolo]], 'zoom.ate'))
  subset <- carteira[[simbolo]][paste(de, ate,sep='::')]
  nrsub <- nrow(subset)
  nCandle <- as.numeric(nrsub)
  eachCandle <- width/nCandle
  n <- ceiling(coord/eachCandle)
  if(n > nrsub){ n <- nrsub }
  if(n < 1){ n <- 1 }
  subset[n]
}

getManterAtualizado <- function(simbolo){
  attr(carteira[[simbolo]], 'manterAtualizado')
}


getSimbolo <- function(mySymbol, de=as.Date(Sys.time())-180, ate=as.Date(Sys.time()), manterAtualizado=TRUE){
  if(!exists('carteira', envir = globalenv())){ carteira <<- new.env() }
  if(exists(mySymbol, envir = carteira) && manterAtualizado){
    s.from <- as.Date(index(last(carteira[[mySymbol]])))+1
    tmp <- new.env()
    suppressWarnings(getSymbols(mySymbol, from=s.from, auto.assign = TRUE, env = tmp))
    carteira[[mySymbol]] <- rbind(carteira[[mySymbol]], tmp[[mySymbol]])
    rm(tmp)
  } else {
    suppressWarnings(getSymbols(mySymbol, auto.assign = TRUE, env = carteira))
  }
  
  today <- getQuote(mySymbol)#, what = yahooQuote.EOD)
  
  if(index(carteira[[mySymbol]])[nrow(carteira[[mySymbol]])] < as.Date(today[1,1])){
    carteira[[mySymbol]] <- rbind(carteira[[mySymbol]], xts(cbind(today$Open,today$High,today$Low,today$Last,today$Volume,today$Last), as.Date(today[['Trade Time']])))
  }
  setManterAtualizado(mySymbol, manterAtualizado)
  setZoomSimbolo(mySymbol, de, ate)
}

getTopoFundo <- function(simbolo){
  mx.x <- ifelse(Op(carteira[[simbolo]]) < Cl(carteira[[simbolo]]), Cl(carteira[[simbolo]]), Op(carteira[[simbolo]]))
  xp <- cbind(mx.x, lag(mx.x,-1), lag(mx.x,-2))
  xp <- xp[1:(nrow(xp)-2),]
  fp <- ifelse(xp[,2] > xp[,1] & xp[,2] > xp[,3], TRUE, FALSE)
  p <- mx.x[lag(fp),]
  
  mn.x <- ifelse(Op(carteira[[simbolo]]) < Cl(carteira[[simbolo]]), Op(carteira[[simbolo]]), Cl(carteira[[simbolo]]))
  xv <- cbind(mx.x, lag(mx.x,-1), lag(mx.x,-2))
  xv <- xp[1:(nrow(xp)-2),]
  fv <- ifelse(xv[,2] < xv[,1] & xv[,2] < xv[,3], TRUE, FALSE)
  v <- mn.x[lag(fv),]
  
  list(p=p, v=v)
}

getVariance <- function(candle1, candle2){
  m1 <- mean(c(Op(candle1), Cl(candle1)))
  m2 <- mean(c(Op(candle2), Cl(candle2)))
  m <- round(100*(m2-m1)/m2,1)
  mx <- round(100*(max(c(Cl(candle2),Op(candle2)))-min(c(Cl(candle1),Op(candle1))))/max(c(Cl(candle2),Op(candle2))),1)
  mn <- round(100*(min(c(Cl(candle2),Op(candle2)))-max(c(Cl(candle1),Op(candle1))))/min(c(Cl(candle2),Op(candle2))),1)
  c(mn,m,mx)
}

getZoomSimbolo <- function(simbolo){
  c(de=as.Date(attr(carteira[[simbolo]],'zoom.de')), ate=as.Date(attr(carteira[[simbolo]],'zoom.ate')))
}

ifr.lim.inf <- function(){
  idx <- index(quantmod:::get.current.chob()@xdata)
  nrw <- nrow(quantmod:::get.current.chob()@xdata)
  xts(rep(3,nrw), idx) 
}

ifr.lim.med <- function(){
  idx <- index(quantmod:::get.current.chob()@xdata)
  nrw <- nrow(quantmod:::get.current.chob()@xdata)
  xts(rep(50,nrw), idx)
}

ifr.lim.sup <- function(){
  idx <- index(quantmod:::get.current.chob()@xdata)
  nrw <- nrow(quantmod:::get.current.chob()@xdata)
  xts(rep(97,nrw), idx)
}

pivot <- function(a, n=1){
  # Suporte e Resistencia
  a <- na.omit(a)
  z <- ZigZag(cbind(Hi(a),Lo(a)), change = 5, percent = TRUE)
  
  p <- z[findPeaks(lag(z,-1))]
  v <- z[findValleys(lag(z,-1))]
  
  pivot <- as.numeric(Cl(last(a)))

  x <- rbind(p,v)
  dp <- abs(as.numeric(x-pivot))
  dv <- abs(as.numeric(x-pivot))
  
  r1 <- sort(as.numeric(x[dp>.05 & x>pivot]))[1]
  s1 <- sort(as.numeric(x[dv>.05 & x<pivot]), decreasing=TRUE)[1]
  
  r2 <- sort(as.numeric(x[dp>.05 & x>pivot]))[2]
  s2 <- sort(as.numeric(x[dv>.05 & x<pivot]), decreasing=TRUE)[2]
  
  list(r2=r2,r1=r1,pivot=pivot,s1=s1,s2=s2)
}

Resistencia1 <- function(){
  p <- attr(quantmod:::get.current.chob()@xdata, 'pivot')
  idx <- index(quantmod:::get.current.chob()@xdata)
  nrw <- nrow(quantmod:::get.current.chob()@xdata)
  xts(rep(p$r1,nrw), idx)
}

Resistencia2 <- function(){
  p <- attr(quantmod:::get.current.chob()@xdata, 'pivot')
  idx <- index(quantmod:::get.current.chob()@xdata)
  nrw <- nrow(quantmod:::get.current.chob()@xdata)
  xts(rep(p$r2,nrw), idx)
}

Suporte1 <- function(){
  p <- attr(quantmod:::get.current.chob()@xdata, 'pivot')
  idx <- index(quantmod:::get.current.chob()@xdata)
  nrw <- nrow(quantmod:::get.current.chob()@xdata)
  xts(rep(p$s1,nrw), idx)
}

Suporte2 <- function(){
  p <- attr(quantmod:::get.current.chob()@xdata, 'pivot')
  idx <- index(quantmod:::get.current.chob()@xdata)
  nrw <- nrow(quantmod:::get.current.chob()@xdata)
  xts(rep(p$s2,nrw), idx)
}

salvaCarteira <- function(){
  if(length(grep('.RData$', filename)) == 0 ){
    filename <<- paste(filename, '.RData', sep='', collapse='')
  }
  
  save(carteira, file = filename)
  setFilename(filename)
}

setFilename <- function(fn = character(0)){
  filename <<- fn
  setTitleJanelaPrincipal(fn)
}


setManterAtualizado <- function(simbolo, status){
  attr(carteira[[simbolo]], 'manterAtualizado') <- status
}

setStatusbar <- function(fmt, ...){
  msg <- sprintf(fmt, ...)
  gtkStatusbarPop(sb, 0)
  gtkStatusbarPush(sb, 0, msg)
  
  invisible(NULL)
}

setTitleJanelaPrincipal <- function(fn = ''){
  fn <- sub('.RData$', '', fn)
  gtkWindowSetTitle(janelaPrincipal, paste(ifelse(changed, '* ', ''),fn, ' @ Analise Tecnica', sep=''))
}

setZoomSimbolo <- function(simbolo, de, ate){
  attr(carteira[[simbolo]], 'zoom.de') <- as.Date(de)
  attr(carteira[[simbolo]], 'zoom.ate') <- as.Date(ate)
}
