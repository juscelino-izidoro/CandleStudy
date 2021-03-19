source('function.R')

callbackCarteiraNova <- function(...){
  setStatusbar('Preparando nova carteira.')
  if(changed){ 
    resp <- alertBox("Houve alterações na carteira. Deseja salva-las?", buttons='yes-no', type='question')
    if(resp == GtkResponseType['yes']){
      callbackCarteiraSalvar(...)
    } 
  }
  
  changed <<- FALSE
  if(exists('carteira', globalenv())){ rm(carteira) }
  carteira <<- new.env()
  setFilename(character(0))
  
  n <- gtkNotebookGetNPages(nb)
  for(i in 1:n){
    gtkNotebookRemovePage(nb, 0)
  }
  
  setStatusbar('Pronto.')
}

callbackCarteiraAbrir <- function(...){
  abrirArquivo()
}

callbackCarteiraSalvar <- function(...){
  if(length(filename) == 0){ 
    callbackCarteiraSalvarComo(...)
  } else {
    salvaCarteira()
    changed <<- FALSE
  }
}

callbackCarteiraSalvarComo <- function(...){
  setFilename(saveFileDialog())
  if(length(filename) > 0){
    salvaCarteira()
    changed <<- FALSE
    gerenciaRecentes(filename)
  }
}

callbackCarteiraRecentesLimpar <- function(...){ 
  submenu <- gtkMenuItemGetSubmenu(gtkMenuGetAttachWidget(subMenuCarteiraRecentes))
  
  while(length(submenu$getChildren()) > 2){
    submenu[[1]]$destroy()
  }
}

callbackCarteiraSair <- function(...){ 
  cancel <- FALSE
  resp <- GtkResponseType['none']
  if(changed){ 
    resp <- msgBox("Houve alterações na carteira. Deseja salva-las antes de sair?", title = 'Salvar', parent = janelaPrincipal, 'gtk-yes', GtkResponseType['yes'], 'gtk-no', GtkResponseType['no'], 'gtk-cancel', GtkResponseType['cancel'])
    if(resp == GtkResponseType['yes']){ 
      callbackCarteiraSalvar()
    }
  }

  if(resp != GtkResponseType['cancel']){
    if(gtkMainLevel() > 0){
      gtkMainQuit()
    }
  }
  
  janelaPrincipal$destroy()
}

callbackEstudoMudaStatus <- function(w){
  if(w == opcaoSupRes){
    statusEstudos['opcaoSupRes'] <<- gtkCheckMenuItemGetActive(opcaoSupRes)
  } else if(w == opcaoBB){
    statusEstudos['opcaoBB'] <<- gtkCheckMenuItemGetActive(opcaoBB)
  } else if(w == opcaoMME){
    statusEstudos['opcaoMME'] <<- gtkCheckMenuItemGetActive(opcaoMME)
  } else if(w == opcaoVF){
    statusEstudos['opcaoVF'] <<- gtkCheckMenuItemGetActive(opcaoVF)
  } else if(w == opcaoMMVF){
    statusEstudos['opcaoMMVF'] <<- gtkCheckMenuItemGetActive(opcaoMMVF)
  } else if(w == opcaoIFR){
    statusEstudos['opcaoIFR'] <<- gtkCheckMenuItemGetActive(opcaoIFR)
  } else if(w == opcaoChAD){
    statusEstudos['opcaoChAD'] <<- gtkCheckMenuItemGetActive(opcaoChAD)
  } else if(w == opcaoDPO){
    statusEstudos['opcaoDPO'] <<- gtkCheckMenuItemGetActive(opcaoDPO)
  }
  
  n <- gtkNotebookGetCurrentPage(nb)
  if(n >= 0){
    da <- gtkNotebookGetNthPage(nb, n)
    asCairoDevice(da)
    simbolo <- gtkNotebookGetTabLabelText(nb, da)
    analise.tecnica(simbolo,da)
  }
}

callbackJanelaPrincipalActivate <- function(...){
  if(file.exists('~/AnaliseTecnica.RData')){
    load('~/AnaliseTecnica.RData', envir = globalenv())
  }
  
  if(exists('arquivos', globalenv())){
    for(a in arquivos){
      gerenciaRecentes(a)
    }
  }
  
  if(!exists('statusEstudos', globalenv())){
    statusEstudos <<- c(opcaoSupRes=TRUE, opcaoBB=TRUE, opcaoMME=TRUE,opcaoVF=TRUE,opcaoMMVF=TRUE,opcaoIFR=TRUE,opcaoChAD=TRUE,opcaoDPO=FALSE)
  }
  
  gtkCheckMenuItemSetActive(opcaoSupRes, statusEstudos['opcaoSupRes'])
  gtkCheckMenuItemSetActive(opcaoBB, statusEstudos['opcaoBB'])
  gtkCheckMenuItemSetActive(opcaoMME, statusEstudos['opcaoMME'])
  gtkCheckMenuItemSetActive(opcaoVF, statusEstudos['opcaoVF'])
  gtkCheckMenuItemSetActive(opcaoMMVF, statusEstudos['opcaoMMVF'])
  gtkCheckMenuItemSetActive(opcaoIFR, statusEstudos['opcaoIFR'])
  gtkCheckMenuItemSetActive(opcaoChAD, statusEstudos['opcaoChAD'])
  gtkCheckMenuItemSetActive(opcaoDPO, statusEstudos['opcaoDPO'])
}

callbackJanelaPrincipalDestroy <- function(...){
  if(changed){
    callbackCarteiraSalvarComo(...)
  }
  
  submenu <- gtkMenuItemGetSubmenu(gtkMenuGetAttachWidget(subMenuCarteiraRecentes))
  len <- length(submenu$getChildren())-2
  
  arquivos <- c()
  if(len > 0){
    for(i in 1:len){
      arquivos <- c(arquivos, submenu[[i]]$label)
    }
  }

  statusEstudos['opcaoSupRes'] <- gtkCheckMenuItemGetActive(opcaoSupRes)
  statusEstudos['opcaoBB'] <- gtkCheckMenuItemGetActive(opcaoBB)
  statusEstudos['opcaoMME'] <- gtkCheckMenuItemGetActive(opcaoMME)
  statusEstudos['opcaoVF'] <- gtkCheckMenuItemGetActive(opcaoVF)
  statusEstudos['opcaoMMVF'] <- gtkCheckMenuItemGetActive(opcaoMMVF)
  statusEstudos['opcaoIFR'] <- gtkCheckMenuItemGetActive(opcaoIFR)
  statusEstudos['opcaoChAD'] <- gtkCheckMenuItemGetActive(opcaoChAD)
  statusEstudos['opcaoDPO'] <- gtkCheckMenuItemGetActive(opcaoDPO)
  
  save(arquivos, statusEstudos, file = "~/AnaliseTecnica.RData")

  if(!interactive()){ gtkMainQuit() }
}

callbackPointerInfo <- function(w,e,u){
  n <- gtkNotebookGetCurrentPage(nb)
  if(n >= 0){
    da <- gtkNotebookGetNthPage(nb, n)
    simbolo <- gtkNotebookGetTabLabelText(nb, da)
    p <- gdkWindowGetPointer(e$window)
    if(exists('x',p)){
      candle <- getCandleByLocator(simbolo,grconvertX(p$x,'device','user'))
      candle0 <- carteira[[simbolo]][(grep(index(candle), index(carteira[[simbolo]]))-1)]
      v <- round(100*(as.numeric(Cl(candle))-as.numeric(Cl(candle0)))/as.numeric(Cl(candle0)),2)
      setStatusbar("Informações do dia %s: Var = %.2f%% ; Min = R$ %s ; Máx = R$ %s ; Ab = R$ %s ; Ult = R$ %s", index(candle[1,]), v, Lo(candle[1,]), Hi(candle[1,]), Op(candle[1,]), Cl(candle[1,]))
    }
  }
}

callbackSimboloAtualiza <- function(...){
  if(checkInternetConnection()){
    n <- gtkNotebookGetCurrentPage(nb)
    if(n >= 0){
      da <- gtkNotebookGetNthPage(nb, n)
      simbolo <- gtkNotebookGetTabLabelText(nb, da)
      setStatusbar('Atualizando %s...', simbolo)
      asCairoDevice(da)
      manterAtualizado <- getManterAtualizado(simbolo)
      z <- getZoomSimbolo(simbolo)
      getSimbolo(simbolo, z[1], z[2], manterAtualizado)
      analise.tecnica(simbolo,da)
      setStatusbar('Símbolo %s atualizado.', simbolo)
      changed <<- TRUE
    }
  } else {
    alertBox('Sem conexão com a internet')
  }
}

callbackSimboloEdita <- function(...){
  if(checkInternetConnection()){
    n <- gtkNotebookGetCurrentPage(nb)
    if(n >= 0){
      da <- gtkNotebookGetNthPage(nb, n)
      simbolo <- gtkNotebookGetTabLabelText(nb, da)
      z <- getZoomSimbolo(simbolo)
      simConfig <- insereSimbolo(simbolo, z[1], z[2])
      if(!is.null(simConfig)){
        with(simConfig, { 
          asCairoDevice(da)
          getSimbolo(simbolo, de, ate, manterAtualizado)
          analise.tecnica(simbolo)
          setStatusbar('Símbolo %s atualizado.', simbolo)
          gtkNotebookSetTabLabelText(nb, da, simbolo)
          gtkNotebookSetCurrentPage(nb, n)
          changed <<- TRUE
        })
      }
    }
  } else {
    alertBox('Sem conexão com a internet')
  }
}

callbackSimboloInsere <- function(...){
  if(checkInternetConnection()){
    simConfig <- insereSimbolo()
    if(!is.null(simConfig)){
      with(simConfig, { 
        setStatusbar('Carregando %s...', simbolo, de, ate)
        da <- gtkDrawingArea(show=FALSE)
        gSignalConnect(da, 'motion-notify-event', callbackPointerInfo)
        da$setEvents(GdkEventMask['pointer-motion-mask'] + GdkEventMask['pointer-motion-hint-mask'])
        asCairoDevice(da)
        da$show()
        gtkNotebookAppendPage(nb, da, gtkLabel(simbolo))
        getSimbolo(simbolo, de, ate, TRUE)
        analise.tecnica(simbolo,da)
        setStatusbar('Símbolo %s carregado', simbolo)
        changed <<- TRUE
        gtkNotebookSetCurrentPage(nb, gtkNotebookGetNPages(nb)-1)
      })
    }
  } else {
    alertBox('Sem conexão com a internet')
  }
}

callbackSimboloRemove <- function(...){
  n <- gtkNotebookGetCurrentPage(nb)
  if(n >= 0){
    da <- gtkNotebookGetNthPage(nb, n)
    simbolo <- gtkNotebookGetTabLabelText(nb, da)
    gtkNotebookRemovePage(nb, n)
    rm(list = simbolo, envir = carteira)
    setStatusbar('Símbolo %s removido.', simbolo)
    changed <<- TRUE
  }
}

callbackToolMoveChartLeft <- function(...){
  n <- gtkNotebookGetCurrentPage(nb)
  if(n >= 0){
    da <- gtkNotebookGetNthPage(nb, n)
    simbolo <- gtkNotebookGetTabLabelText(nb, da)
    de <- as.Date(attr(carteira[[simbolo]],'zoom.de'))
    ate <- as.Date(attr(carteira[[simbolo]],'zoom.ate'))
    primeiro <- index(carteira[[simbolo]][1,])
    if((de-5) >= primeiro){
      de <- de - 5;
      ate <- ate - 5;
    } else {
      n <- de - primeiro
      de <- primeiro
      ate <- ate - n
    }
    attr(carteira[[simbolo]],'zoom.ate') <- as.Date(ate)
    attr(carteira[[simbolo]],'zoom.de') <- as.Date(de)
    analise.tecnica(simbolo,da)
  }
  return(FALSE)
}

callbackToolMoveChartRight <- function(...){
  n <- gtkNotebookGetCurrentPage(nb)
  if(n >= 0){
    da <- gtkNotebookGetNthPage(nb, n)
    simbolo <- gtkNotebookGetTabLabelText(nb, da)
    de <- as.Date(attr(carteira[[simbolo]],'zoom.de'))
    ate <- as.Date(attr(carteira[[simbolo]],'zoom.ate'))
    ultimo <- index(last(carteira[[simbolo]]))
    if((ate+5) <= ultimo){
      de <- de + 5;
      ate <- ate + 5;
    } else {
      n <- ultimo - ate
      de <- de + n
      ate <- ultimo
    }
    attr(carteira[[simbolo]],'zoom.ate') <- as.Date(ate)
    attr(carteira[[simbolo]],'zoom.de') <- as.Date(de)
    analise.tecnica(simbolo,da)
  }
  return(FALSE)
}

callbackToolRepaint <- function(...){
  n <- gtkNotebookGetCurrentPage(nb)
  if(n >= 0){
    da <- gtkNotebookGetNthPage(nb, n)
    asCairoDevice(da)
    simbolo <- gtkNotebookGetTabLabelText(nb, da)
    analise.tecnica(simbolo,da)
  }
}

callbackToolVariance <- function(...){
  n <- gtkNotebookGetCurrentPage(nb)
  if(n >= 0){
    da <- gtkNotebookGetNthPage(nb, n)
    simbolo <- gtkNotebookGetTabLabelText(nb, da)
    setStatusbar("Clique sobre os 2 candlesticks que você deseja calcular a variação")
    gdkWindowSetCursor(gtkWidgetGetWindow(da),gdkCursorNewFromName(gtkWidgetGetDisplay(da), "default"))
    loc <- locator(2)
    if(length(loc[[1]]) == 2){
      candle <- getCandleByLocator(simbolo,loc[[1]])
      v <- sort(getVariance(candle[1,], candle[2,]))
      setStatusbar("Variação entre os dias %s e %s: %s%% / %s%% / %s%%", index(candle[1,]), index(candle[2,]), v[1], v[2], v[3])
    }
  }
}

callbackToolZoomIn <- function(...){
  n <- gtkNotebookGetCurrentPage(nb)
  if(n >= 0){
    da <- gtkNotebookGetNthPage(nb, n)
    asCairoDevice(da)
    simbolo <- gtkNotebookGetTabLabelText(nb, da)
    
    de <- attr(carteira[[simbolo]],'zoom.de')
    ate <- attr(carteira[[simbolo]],'zoom.ate')
    zoom.dif <- (ate-de)
    zoom.in <- zoom.dif/4

    if(zoom.in > 25){
      de <- de + zoom.in
      ate <- ate - zoom.in
      
      attr(carteira[[simbolo]], 'zoom.de') <- as.Date(de)
      attr(carteira[[simbolo]], 'zoom.ate') <- as.Date(ate)

      analise.tecnica(simbolo,da)
    }
  }
}

callbackToolZoomInterval <- function(...){
  n <- gtkNotebookGetCurrentPage(nb)
  if(n >= 0){
    da <- gtkNotebookGetNthPage(nb, n)
    asCairoDevice(da)
    simbolo <- gtkNotebookGetTabLabelText(nb, da)
    
    out <- input2DateBox('Zoom entre datas específicas')
    if(!is.null(out)){
      z <- strsplit(out, '::')[[1]]
      
      if(length(z) == 2){
        de <- as.Date(z[1])
        ate <- as.Date(z[2])
      } else if((length(z) == 1) && (z[1] == '')){
        de <- as.Date(index(carteira[[simbolo]][1,]))
        ate <- as.Date(z[1])
      } else {
        de <- as.Date(z[1])
        ate <- as.Date(Sys.time())
      }
      
      if(de > ate){
        alertBox('Data à esquerda superior à da direita!')
        setStatusbar('Zoom entre datas cancelado.')
      } else {
        setZoomSimbolo(simbolo, de, ate)
        analise.tecnica(simbolo,da)
      }
    } else {
      setStatusbar('Datas faltando. Operação cancelada.')
    }
  }
}

callbackToolZoomFit <- function(...){
  n <- gtkNotebookGetCurrentPage(nb)
  if(n >= 0){
    da <- gtkNotebookGetNthPage(nb, n)
    asCairoDevice(da)
    simbolo <- gtkNotebookGetTabLabelText(nb, da)
    
    de <- Sys.Date()-180
    ate <- Sys.Date() 
    
    attr(carteira[[simbolo]], 'zoom.de') <- as.Date(de)
    attr(carteira[[simbolo]], 'zoom.ate') <- as.Date(ate)
    
    analise.tecnica(simbolo,da)
  }
}

callbackToolZoomOut <- function(...){
  n <- gtkNotebookGetCurrentPage(nb)
  if(n >= 0){
    da <- gtkNotebookGetNthPage(nb, n)
    asCairoDevice(da)
    simbolo <- gtkNotebookGetTabLabelText(nb, da)
    de <- attr(carteira[[simbolo]],'zoom.de')
    ate <- attr(carteira[[simbolo]],'zoom.ate')
    zoom.dif <- (ate-de)
    zoom.out <- zoom.dif/2
    de <- de - zoom.out
    callbackToolVariance <- function(...){
      n <- gtkNotebookGetCurrentPage(nb)
      if(n >= 0){
        da <- gtkNotebookGetNthPage(nb, n)
        simbolo <- gtkNotebookGetTabLabelText(nb, da)
        loc <- locator(2)
        if(length(loc[[1]]) == 2){
          candle <- getCandleByLocator(simbolo,loc[[1]])
          v <- sort(getVariance(candle[1,], candle[2,]))
          setStatusbar("Variação entre os dias %s e %s: %s%% / %s%% / %s%%", index(candle[1,]), index(candle[2,]), v[1], v[2], v[3])
        }
      }
    }
    
    if(de < index(carteira[[simbolo]][1,])){
      de <- index(carteira[[simbolo]][1,])
    } else {
      de <- de - zoom.out
    }
    
    if(ate > index(carteira[[simbolo]][nrow(carteira[[simbolo]]),])){
      ate <- attr(carteira[[simbolo]], 'zoom.ate')
    } else {
      ate <- ate + zoom.out
    }
    
    attr(carteira[[simbolo]], 'zoom.de') <- as.Date(de)
    attr(carteira[[simbolo]], 'zoom.ate') <- as.Date(ate)
    
    analise.tecnica(simbolo,da)
  }
}
