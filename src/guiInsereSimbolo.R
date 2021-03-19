insereSimbolo <- function(simbolo='', de=as.character(as.Date(Sys.time())-180), ate=as.character(as.Date(Sys.time())), manterAtualizado=TRUE){
  # Janela de inseção de símbolo
  janelaInsereSimbolo <- gtkDialogNewWithButtons("Novo Símbolo"
                                               , NULL
                                               , 0
                                               , 'gtk-ok', GtkResponseType['ok']
                                               , 'gtk-cancel', GtkResponseType['cancel'] 
                                               , show = FALSE)
  
  gtkWindowSetResizable(janelaInsereSimbolo, FALSE)
  
  tabLayout <- gtkTable(4, 3, FALSE, TRUE)
  
  vbox <- janelaInsereSimbolo$getContentArea()
  vbox$packStart(tabLayout)
  
  lblSimbolo <- gtkLabel("Símbolo")
  lblDe <- gtkLabel("Mostrar a partir de")
  lblAte <- gtkLabel("Mostrar até")
  
  txtSimbolo <- gtkEntry()
  txtDe <- gtkEntry()
  txtAte <- gtkEntry()
  
  btnDe  <- gtkButton('...')
  btnAte <- gtkButton('...')
  
  chkAtualizar <- gtkCheckButton('Manter símbolo sempre atualizado?')
  gtkToggleButtonSetActive(chkAtualizar, manterAtualizado)
  
  lblSimbolo$xpad <- 10
  lblDe$xpad <- 10
  lblAte$xpad <- 10
  
  gSignalConnect(chkAtualizar, 'toggled', function(...){ manterAtualizado <<- gtkToggleButtonGetActive(chkAtualizar) })
  
  bufSimbolo <- gtkEntryBuffer()
  bufDe <- gtkEntryBuffer()
  bufAte <- gtkEntryBuffer()
  
  gtkEntrySetBuffer(txtSimbolo, bufSimbolo)
  gtkEntrySetBuffer(txtDe, bufDe)
  gtkEntrySetBuffer(txtAte, bufAte)
  
  gtkEntryBufferSetText(bufSimbolo, simbolo, -1)
  gtkEntryBufferSetText(bufDe, de, 10)
  gtkEntryBufferSetText(bufAte, ate, 10)
  
  gtkTableAttach(tabLayout, lblSimbolo   , 0,1,0,1)
  gtkTableAttach(tabLayout, lblDe        , 0,1,1,2)
  gtkTableAttach(tabLayout, lblAte       , 0,1,2,3)
  gtkTableAttach(tabLayout, txtSimbolo   , 1,2,0,1)
  gtkTableAttach(tabLayout, txtDe        , 1,2,1,2)
  gtkTableAttach(tabLayout, txtAte       , 1,2,2,3)
  gtkTableAttach(tabLayout, btnDe        , 2,3,1,2)
  gtkTableAttach(tabLayout, btnAte       , 2,3,2,3)
  gtkTableAttach(tabLayout, chkAtualizar , 0,2,3,4)
  
  gSignalConnect(btnDe, 'clicked', function(...){ 
    d <- dropDownCalendar(de, parent = janelaInsereSimbolo)
    if(!is.null(d)){
      de <<- sprintf("%04d-%02d-%02d", as.numeric(d['year']),as.numeric(d['month'])+1,as.numeric(d['day']))
      gtkEntryBufferSetText(bufDe, de, 10)
    }
  })
  
  gSignalConnect(btnAte, 'clicked', function(...){ 
    a <- dropDownCalendar(ate, parent = janelaInsereSimbolo)
    if(!is.null(a)){
      ate <<- sprintf("%04d-%02d-%02d", as.numeric(a['year']),as.numeric(a['month'])+1,as.numeric(a['day']))
      gtkEntryBufferSetText(bufAte, ate, 10)
    }
  })
  
  out <- janelaInsereSimbolo$run()

  janelaInsereSimbolo$destroy()
  
  if(out == GtkResponseType['ok']){
    simbolo <- gtkEntryBufferGetText(bufSimbolo)
    de <- gtkEntryBufferGetText(bufDe)
    ate <- gtkEntryBufferGetText(bufAte)
    if(nchar(simbolo) < 3){ 
      alertBox('Símbolo inválido')
      return(insereSimbolo(simbolo, de, ate))
    } else if(length(grep('\\d{4}-\\d{2}-\\d{2}', de)) == 0){
      alertBox('Data "de" inválida')
      return(insereSimbolo(simbolo, de, ate))
    } else if(length(grep('\\d{4}-\\d{2}-\\d{2}', ate)) == 0){
      alertBox('Data "até" inválida')
      return(insereSimbolo(simbolo, de, ate))
    }
    
    return(list(simbolo=simbolo, de=de, ate=ate, manterAtualizado=manterAtualizado))
  }
  return(NULL)
}
