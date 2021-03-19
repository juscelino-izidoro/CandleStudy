getFileFilter <- function(){
  f <- gtkFileFilter()
  f$setName('R Data')
  f$addPattern('*.RData')
  f
}

openFileDialog <- function(){
  d <- gtkFileChooserDialog('Abrir', janelaPrincipal, 'open', 'gtk-ok', GtkResponseType['ok'], 'gtk-cancel', GtkResponseType['cancel'])
  d$addFilter(getFileFilter())
  out <- d$run()
  fn <- character(0)
  if(out == GtkResponseType['ok']){
    fn <- d$getFilename()
  }
  d$destroy()
  return(fn)
}

saveFileDialog <- function(){
  d <- gtkFileChooserDialog('Salvar', janelaPrincipal, 'save', 'gtk-ok', GtkResponseType['ok'], 'gtk-cancel', GtkResponseType['cancel'])
  d$addFilter(getFileFilter())
  out <- d$run()
  fn <- character(0)
  if(out == GtkResponseType['ok']){
    fn <- d$getFilename()
  }
  d$destroy()
  return(fn)
}


alertBox <- function(msg, buttons="ok", type='warning', parent=janelaPrincipal){
  dialog <- gtkMessageDialog(parent, 0, type, buttons, msg) 
  out <- dialog$run() 
  dialog$destroy()
  out
}

msgBox <- function(msg, title='', parent=janelaPrincipal, ...){
  args <- list(...)
  
  if(length(args) == 0){
    dialog <- gtkDialogNewWithButtons(title, parent, 0, 'gtk-ok', GtkResponseType['ok'], show = FALSE)
  } else {
    dialog <- gtkDialogNewWithButtons(title, parent, 0, ..., show = FALSE)
  }
  vbox <- dialog$getContentArea()
  vbox$spacing <- 10
  vbox$packStart(gtkLabel(msg))
  out <- dialog$run()
  dialog$destroy()
  out
}

inputBox <- function(msg, title='', parent=janelaPrincipal){
  dialog <- gtkDialogNewWithButtons(title, parent, 0, 'gtk-ok', GtkResponseType['ok'], show = FALSE)
  vbox <- dialog$getContentArea()
  vbox$spacing <- 10
  vbox$packStart(gtkLabel(msg))
  ent <- gtkEntry()
  buf <- gtkEntryBuffer()
  gtkEntrySetBuffer(ent, buf)
  vbox$packStart(ent)
  dialog$run()
  out <- gtkEntryBufferGetText(buf)
  dialog$destroy()
  out
}

input2DateBox <- function(de = NULL, ate = NULL, title = '', parent = janelaPrincipal){
  dialog <- gtkDialogNewWithButtons('Selecione as datas', parent, 0, 'gtk-ok', GtkResponseType['ok'], show = FALSE)
  gtkWindowSetTitle(dialog, title)
  vbox <- dialog$getContentArea()
  txtDe <- gtkEntry()
  txtAte <- gtkEntry()
  bufDe <- gtkEntryBuffer()
  bufAte <- gtkEntryBuffer()
  btnDe <- gtkButton('...')
  btnAte <- gtkButton('...')
  tabLayout <- gtkTable(2, 3, FALSE, TRUE)
  out <- NULL
  
  gtkEntrySetBuffer(txtDe, bufDe)
  gtkEntrySetBuffer(txtAte, bufAte)
  
  vbox$packStart(tabLayout)
  gtkTableAttach(tabLayout, gtkLabel('Mostrar a partir de:'), 0,1,0,1)
  gtkTableAttach(tabLayout, txtDe                           , 1,2,0,1)
  gtkTableAttach(tabLayout, btnDe                           , 2,3,0,1)
  gtkTableAttach(tabLayout, gtkLabel('Mostrar até:')        , 0,1,1,2)
  gtkTableAttach(tabLayout, txtAte                          , 1,2,1,2)
  gtkTableAttach(tabLayout, btnAte                          , 2,3,1,2)
  
  gSignalConnect(btnDe, 'clicked', function(...){ 
    d <- dropDownCalendar(parent = parent)
    if(!is.null(d)){
      de <<- sprintf("%04d-%02d-%02d", as.numeric(d['year']),as.numeric(d['month'])+1,as.numeric(d['day']))
      gtkEntryBufferSetText(bufDe, de, 10)
    }
  })
  
  gSignalConnect(btnAte, 'clicked', function(...){ 
    a <- dropDownCalendar(parent = parent)
    if(!is.null(a)){
      ate <<- sprintf("%04d-%02d-%02d", as.numeric(a['year']),as.numeric(a['month'])+1,as.numeric(a['day']))
      gtkEntryBufferSetText(bufAte, ate, 10)
    }
  })
  
  x <- dialog$run()
  if(x == GtkResponseType['ok']){
    de <- gtkEntryBufferGetText(bufDe)
    ate <- gtkEntryBufferGetText(bufAte)
    if(de != '' && ate != ''){
      out <- c(de=de, ate=ate)
    }
  }
  dialog$destroy()
  return(out)
}

dropDownCalendar <- function(data = NULL, parent = NULL){
  dialog <- gtkDialog(show = FALSE, parent = parent)
  cal <- gtkCalendar()
  vbox <- dialog$getContentArea()
  vbox$packStart(gtkLabel('Clique duplo no dia para confirmar'))
  vbox$packStart(cal)
  out <- NULL
  
  gSignalConnect(cal, 'day-selected-double-click', function(...){ 
    out <<- gtkCalendarGetDate(cal)
    gtkDialogResponse(dialog, GtkResponseType['ok']) 
  })
  
  gSignalConnect(cal, 'key-press-event', function(w, e, u){ 
    if(e$keyval == GDK_Return){ 
      out <<- gtkCalendarGetDate(cal)
      gtkDialogResponse(dialog, GtkResponseType['ok'])
    }
    FALSE
  })
  
  gSignalConnect(dialog, 'close', function(...){ out <<-NULL ; gtkDialogResponse(dialog, GtkResponseType['close']) } )
  
  if(!is.null(data)){
    if(length(grep('[0-9]{4}-[0-9]{2}-[0-9]{2}', data)) >0){
      z <- as.numeric(strsplit(data, '-')[[1]])
      cal$year <- z[1]
      cal$month <- z[2]-1
      cal$day <- z[3]
    } else if(length(grep('[0-9]{2}/[0-9]{2}/[0-9]{4}', data)) > 0){
      z <- as.numeric(strsplit(data, '/')[[1]])
      cal$day <- z[1]
      cal$month <- z[2]-1
      cal$year <- z[3]
    } else {
      alertBox('A data inicial informada é inválida.')
      return(out)
    }
  }
  dialog$run()
  dialog$destroy()
  return(out)
}
