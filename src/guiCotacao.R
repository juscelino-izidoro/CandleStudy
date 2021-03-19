atualizaCotacao <- function(acoes){
  if(!exists('cotacao', globalenv())){
    cotacao <<- getQuote(acoes$Simbolo)
    cotacao <- cotacao[!is.na(cotacao$`Trade Time`),]
    cotacao$Simbolo <- row.names(cotacao)
    cotacao$Empresa <- sapply(cotacao$Simbolo, function(s){acoes$Empresa[acoes$Simbolo==s]})
    cotacao <- cotacao[,c('Simbolo','Empresa','Last','Change','% Change','Open','High','Low','Volume','Trade Time')]
    cotacao$Last <- round(as.numeric(sub('%','',cotacao$Last)),2)
    cotacao$Change <- round(as.numeric(cotacao$Change),2)
    cotacao$`% Change` <- round(as.numeric(sub('%','',cotacao$`% Change`)),2)
    cotacao$Open <- round(as.numeric(sub('%','',cotacao$Open)),2)
    cotacao$High <- round(as.numeric(sub('%','',cotacao$High)),2)
    cotacao$Low <- round(as.numeric(sub('%','',cotacao$Low)),2)
    cotacao$Volume <- round(as.numeric(sub('%','',cotacao$Volume)),2)
    cotacao$`Trade Time` <- as.character(cotacao$`Trade Time`)
    names(cotacao) <- c('Símbolo','Nome_Empresa','Último','Variação_R$','Variação_%','Abertura','Máxima','Mínima','Volume','Data_Horário')
  }
  
  cotacao
}

tabelaCotacao <- function(){
  acoes <- read.csv('acoes.csv', stringsAsFactors = FALSE)
  cotacao <- atualizaCotacao(acoes)
  
  janelaCotacao <- gtkDialogNewWithButtons("Cotação"
                                                 , NULL
                                                 , 0
                                                 , 'gtk-ok', GtkResponseType['ok']
                                                 , show = FALSE)
  scrolledTable <- gtkScrolledWindow()
  
  gtkWindowSetResizable(janelaCotacao, TRUE)
  gtkWindowSetDefaultSize(janelaCotacao, nchar(paste(colnames(cotacao),collapse=''))*9.2, 400)
  
  tabLayout <- gtkTable(3, 2, FALSE, TRUE)
  
  vbox <- janelaCotacao$getContentArea()
  vbox$packStart(tabLayout)
  
  cotacaoModel <- rGtkDataFrame(cotacao)
  sortedCotacaoModel <- gtkTreeModelSortNewWithModel(cotacaoModel)
  tblCotacao <- gtkTreeView(sortedCotacaoModel)
  
  mapply(tblCotacao$insertColumnWithAttributes
       , position = -1
       , title = colnames(cotacaoModel)
       , cell = list(gtkCellRendererText())
       , text=seq_len(ncol(cotacaoModel)) - 1
       )
  sapply(seq_len(ncol(cotacaoModel)), function(i){tblCotacao$getColumn(i-1)$setSortColumnId(i-1) })
  
  btnAtualiza  <- gtkButton('Atualizar')

  gtkTableAttach(tabLayout, btnAtualiza   , 0,2,0,1, yoptions=0)
  gtkTableAttach(tabLayout, scrolledTable , 0,2,1,2)
  
  scrolledTable$add(tblCotacao)

  #gSignalConnect(tblCotacao, 'double-clicked', function(...){ 
  #  print("OK")
  #})
  
  out <- janelaCotacao$run()
  
  janelaCotacao$destroy()
  
  return(NULL)
}