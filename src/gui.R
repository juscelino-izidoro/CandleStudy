source('dialogBox.R')
source('guiInsereSimbolo.R')

gtkInit()

# Janela Principal
janelaPrincipal <- gtkWindow(show = FALSE)

gtkWindowSetTitle(janelaPrincipal, "Analise Tecnica")

# Layout

vb <- gtkVBox(FALSE)

janelaPrincipal$add(vb)

# Barra do Menu Principal
menuPrincipal <- gtkMenuBar()

menuItemCarteira <- gtkMenuItemNewWithMnemonic("_Carteira")
menuCarteira <- gtkMenu()
opcaoCarteiraNova <- gtkImageMenuItemNewWithMnemonic("_Novo")
opcaoCarteiraAbrir <- gtkImageMenuItemNewWithMnemonic("_Abrir...")
opcaoCarteiraSalvar <- gtkImageMenuItemNewWithMnemonic("_Salvar")
opcaoCarteiraSalvarComo <- gtkImageMenuItemNewWithMnemonic("Salvar _Como...")
opcaoCarteiraRecentes <- gtkImageMenuItemNewWithMnemonic("R_ecentes")
subMenuCarteiraRecentes <- gtkMenu()
subOpcaoCarteiraRecentesLimpar <- gtkImageMenuItemNewWithMnemonic("_Limpar")
opcaoCarteiraSeparador <- gtkImageMenuItem("-")
opcaoCarteiraSair <- gtkImageMenuItemNewWithMnemonic("Sai_r")

menuItemSimbolo <- gtkMenuItemNewWithMnemonic("_Símbolo")
menuSimbolo <- gtkMenu()
opcaoSimboloInserir <- gtkImageMenuItemNewWithMnemonic("_Inserir...")
opcaoSimboloAtualizar <- gtkImageMenuItemNewWithMnemonic("_Atualizar")
opcaoSimboloEditar <- gtkImageMenuItemNewWithMnemonic("_Editar")
opcaoSimboloRemover <- gtkImageMenuItemNewWithMnemonic("_Remover")

menuItemEstudo <- gtkMenuItemNewWithMnemonic("_Estudo")
menuEstudo <- gtkMenu()
opcaoSupRes <- gtkCheckMenuItemNewWithMnemonic("_Suporte e Resistência")
opcaoBB <- gtkCheckMenuItemNewWithMnemonic("_Bandas de Bolinger")
opcaoMME <- gtkCheckMenuItemNewWithMnemonic("_Médias Móveis Exponenciais")
opcaoVF <- gtkCheckMenuItemNewWithMnemonic("_Volume Financeiro")
opcaoMMVF <- gtkCheckMenuItemNewWithMnemonic("_Médias Móveis sobre Volume Financeiro")
opcaoIFR <- gtkCheckMenuItemNewWithMnemonic("_Indice de Força Relativa")
opcaoChAD <- gtkCheckMenuItemNewWithMnemonic("_Acumulação/Distribuição Chaikin")
opcaoDPO <- gtkCheckMenuItemNewWithMnemonic("_Oscilador de Preço Retificado")

menuCarteira$add(opcaoCarteiraNova)
menuCarteira$add(opcaoCarteiraAbrir)
menuCarteira$add(opcaoCarteiraSalvar)
menuCarteira$add(opcaoCarteiraSalvarComo)
menuCarteira$add(opcaoCarteiraRecentes)
subMenuCarteiraRecentes$add(gtkSeparatorMenuItem())
subMenuCarteiraRecentes$add(subOpcaoCarteiraRecentesLimpar)
gtkMenuItemSetSubmenu(opcaoCarteiraRecentes, subMenuCarteiraRecentes)
menuCarteira$add(gtkSeparatorMenuItem())
menuCarteira$add(opcaoCarteiraSair)
gtkMenuItemSetSubmenu(menuItemCarteira, menuCarteira)
menuPrincipal$add(menuItemCarteira)


menuSimbolo$add(opcaoSimboloInserir)
menuSimbolo$add(opcaoSimboloAtualizar)
menuSimbolo$add(opcaoSimboloEditar)
menuSimbolo$add(opcaoSimboloRemover)
gtkMenuItemSetSubmenu(menuItemSimbolo,menuSimbolo)
menuPrincipal$add(menuItemSimbolo)


menuEstudo$add(opcaoSupRes)
menuEstudo$add(opcaoBB)
menuEstudo$add(opcaoMME)
menuEstudo$add(opcaoVF)
menuEstudo$add(opcaoMMVF)
menuEstudo$add(opcaoIFR)
menuEstudo$add(opcaoChAD)
menuEstudo$add(opcaoDPO)
gtkMenuItemSetSubmenu(menuItemEstudo,menuEstudo)
menuPrincipal$add(menuItemEstudo)
               

vb$packStart(menuPrincipal, FALSE, TRUE)

# Toolbar com acessibilidades para a análise
tb <- gtkToolbar()

toolZoomOut      <- gtkToolButton(NULL, label = '[-]')
toolZoomIn       <- gtkToolButton(NULL, label = '[+]')
toolZoomFit      <- gtkToolButton(NULL, label = '[ ]')
toolZoomInterval <- gtkToolButton(NULL, label = '[:]')
toolVariance     <- gtkToolButton(NULL, label = '(%)')
toolRepaint      <- gtkToolButton(NULL, label = '(R)')
toolLeft         <- gtkToolButton(NULL, label = '<')
toolRight        <- gtkToolButton(NULL, label = '>')

tb$add(toolZoomOut)
tb$add(toolZoomIn)
tb$add(toolZoomFit)
tb$add(toolZoomInterval)
tb$add(toolVariance)
tb$add(toolRepaint)
tb$add(toolLeft)
tb$add(toolRight)


vb$packStart(tb, FALSE, TRUE)

# Painel de Abas

nb <- gtkNotebook()

vb$packStart(nb, TRUE, TRUE)

# Barra de Status

sb <- gtkStatusbar()

vb$packStart(sb, FALSE, TRUE)

# Mostra a Janela Principal

gtkWindowResize(janelaPrincipal, 800,600)
