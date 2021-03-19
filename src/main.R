rm(list=ls())
options(encoding='UTF-8')
source('libs.R')
source('gui.R')
source('callbackDefinition.R')
source('callbackConnection.R')

NRECENTES <- 10
changed <- FALSE
setFilename(character(0))
carteira <- new.env()

janelaPrincipal$show()

if(!interactive()){ gtkMain() }
