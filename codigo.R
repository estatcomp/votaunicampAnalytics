##Chunk 1

if (Sys.info()['sysname'] == 'Darwin') Sys.setlocale(locale='UTF-8')
library(knitr)
library(printr)
library(vegan)
library(stringr)
library(rvest)
library(googleVis)
library(leaflet)
library(formattable)
library(data.table)
library(formattable)
library(ggplot2)
page = read_html('http://votaunicamp.herokuapp.com/prev_results/', encoding='UTF-8')
tbl = html_table(page)[[2]]


##Chunk 2
mytbl = data.frame(codigo=as.integer(str_extract(tbl$Curso, "^\\d+")),
                   nivel=NA,
                   curso=gsub("^(\\d+) - (.*)$", "\\2", tbl$Curso),
                   total=tbl$Total,
                   sim=as.integer(str_extract(tbl$Sim, "^\\d+")),
                   nao=as.integer(str_extract(tbl$Não, "^\\d+")),
                   abstencao=as.integer(str_extract(tbl$Abstenções, "^\\d+")))
nivel = str_extract(mytbl$curso, "(Dou|Mes)[a-z]+")
nivel[grepl("Mes", nivel)] = "Mestrado"
nivel[grepl("Dou", nivel)] = "Doutorado"
nivel[is.na(nivel)] = "Graduacao"
mytbl$nivel = nivel
rm(nivel)
mytbl$curso = gsub("(Mes|Dou).+ em (.*)$", "\\2", mytbl$curso)

##Chunk 3

gradinst = cbind(nivel='Graduacao', read.table('Grad.csv', header=TRUE, sep=','))
doutinst = cbind(nivel='Doutorado', read.table('Dout.csv', header=TRUE, sep=','))
mestinst = cbind(nivel='Mestrado', read.table('Mestr.csv', header=TRUE, sep=','))
names(gradinst) = names(doutinst) = names(mestinst) = c('nivel', 'codigo', 'instituto', 'area')
inst = rbind(gradinst, doutinst, mestinst)
rm(gradinst, doutinst, mestinst)
mytbl = merge(mytbl, inst)


##Chunk 5

tbl2 = mytbl[,c(2,3)]
pnao = with(mytbl, nao/total)
mes = qnorm(.975)*sqrt(1/(4*mytbl$total))
ics = cbind(pmax(pnao-mes, 0), pmin(pnao+mes, 1))
tbl2$Nao = pnao
tbl2$lowerpnao=ics[,1]
tbl2$upperpnao=ics[,2]
rm(pnao, mes, ics)

##Chunk 6

psim = with(mytbl, sim/total)
mes = qnorm(.975)*sqrt(1/(4*mytbl$total))
ics = cbind(pmax(psim-mes, 0), pmin(psim+mes, 1))
tbl2$Sim=psim
tbl2$lowerpsim=ics[,1]
tbl2$upperpsim=ics[,2]
rm(psim, mes, ics)

##Chunk 7

pabs = with(mytbl, abstencao/total)
mes = qnorm(.975)*sqrt(1/(4*mytbl$total))
ics = cbind(pmax(pabs-mes, 0), pmin(pabs+mes, 1))
tbl2$Abtenho=pabs
tbl2$lowerpabs=ics[,1]
tbl2$upperpabs=ics[,2]
names(tbl2)=c("Nível", "Curso", "Não", "Inf Não", "Sup Não", "Sim", "Inf Sim", "Sup Sim", "Abstenho", "Inf Abstenho", "Sup Abstenho")

##Chunk 8

mytbl = as.data.table(mytbl)
dados = mytbl[, list(total=sum(total), sim=sum(sim), nao=sum(nao), abstencao=sum(abstencao)), by=instituto]
dados = as.data.frame(dados)
dados$psim = with(dados, sim/total)
dados$pnao = with(dados, nao/total)
dados$pabs = with(dados, abstencao/total)



##Chunk 10

dados2 = dados[, c('instituto', 'nao', 'sim', 'abstencao', 'total')]
names(dados2) = c("Instituto", "Não", "Sim", "Abstenções", "Total")
kable(head(dados2), caption = 'Tabela 3: Votos por instituto')

##Chunk 11



##Chunk 12

kable(head(dados[,-c(2,3,4,5)]), caption='Tabela 2: Proporção de votos por instituto')
