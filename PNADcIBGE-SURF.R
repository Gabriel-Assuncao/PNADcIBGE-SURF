##########################################################################

# Limpando arquivos armazenados na memória
rm(list=ls(all=TRUE))

# Definindo limite de memória para compilação do programa
aviso <- getOption("warn")
options(warn=-1)
memory.limit(size=50000)
options(warn=aviso)
rm(aviso)

# Definindo tempo de espera para obtenção de resposta do servidor
aviso <- getOption("warn")
options(warn=-1)
options(timeout=600)
options(warn=aviso)
rm(aviso)

# Definindo opção de codificação dos caracteres e linguagem
aviso <- getOption("warn")
options(warn=-1)
options(encoding="latin1")
options(warn=aviso)
rm(aviso)

# Definindo opção de exibição de números sem representação em exponencial
aviso <- getOption("warn")
options(warn=-1)
options(scipen=999)
options(warn=aviso)
rm(aviso)

# Definindo opção de repositório para instalação dos pacotes necessários
aviso <- getOption("warn")
options(warn=-1)
options(repos=structure(c(CRAN="https://cran.r-project.org/")))
options(warn=aviso)
rm(aviso)

# Definindo diretório de trabalho
caminho <- getwd()
setwd(dir=caminho)

# Carregando pacotes necessários para obtenção da estimativa desejada
if("PNADcIBGE" %in% rownames(installed.packages())==FALSE)
{
  install.packages(pkgs="PNADcIBGE", dependencies=TRUE)
}
library(package="PNADcIBGE", verbose=TRUE)
if("dplyr" %in% rownames(installed.packages())==FALSE)
{
  install.packages(pkgs="dplyr", dependencies=TRUE)
}
library(package="dplyr", verbose=TRUE)
if("tibble" %in% rownames(installed.packages())==FALSE)
{
  install.packages(pkgs="tibble", dependencies=TRUE)
}
library(package="tibble", verbose=TRUE)
if("survey" %in% rownames(installed.packages())==FALSE)
{
  install.packages(pkgs="survey", dependencies=TRUE)
}
library(package="survey", verbose=TRUE)
if("surf" %in% rownames(installed.packages())==FALSE)
{
  install.packages(pkgs="surf", dependencies=TRUE)
}
library(package="surf", verbose=TRUE)

# Obtendo microdados trimestrais da PNAD Contínua (PNADcIBGE >= 0.6.0)
pnadc022022 <- PNADcIBGE::get_pnadc(year=2022, quarter=2, labels=TRUE, deflator=TRUE, design=FALSE)
pnadc032022 <- PNADcIBGE::get_pnadc(year=2022, quarter=3, labels=TRUE, deflator=TRUE, design=FALSE)
pnadc042022 <- PNADcIBGE::get_pnadc(year=2022, quarter=4, labels=TRUE, deflator=TRUE, design=FALSE)

# Realizando coleta de lixo acumulada durante a obtenção dos microdados
gc(verbose=FALSE, reset=FALSE, full=TRUE)

# Criando variáveis auxiliares para obtenção da estimativa desejada
pnadc022022 <- transform(pnadc022022, ID_DOMICILIO=paste0(UPA,V1008,V1014))
pnadc022022 <- transform(pnadc022022, ID_PESSOA=paste0(UPA,V1008,V1014,V2003))
pnadc032022 <- transform(pnadc032022, ID_DOMICILIO=paste0(UPA,V1008,V1014))
pnadc032022 <- transform(pnadc032022, ID_PESSOA=paste0(UPA,V1008,V1014,V2003))
pnadc042022 <- transform(pnadc042022, ID_DOMICILIO=paste0(UPA,V1008,V1014))
pnadc042022 <- transform(pnadc042022, ID_PESSOA=paste0(UPA,V1008,V1014,V2003))

# Realizando processo de junção das bases dos trimestres de referência
data_pnadc <- merge(x=pnadc022022, y=pnadc032022, by.x="ID_PESSOA", by.y="ID_PESSOA", all.x=FALSE, all.y=FALSE)
data_pnadc <- merge(x=data_pnadc, y=pnadc042022, by.x="ID_PESSOA", by.y="ID_PESSOA", all.x=FALSE, all.y=FALSE)
rm(pnadc022022, pnadc032022, pnadc042022)

# Mantendo somente observações das visitas de referência para comparação
data_pnadc <- subset(data_pnadc, `V1016.x`%in%c("1","2","3") & `V1016.y`%in%c("2","3","4") & `V1016`%in%c("3","4","5"))
data_pnadc <- tibble::as_tibble(x=data_pnadc)

# Criando variável de status para análise do fluxo bruto entre trimestres
data_pnadc <- transform(data_pnadc, status_1=as.factor(ifelse(`VD4001.x`=="Pessoas fora da força de trabalho", "Pessoas fora da força de trabalho", as.character(`VD4002.x`))))
data_pnadc$status_1 <- factor(x=data_pnadc$status_1, levels=c("Pessoas ocupadas","Pessoas desocupadas","Pessoas fora da força de trabalho"))
data_pnadc <- transform(data_pnadc, status_2=as.factor(ifelse(`VD4001.y`=="Pessoas fora da força de trabalho", "Pessoas fora da força de trabalho", as.character(`VD4002.y`))))
data_pnadc$status_2 <- factor(x=data_pnadc$status_2, levels=c("Pessoas ocupadas","Pessoas desocupadas","Pessoas fora da força de trabalho"))
data_pnadc <- transform(data_pnadc, status_3=as.factor(ifelse(`VD4001`=="Pessoas fora da força de trabalho", "Pessoas fora da força de trabalho", as.character(`VD4002`))))
data_pnadc$status_3 <- factor(x=data_pnadc$status_3, levels=c("Pessoas ocupadas","Pessoas desocupadas","Pessoas fora da força de trabalho"))

# Incorporando desenho amostral com pesos do primeiro trimestre de referência
metodo <- c("antigo","novo","replicacao")
metodo <- metodo[2]
options(survey.lonely.psu="adjust")
{
  if(metodo=="antigo")
  {
    data_prior <- survey::svydesign(ids=~`UPA.x`, strata=~`Estrato.x`, data=data_pnadc, weights=~`V1027.x`, nest=TRUE)
    popc.types <- data.frame(`posest.x`=as.character(unique(data_pnadc$`posest.x`)), Freq=as.numeric(unique(data_pnadc$`V1029.x`)))
    popc.types <- popc.types[order(popc.types$`posest.x`), ]
    data_pnadc <- survey::postStratify(design=data_prior, strata=~`posest.x`, population=popc.types)
    rm(data_prior, popc.types)
  }
  else if(metodo=="novo")
  {
    data_prior <- survey::svydesign(ids=~`UPA.x`+`ID_DOMICILIO.x`, strata=~`Estrato.x`, data=data_pnadc, weights=~`V1027.x`, nest=TRUE)
    popc.types <- data.frame(`posest.x`=as.character(unique(data_pnadc$`posest.x`)), Freq=as.numeric(unique(data_pnadc$`V1029.x`)))
    popc.types <- popc.types[order(popc.types$`posest.x`),]
    popi.types <- data.frame(`posest_sxi.x`=as.character(unique(data_pnadc$`posest_sxi.x`)), Freq=as.numeric(unique(data_pnadc$`V1033.x`)))
    popi.types <- popi.types[order(popi.types$`posest_sxi.x`),]
    pop.rake.calib <- c(sum(popc.types$Freq), popc.types$Freq[-1], popi.types$Freq[-1])
    data_pnadc <- survey::calibrate(design=data_prior, formula=~`posest.x`+`posest_sxi.x`, pop=pop.rake.calib, calfun="raking", aggregate.stage=2, bounds=c(0.2, 5), multicore=TRUE)
    rm(data_prior, popc.types, popi.types, pop.rake.calib)
  }
  else
  {
    data_pnadc <- survey::svrepdesign(data=data_pnadc, weight=~`V1028.x`, type="bootstrap", repweights="V1028[0-9]+.x", mse=TRUE, replicates=length(sprintf("V1028%03d.x", seq(1:200))), df=length(sprintf("V1028%03d.x", seq(1:200))))
  }
}
str(object=data_pnadc)

# Definindo base da PNAD Contínua com recorte pela população em idade ativa
flow_pnadc <- subset(data_pnadc, `V2009.x`>=14 & `V2009.y`>=14 & `V2009`>=14)

# Estimando tabulações ingênuas das variáveis com e sem dados ausentes
survey::svytable(formula=~status_1+status_2, design=flow_pnadc)
survey::svytable(formula=~status_1+status_2, design=flow_pnadc, addNA=TRUE)
survey::svytable(formula=~status_1+status_3, design=flow_pnadc)
survey::svytable(formula=~status_1+status_3, design=flow_pnadc, addNA=TRUE)
survey::svytable(formula=~status_2+status_3, design=flow_pnadc)
survey::svytable(formula=~status_2+status_3, design=flow_pnadc, addNA=TRUE)

# Ajustando modelos de fluxos brutos com amostra complexa da PNAD Contínua
modA_pnadc_1_2 <- surf::svyflow(x=~status_1+status_2, design=flow_pnadc, model="A", verbose=TRUE)
modB_pnadc_1_2 <- surf::svyflow(x=~status_1+status_2, design=flow_pnadc, model="B", verbose=TRUE)
modC_pnadc_1_2 <- surf::svyflow(x=~status_1+status_2, design=flow_pnadc, model="C", verbose=TRUE)
modD_pnadc_1_2 <- surf::svyflow(x=~status_1+status_2, design=flow_pnadc, model="D", verbose=TRUE)
modA_pnadc_1_3 <- surf::svyflow(x=~status_1+status_3, design=flow_pnadc, model="A", verbose=TRUE)
modB_pnadc_1_3 <- surf::svyflow(x=~status_1+status_3, design=flow_pnadc, model="B", verbose=TRUE)
modC_pnadc_1_3 <- surf::svyflow(x=~status_1+status_3, design=flow_pnadc, model="C", verbose=TRUE)
modD_pnadc_1_3 <- surf::svyflow(x=~status_1+status_3, design=flow_pnadc, model="D", verbose=TRUE)
modA_pnadc_2_3 <- surf::svyflow(x=~status_2+status_3, design=flow_pnadc, model="A", verbose=TRUE)
modB_pnadc_2_3 <- surf::svyflow(x=~status_2+status_3, design=flow_pnadc, model="B", verbose=TRUE)
modC_pnadc_2_3 <- surf::svyflow(x=~status_2+status_3, design=flow_pnadc, model="C", verbose=TRUE)
modD_pnadc_2_3 <- surf::svyflow(x=~status_2+status_3, design=flow_pnadc, model="D", verbose=TRUE)

# Avaliando resultados obtidos dos modelos de fluxos brutos da PNAD Contínua
list(coeficiente=coef(object=modA_pnadc_1_2$muij), erro_padrao=SE(object=modA_pnadc_1_2$muij))
list(coeficiente=coef(object=modB_pnadc_1_2$muij), erro_padrao=SE(object=modB_pnadc_1_2$muij))
list(coeficiente=coef(object=modC_pnadc_1_2$muij), erro_padrao=SE(object=modC_pnadc_1_2$muij))
list(coeficiente=coef(object=modD_pnadc_1_2$muij), erro_padrao=SE(object=modD_pnadc_1_2$muij))
list(coeficiente=coef(object=modA_pnadc_1_3$muij), erro_padrao=SE(object=modA_pnadc_1_3$muij))
list(coeficiente=coef(object=modB_pnadc_1_3$muij), erro_padrao=SE(object=modB_pnadc_1_3$muij))
list(coeficiente=coef(object=modC_pnadc_1_3$muij), erro_padrao=SE(object=modC_pnadc_1_3$muij))
list(coeficiente=coef(object=modD_pnadc_1_3$muij), erro_padrao=SE(object=modD_pnadc_1_3$muij))
list(coeficiente=coef(object=modA_pnadc_2_3$muij), erro_padrao=SE(object=modA_pnadc_2_3$muij))
list(coeficiente=coef(object=modB_pnadc_2_3$muij), erro_padrao=SE(object=modB_pnadc_2_3$muij))
list(coeficiente=coef(object=modC_pnadc_2_3$muij), erro_padrao=SE(object=modC_pnadc_2_3$muij))
list(coeficiente=coef(object=modD_pnadc_2_3$muij), erro_padrao=SE(object=modD_pnadc_2_3$muij))

##########################################################################
