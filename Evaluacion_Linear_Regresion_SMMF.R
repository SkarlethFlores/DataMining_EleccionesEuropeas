# --------------------------------------------------------
#
#               REGRESION LINEAL
#
# --------------------------------------------------------

# Importat Funciones
#/Users/admin/Master_2023-UCM/7-MineriaDatosModelizacionPredictiva/Evaluacion/
path_funciones   <- "/Users/admin/Master_2023-UCM/7-MineriaDatosModelizacionPredictiva/Evaluacion/FuncionesMineria.R"
source(path_funciones, encoding = "UTF-8")

library(questionr)
library(car)
library(Hmisc)
library(readxl)
library(corrplot)
library(ggplot2)
library(tidyr)

#install.packages("lattice")
library(caret)
library(OneR)

par(mar=c(7, 9, 4.1, 2.1))

datosCont<-readRDS("datosEleccionesEuropeasDep2")




# Reduce the data set, imposible to run the full 30^2 interactions
#


datosCont <- datosCont[ , !(names(datosCont) %in%   c( "aleatorio", "aleatorio2",
                                                       "M_AutonomosPtge","WomenUnemploymentPtge","Age_under19_Ptge","M_UnemployLess25_Ptge",
                                                       "M_IndustryUnemploymentPtge","M_PobChange_pct","UnemployMore40_Ptge","M_AgricultureUnemploymentPtge",
                                                       "M_ConstructionUnemploymentPtge"))]
summary(datosCont)

varObjCont <- datosCont$varObjCont
inputCont <- datosCont[,-(1:1)]
summary(varObjCont)
summary(inputCont)






# hay alguna variable altamente correlacionada? 

lincorCont <- cor(data.frame(varObjCont,Filter(is.numeric, inputCont)))
lincorCont

high_corr_check<- function (x,limite){
  abs(x) >= limite 
}

get_corr <- function (lincorCont,limite){
  m_lincorCont        <- as.matrix(apply(lincorCont, 2, high_corr_check,limite) ) 
  diag(m_lincorCont)  <- FALSE
  A                   <- lower.tri(m_lincorCont, diag = FALSE) * m_lincorCont
  a                   <- apply(A,2, function(x) x == 1)
  high_corr           <- which(a, arr.ind = TRUE)
  if ( length(high_corr[,1]) == 1 ){
    rows<-rownames(lincorCont[])
    text <- paste(rows[high_corr[,1]],' y ',  rows[high_corr[,2]])
    return( text) #high_corr ,
  }
  else{
    try( high_corr[,1]      <- rownames(x = lincorCont[high_corr[,1],  ])  , silent = TRUE) 
    try( high_corr[,2]  <- colnames(x = lincorCont[, sapply(high_corr[,2],   as.numeric)  ]), silent = TRUE)
    rownames(high_corr) <- NULL
    return(high_corr)
  }
}

high_corr <- get_corr(lincorCont, 0.95)
high_corr

# ----->
#Se eliminan las variables Censo por ser redundante a la variable poblacion. 
inputCont <- inputCont[, !(names(inputCont) %in% c("Censo"))]
summary(inputCont)
names(inputCont)



# Transformaciones
TransfCont<-Transf_Auto(Filter(is.numeric, inputCont),varObjCont)
names(TransfCont)



# binning
#Se agrupan las categoras de aquellas variables discretizadas cuyas frecuencias sean menores al 5% con valores medios similares.

discCont<-droplevels(optbin(data.frame(Filter(is.numeric, inputCont),
                                       bin(varObjCont,nbins=5,method = "content"))))[, -(ncol(Filter(is.numeric, inputCont))+1)]
names(discCont)<-paste("disc", names(discCont), sep = "_")
names(discCont)

# ver que ninguna frecuencia es muy baja
apply(discCont,2,freq)



#aggregate(varObjCont, by=list(discCont$disc_Age_under19_Ptge), mean)
#discCont$disc_Age_under19_Ptge<-car::recode(discCont$disc_Age_under19_Ptge,
#                                      "c('(12.5,13.4]','(13.4,14]')='(12.5,13.4,14]'")


aggregate(varObjCont, by=list(discCont$disc_Age_over65_Ptge), mean)
discCont$disc_Age_over65_Ptge<-car::recode(discCont$disc_Age_over65_Ptge,
                                            "c('(28.3,29.4]','(29.4,30.8]')='(28.3,30.8]'")



aggregate(varObjCont, by=list(discCont$disc_WomanPopulationPtge), mean)
discCont$disc_WomanPopulationPtge<-car::recode(discCont$disc_WomanPopulationPtge,
                                           "c('(46.9,47.4]','(47.4,48]','(48,48.1]')='(46.9,48.1]'")


aggregate(varObjCont, by=list(discCont$disc_ForeignersPtge), mean)
discCont$disc_ForeignersPtge<-car::recode(discCont$disc_ForeignersPtge,
                                               "c('(5.4,6.08]','(6.08,6.36]')='(5.4,6.36]'")


aggregate(varObjCont, by=list(discCont$disc_UniversityPtge), mean)
discCont$disc_UniversityPtge<-car::recode(discCont$disc_UniversityPtge,
                                               "c('(8.47,9.89]','(9.89,10.6]')='(8.47,10.6]'")

aggregate(varObjCont, by=list(discCont$disc_Densidad), mean)
discCont$disc_Densidad<-car::recode(discCont$disc_Densidad,
                                               "c('(204,232]','(232,2.24e+04]')='(204,2.24e+04]'")

aggregate(varObjCont, by=list(discCont$disc_PobChange_pct), mean)
discCont$disc_PobChange_pct<-car::recode(discCont$disc_PobChange_pct,
                                               "c('(-5.33,-4.42]','(-5.95,-5.33]')='(-5.33,-5.33] '")

aggregate(varObjCont, by=list(discCont$disc_PersonasInmueble), mean)
discCont$disc_PersonasInmueble<-car::recode(discCont$disc_PersonasInmueble,
                                               "c('(1.21,1.27]','(1.27,1.31]')='(1.21,1.31]'")

aggregate(varObjCont, by=list(discCont$disc_Explotaciones), mean)
discCont$disc_Explotaciones<-car::recode(discCont$disc_Explotaciones,
                                               "c('(141,149]','(149,174]','(174,175]')='(141,175]'")

aggregate(varObjCont, by=list(discCont$disc_UnemploymentPtge), mean)
discCont$disc_UnemploymentPtge<-car::recode(discCont$disc_UnemploymentPtge,
                                               "c('(5.39,6.75]','(6.75,6.96]')='(5.39,6.96]'")

#aggregate(varObjCont, by=list(discCont$disc_WomenUnemploymentPtge), mean)
#discCont$disc_WomenUnemploymentPtge<-car::recode(discCont$disc_WomenUnemploymentPtge,
#                                               "c('(50.6,53.4]','(53.4,53.7]')='(50.6,53.7]'")

# No need
#aggregate(varObjCont, by=list(discCont$disc_UnemployLess25_Ptge), mean)
#discCont$disc_UnemployLess25_Ptge<-car::recode(discCont$disc_UnemployLess25_Ptge,
#                                               "c('(46.9,47.4]','(47.4,48]','(48,48.1]')='(46.9,48.1]'")


#aggregate(varObjCont, by=list(discCont$disc_UnemployMore40_Ptge), mean)
#discCont$disc_UnemployMore40_Ptge<-car::recode(discCont$disc_UnemployMore40_Ptge,
#                                               "c('(52.4,54.1]','(54.1,54.5]')='(52.4,54.5]'")

aggregate(varObjCont, by=list(discCont$disc_AgricultureUnemploymentPtge), mean)
discCont$disc_AgricultureUnemploymentPtge<-car::recode(discCont$disc_AgricultureUnemploymentPtge,
                                               "c('(4.66,7.76]','(7.76,8.81]')='(4.66,8.81]'")

# No need
#$disc_IndustryUnemploymentPtge

aggregate(varObjCont, by=list(discCont$disc_ConstructionUnemploymentPtge), mean)
discCont$disc_ConstructionUnemploymentPtge<-car::recode(discCont$disc_ConstructionUnemploymentPtge,
                                               "c('(-0.0407,6.48]','(6.48,7.13]')='(-0.0407,7.13]'")

aggregate(varObjCont, by=list(discCont$disc_ServicesUnemploymentPtge), mean)
discCont$disc_ServicesUnemploymentPtge<-car::recode(discCont$disc_ServicesUnemploymentPtge,
                                               "c('(-0.1,61.8]','(61.8,62.2]','(62.2,63.8]')='(-0.1,63.8]'")

aggregate(varObjCont, by=list(discCont$disc_AutonomosPtge), mean)
discCont$disc_AutonomosPtge<-car::recode(discCont$disc_AutonomosPtge,
                                               "c('(10.7,11.9]','(11.9,12.3]')='(107,12.3]'")

apply(discCont,2,freq)

# Agregamos todos los datos y transformaciones juntas:
#Finalmente, se dene el data frame datos todocont, el cual auna varObjCont, las variables
#input orginales, sus transformadas y sus discretizadas.
datos_todocont<-data.frame(varObjCont,inputCont,TransfCont,discCont)
names(datos_todocont)


lincorCont <- cor(data.frame(varObjCont,Filter(is.numeric, inputCont)))
high_corr <- get_corr(lincorCont , 0.95)
high_corr
# No se detectaron variables altamente correlacionadas.


high_corr_todocont <- get_corr(cor(data.frame(varObjCont,Filter(is.numeric, datos_todocont[,-(1:1)]))) , 0.95)
high_corr_todocont
# No se detectaron variables altamente correlacionadas.



# quitar variables no transformadas
lincor_todocont <- cor(data.frame(varObjCont,Filter(is.numeric, datos_todocont[,-(1:1)])))
lincor_todocont 

# No drops
#drops <- c("ComercTTEHosteleriaPtge", "PobChange_pct", "AutonomosPtge", "inv_ConstruccionPtge")
#red_datos_todocont <- datos_todocont[ , !(names(datos_todocont) %in% drops)]

# No change 
high_corr_todocont_new <- get_corr(cor(data.frame(varObjCont,Filter(is.numeric, datos_todocont[,-(1:1)]))) , 0.95)
high_corr_todocont_new


# Como ya hemos comentado, realizar una partición train-test es habitual en minería de datos, pues nos permite
# evaluar los modelos de una manera más realista y evitar el sobreajuste. Por ello, comenzamos dividiendo el
# conjunto de datos:
names(datos_todocont)

#rm(trainIndex,data_train,data_test )

# --------------------------------------------------------------------------------
# COMPARACION DE MODELOS
#
# --------------------------------------------------------------------------------
#

set.seed(12345)
#trainIndex    <- createDataPartition(datosCont$varObjCont, p=0.8, list=FALSE)
#data_train    <- datosCont[trainIndex,]
#data_test     <- datosCont[-trainIndex,]
#summary(data_test)
trainIndex    <- createDataPartition(datos_todocont$varObjCont, p=0.8, list=FALSE)
data_train    <- datos_todocont[trainIndex,]
data_test     <- datos_todocont[-trainIndex,]
summary(data_test)
names(data_test)

# 
# 1. Selección de variables con las input originales
# 
# MODELO MANUAL Y ModeloStepAIC Y modeloBackAIC
#

# Excluyo Censo
modelo_manual<-lm(varObjCont~CCAA+Population+WomanPopulationPtge+UniversityPtge+
               Densidad+PartidoCCAA+UnemploymentPtge+
               AgricultureUnemploymentPtge+ServicesUnemploymentPtge+AutonomosPtge,
             data=data_train)
summary(modelo_manual)

Rsq(modelo_manual,"varObjCont",data_train)
Rsq(modelo_manual,"varObjCont",data_test)
par(mar=c(6, 15, 4.1, 2.1))
#importanciaVariables(modelo_manual)
modelo_manual$rank

#seleccionamos variables con las inputs originales
null<-lm(varObjCont~1, data=data_train) #Modelo minimo
full<-lm(varObjCont~., data=data_train[,c(1:21)])#Modelo maximo, seleccionamos solo las columnas de las variables originales

modeloStepAIC<-step(null, scope=list(lower=null, upper=full), direction="both", trace=F)
Rsq(modeloStepAIC,"varObjCont",data_train)
Rsq(modeloStepAIC,"varObjCont",data_test)
modeloStepAIC$rank

modeloBackAIC<-step(full, scope=list(lower=null, upper=full), direction="backward", trace=F)
Rsq(modeloBackAIC,"varObjCont",data_train)
Rsq(modeloBackAIC,"varObjCont",data_test)
modeloBackAIC$rank

modelos<-list(modelo_manual,modeloStepAIC,modeloBackAIC)
sapply(modelos,function(x) x$rank) 
sapply(modelos,function(x) Rsq(x,"varObjCont",data_train))
sapply(modelos,function(x) Rsq(x,"varObjCont",data_test))
#Los dos modelos AIC son equivalentes.
#[1] 17 26 30
#[1] 0.5023349 0.5090492 0.5097055
#[1] 0.5257372 0.5305401 0.5328425

#Reduced:
#[1] 17 27 27
#[1] 0.5067912 0.5129478 0.5129478
#[1] 0.5092384 0.5168516 0.5168516


# --------------------------------------------------------------------------------
#
#   ModeloStepBIC Y modeloBackBIC

modeloStepBIC<-step(null, scope=list(lower=null, upper=full), direction="both",
                    k=log(nrow(data_train)),trace=F)
Rsq(modeloStepBIC,"varObjCont",data_train)
Rsq(modeloStepBIC,"varObjCont",data_test)
modeloStepBIC$rank
#[1] 0.4914671
#[1] 0.5116847
#[1] 16
# 16 degrees, pero representa una cantidad inferior al anterior. 

##reduced
#[1] 0.5101304
#[1] 0.5133342
#[1] 19

modeloBackBIC<-step(null, scope=list(lower=null, upper=full), direction="backward",
                    k=log(nrow(data_train)),trace=F)
Rsq(modeloBackBIC,"varObjCont",data_train)
Rsq(modeloBackBIC,"varObjCont",data_test)
modeloBackBIC$rank
# Produce error. 





# --------------------------------------------------------------------------------
#
# 3. iputs originales y Transformadas, Modelos: ModeloStepAIC Y modeloBackAIC
#

# 
# Selección de variables con las input originales y Transformaciones
# 

fullT <- lm(varObjCont~., data=data_train[,c(1:35)])

modeloStepAIC_trans<-step(null, scope=list(lower=null, upper=fullT), direction="both"
                          ,trace=F)
Rsq(modeloStepAIC_trans,"varObjCont",data_train)
Rsq(modeloStepAIC_trans,"varObjCont",data_test)
modeloStepAIC_trans$rank
#[1] 0.5269694
#[1] 0.5233147
#[1] 31

modeloStepBIC_trans<-step(null, scope=list(lower=null, upper=fullT), direction="both",
                          k=log(nrow(data_train)),trace=F)

Rsq(modeloStepBIC_trans,"varObjCont",data_train)
Rsq(modeloStepBIC_trans,"varObjCont",data_test)
modeloStepBIC_trans$rank
#[1] 0.5215972
#[1] 0.5236025
#[1] 23








# 
# 4. Selección de variables con las input originales, transformadas y discretizadas:
#

fulltodo<-lm(varObjCont~., data=data_train[,c(1:49)])
modeloStepAIC_todo<-step(null, scope=list(lower=null, upper=fulltodo), direction="both"
                         ,trace=F)
Rsq(modeloStepAIC_todo,"varObjCont",data_train)
Rsq(modeloStepAIC_todo,"varObjCont",data_test)
modeloStepAIC_todo$rank


modeloStepBIC_todo<-step(null, scope=list(lower=null, upper=fulltodo), direction="both",
                         k=log(nrow(data_train)),trace=F)

Rsq(modeloStepBIC_todo,"varObjCont",data_train)
Rsq(modeloStepBIC_todo,"varObjCont",data_test)
modeloStepBIC_todo$rank











# --------------------------------------------------------------------------------
#
# 2. MODELO MANUAL iputs originales e interacionales Y ModeloStepAIC Y modeloBackAIC
#

# 
# Selección de variables con las input originales e interacciones
# 
#modelo_manual_inter<-lm(varObjCont~CCAA+Censo+Population+WomanPopulationPtge+UniversityPtge+
#                    Densidad+PartidoCCAA+UnemploymentPtge+
#                    AgricultureUnemploymentPtge+ServicesUnemploymentPtge+AutonomosPtge+
#                      PartidoCCAA:Age_over65_Ptge,
#                  data=data_train)
#summary(modelo_manual_inter)




fullInt      <- lm(varObjCont~.^2, data=data_train[,c(1:21)])
#fullInt_red  <- update(fullInt, . ~ . -M_AutonomosPtge-WomenUnemploymentPtge-Age_under19_Ptge-M_UnemployLess25_Ptge-
#                        M_IndustryUnemploymentPtge-M_PobChange_pct-UnemployMore40_Ptge-M_AgricultureUnemploymentPtge-
#                        M_ConstructionUnemploymentPtge)
#names(fullInt_red)

modeloStepAIC_int<-step(null, scope=list(lower=null, upper=fullInt), direction="both", trace=F)
Rsq(modeloStepAIC_int,"varObjCont",data_train)
Rsq(modeloStepAIC_int,"varObjCont",data_test)
modeloStepAIC_int$rank
#[1] 0.5749985
#[1] 0.518143
#Warning message:
#  In predict.lm(modelo, datos) :
#  prediction from rank-deficient fit; attr(*, "non-estim") has doubtful cases
# modeloStepAIC_int$rank
#[1] 134
# Es algo mejor que los anteriores, pero tiene muchos parametros.

###here
# Pending of running:
modeloStepBIC_int<-step(null, scope=list(lower=null, upper=fullInt), direction="both",
                        k=log(nrow(data_train)),trace=F)
Rsq(modeloStepBIC_int,"varObjCont",data_train)
Rsq(modeloStepBIC_int,"varObjCont",data_test)
modeloStepBIC_int$rank
#[1] 0.5298591
#[1] 0.5420312
#[1] 34

# Es un poco peor, pero tiene significativamente menos parametros.













#
# 5. Selección de variables con las input originales, transformadas, discretizadas e interacciones
#

fullIntT<-lm(varObjCont~.^2, data=data_train)
fullIntT_red <- update(fullIntT, . ~ . -CCAA:PartidoCCAA  )


modeloStepBIC_todoInt<-step(null, scope=list(lower=null, upper=fullIntT), direction="both",
                            k=log(nrow(data_train)),trace=F)
Rsq(modeloStepBIC_todoInt,"varObjCont",data_test)




# COMPARACION

modelos<-list(modelo_manual,modeloStepAIC,modeloBackAIC,modeloStepBIC,modeloBackBIC,
              modeloStepAIC_int,modeloStepBIC_int,
              modeloStepAIC_trans,modeloStepBIC_trans,modeloStepAIC_todo,modeloStepBIC_todo,modeloStepBIC_todoInt) #incluir los modelos que se desee comparar

sapply(modelos,function(x) x$rank)
sapply(modelos,function(x) Rsq(x,"varObjCont",data_train))
sapply(modelos,function(x) Rsq(x,"varObjCont",data_test))






#rm(trainIndex,data_train,data_test )












