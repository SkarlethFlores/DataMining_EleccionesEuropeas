


# Importat Funciones
#/Users/admin/Master_2023-UCM/7-MineriaDatosModelizacionPredictiva/Evaluacion/
path_funciones   <- "/Users/admin/Master_2023-UCM/7-MineriaDatosModelizacionPredictiva/Evaluacion/FuncionesMineria.R"
source(path_funciones, encoding = "UTF-8")


# Instalar librerias si es necesario:
# install.packages("questionr")
# install.packages("Hmisc")
# install.packages("corrplot")
# install.packages("caret")
# install.packages("OneR")


# 2- Importacion de datos y depuración

### Librerias a utilizar 

library(car)
library(readxl) 
library(questionr) 
library(Hmisc) 
library(corrplot)
library(ggplot2)
library(tidyr)
#library(caret)
#library(OneR)



# Import DATA

ruta   <- "/Users/admin/Master_2023-UCM/7-MineriaDatosModelizacionPredictiva/Evaluacion/DatosEleccionesEuropeas2019.xlsx"
Datos1 <-read_xlsx(ruta) 
#Datos1

# Excluimos las columnas que no son de interes:
Datos   <- Datos1[,!(names(Datos1) %in% c("Abstenciones", "Blancos", "Nulos", "Cs", "PP", "VOX", "Podemos", "Otros"))]
Datos 

### Comprobación de la correcta tipología y rol de las variables:
str(Datos)

# Asignacion de variables como factores:
Datos[,c(4,5,22)] <- lapply(Datos[,c(4,5,22)], as.factor)
str(Datos)

### Análisis exploratorio de los datos y corrección de errores detectados.
#Comprobar que todas las variables numericas tengan al menos 10 valores diferentes. 
sapply(Filter(is.numeric, Datos),function(x) length(unique(x)))

###  Análisis descriptivo univariante
#### Y utilizando la función summary realizamos un primer análisis descriptivo básico.
summary(Datos)


freq(Datos$PartidoMasVotado)

freq(Datos$CCAA)



### A. Tratamiento de datos faltantes.

# Descartar variables con alto numero de NA's
Datos   <- Datos[,!(names(Datos) %in% c("IndustriaPtge", "ConstruccionPtge", "ComercTTEHosteleriaPtge", "ServiciosPtge"))]
summary(Datos)

### C. Reemplazar los valores mayores a 100.
#Todas estas categorias presentan valores con porcentajes de 100%, estos pueden ser datos atipicos.
#PobChange_pct, UnemploymentPtge, AutonomosPtge 
Datos$UnemploymentPtge<-replace(Datos$UnemploymentPtge, which(Datos$UnemploymentPtge>100),NA)
Datos$PobChange_pct<-replace(Datos$PobChange_pct, which(Datos$PobChange_pct>100),NA)  
Datos$AutonomosPtge<-replace(Datos$AutonomosPtge, which(Datos$AutonomosPtge>100),NA)  
summary(Datos) 


### D. Categorías poco representadas en la variables: CCAA, PartidoMasVotado
# muchos partidos tienen baja freq. Asi que agruparemos en 3 categorias: otros, PSOE, PP

for (key in unique(Datos$PartidoMasVotado)){
  if (key == 'PP' |key == 'PSOE' | is.na(key)){
    next
  }
  # PSC es el mismo partido que PSOE
  if (key == 'PSC' ){ 
    Datos$PartidoMasVotado<-car::recode(Datos$PartidoMasVotado, "key = 'PSOE'")
    next
  }
  Datos$PartidoMasVotado<-car::recode(Datos$PartidoMasVotado, "key = 'Otro'")
}
freq(Datos$PartidoMasVotado)


#La variable CCAA, presenta varios registros en varias categorias, agruparemos las comunidades, 
#usando criterios de similitud proximidad geografica y socioeconomica.

# Agruparemos CCAA con freq<5%
#freq(Datos$CCAA) 
for (key in unique(Datos$CCAA)){
  if (key == 'Baleares' |key == 'ComValenciana' ){
    Datos$CCAA <- car::recode(Datos$CCAA, "key = 'Levante'")
  }
  if (key == 'Galicia' |key == 'Asturias' |key == 'Cantabria'){
    Datos$CCAA <- car::recode(Datos$CCAA, "key = 'NorOeste'")
  }
  if (key == 'Murcia' |key == 'Ceuta' |key == 'Melilla' | key == 'Canarias' |key == 'Andalucía' |key == 'Extremadura'){
    Datos$CCAA <- car::recode(Datos$CCAA, "key = 'Sur'")
  } 
  if (key == 'CastillaLeón' |key == 'CastillaMancha' |key == 'Madrid' | key == 'Rioja'){
    Datos$CCAA <- car::recode(Datos$CCAA, "key = 'Centro'")
  }  
  if (key == 'PaísVasco' |key == 'Navarra'){
    Datos$CCAA <- car::recode(Datos$CCAA, "key = 'PVasco_Navarra'")
  } 
}
freq(Datos$CCAA) 



### Detección de datos atípicos.

#Buscar outliers:
#hist(Datos$UnemploymentPtge,xlab="Unemployment %",main="")
par(mfrow = c(2, 3))
boxplot(Datos$Empresas, xlab="Empresas")
boxplot(Datos$Densidad, xlab="Densidad")
boxplot(Datos$Explotaciones, xlab="Explotaciones")
hist(Datos$Empresas,xlab="Empresas",main="")
hist(Datos$Densidad,xlab="Densidad",main="")
hist(Datos$Explotaciones,xlab="Explotaciones",main="")


#Definir las variables input y output.

drops_input <- c( "PartidoMasVotado " , "PSOE" , "VotosEmitidos " , 'CodigoINE ' )
var_input <- Datos[, !( names(Datos) %in% c( "PartidoMasVotado" , "PSOE" , "VotosEmitidos" , 'CodigoINE' ) ) ]
#var_input
row.names ( var_input ) <- Datos$CodigoINE


var_output    <- as.data.frame ( Datos[, c( "PartidoMasVotado" , "PSOE" , "VotosEmitidos" ) ] )
#var_output
# ## OUTPUTS
colMeans(is.na(variables_output)) 





# DEFINIR OUTPUT
# variables_output<-as.data.frame(datos[,c("PartidoMasVotado", "PSOE", "VotosEmitidos")])
Datos$PSOE
varObjCont <- round(100*Datos$PSOE/Datos$VotosEmitidos, digits = 2)











#El criterio escogido para detectar outliers en esta practica son los valores tres veces mayores
#del rango intercuartilico Q1-Q3, si estos suponen menos del 5% del conjunto de datos.
# Producimos los graficos
for (i in 1:length(colnames(var_input)) ) {
  col_name <- colnames(var_input)[i]
  if (col_name== 'CodigoINE'| col_name== 'CCAA'| col_name== 'PartidoCCAA' ){next}
  print(col_name)
  png(paste0("raw_",col_name, ".png"))
  boxplot( var_input[,i], main = paste0("raw_",col_name) )
  dev.off()
}

str(var_input)
cols_outliers<- c( "AgricultureUnemploymentPtge", "AutonomosPtge", "ConstructionUnemploymentPtge", "ForeignersPtge", "IndustryUnemploymentPtge",
                   "PobChange_pct", "ServicesUnemploymentPtge", "UnemployLess25_Ptge", "UnemploymentPtge",  "UniversityPtge",   "WomanPopulationPtge",  "WomenUnemploymentPtge")

#  "IndustriaPtge", "Superficie",  "Censo", "Empresas", "Densidad", "Explotaciones", "Population")

#  colnames(Filter(is.numeric, var_input))
for (i in cols_outliers){
  print(i)
  outliers(paste0("var_input$",i))
}



# ¿Hay observaciones con mas del 50% missings?

var_input$prop_missings<-rowMeans(is.na(var_input))
summary(var_input$prop_missings)
summary(var_input)

(prop_missingsVars<-colMeans(is.na(var_input)))

#subset(var_input,prop_missings<0.5,select=names(prop_missingsVars)[prop_missingsVars<0.5])
#Las variable: UnemployLess25_Ptge cuenta con un porcentaje de missing del 2.4%. 
summary(var_input)
var_input <-  subset(var_input,prop_missings<0.5,select=names(prop_missingsVars)[prop_missingsVars<0.5])
summary(var_input)


varObjCont <- varObjCont[var_input$prop_missings < 0.5]
#varObjCont






#En cuanto a las variables cuantitativas, dado que no podemos recategorizarlas directamente, antes de la
#imputación creamos tantas variables indicadoras de ausentes como variables tengan una proporción suficiente
#de ausentes.
var_input$M_PobChange_pct                 <-factor(is.na(var_input$PobChange_pct ))  
var_input$M_PersonasInmueble              <-factor(is.na(var_input$PersonasInmueble ))  
var_input$M_UnemployLess25_Ptge           <-factor(is.na(var_input$UnemployLess25_Ptge ))  
var_input$M_UnemployMore40_Ptge          <-factor(is.na(var_input$UnemployMore40_Ptge ))  
var_input$M_AgricultureUnemploymentPtge   <-factor(is.na(var_input$AgricultureUnemploymentPtge ))  
var_input$M_IndustryUnemploymentPtge      <-factor(is.na(var_input$IndustryUnemploymentPtge ))  
var_input$M_ConstructionUnemploymentPtge  <-factor(is.na(var_input$ConstructionUnemploymentPtge ))  
var_input$M_AutonomosPtge                 <-factor(is.na(var_input$AutonomosPtge ))  


var_input[,as.vector(which(sapply(var_input, class)=="numeric"))] <-  sapply(Filter(is.numeric, var_input),function(x) impute(x,"random"))


length(unique(var_input$prop_missings))
# Tratar prop_missings como factor? ->SI 

freq(var_input$prop_missings)

var_input$prop_missings<-as.factor(var_input$prop_missings)
freq(var_input$prop_missings)

var_input$prop_missings <- car::recode(var_input$prop_missings,  "c(0.0434782608695652,0.0869565217391304,0.130434782608696,0.173913043478261,0.434782608695652)='>0'")
freq(var_input$prop_missings)


summary(var_input)


saveRDS(data.frame(varObjCont,var_input),"datosEleccionesEuropeasDep1")

boxplot_cuantcuali(varObjCont,var_input$CCAA,"varObjCont")

boxplot_cuantcuali(varObjCont,var_input$PartidoCCAA,"varObjCont")

#boxplot_cuantcuali(varObjCont,var_input$Densidad,"varObjCont")

dispersion(Filter(is.numeric, var_input),varObjCont)


corrplot.mixed(cor(data.frame(varObjCont,Filter(is.numeric, var_input)),
                   use="pairwise", method="pearson"), tl.pos = "lt",
               lower = "number", upper = "ellipse",lower.col = "black", 
               number.cex = .8, number.digits = 1)

# ----------------------
#   MEDICIÓN DE LA RELACIÓN ENTRE VARIABLES
# ----------------------

# para comparar si las variables son significativas o no

var_input$aleatorio  <-runif(nrow(var_input))
var_input$aleatorio2 <-runif(nrow(var_input))


## VARIABLE CONTINUA

par(mar=c(15, 4.0, 4.1, 2.1)) #Para ajustar los márgenes del gráfico
graficoVcramer(var_input,varObjCont)

names(var_input)


var_input2<-var_input[,-c(26,28)]
names(var_input2)


saveRDS(data.frame(varObjCont,var_input2),"datosEleccionesEuropeasDep2")
