

library(readr)
nba <- read_csv("nba.csv")
View(nba)
library(tidyverse)
attach(nba)
library(fBasics)
library(car)
library(dplyr)
library(ggplot2)
library(knitr)
library(MASS)
library(corrplot)
library(PerformanceAnalytics)
library(gvlma)





##Variables de nba :

#1:Player           Nombre del jugador.
#2: Salary          Salario del jugador.
#3:NBA_Country      Nacionalidad del jugador.
#4:NBA_DraftNumber  Número del draft.
#5:Age              Edad del jugador.
#6: Tm              Abreviatura del equipo
#7: G               Partidos jugados.
#8: MP              Minutos jugados.
#9: PER             Eficiencia de jugador.
#10: TS%            Porcentaje de tiro.
#11: 3PAr           % de triples
#12 FTr             % de tiros libres
#13: ORB%           % Rebotes Ofensivos ganados
#14: DRB%           % Rebotes defensivos ganados
#15:TRB%            % Rebotes totales
#16: AST%           % Asistencia
#17: STL%           % Robos
#18 BLK%            % Bloqueos
#19: TOV%           % Robo previo a tiro
#20: USG%           % de participacion en jugadas
#21: OWS            Acciones en ataque acertadas
#22:DWS             Acciones defensivas acertadas
#23: WS             Victorias contribuidas
#24:WS/48           Ratio de contribución por partido
#025: OBPM          +/- rendimiento respecto al equipo (cada 100 jugadas en ataque)
#26:DBPM            +/- rendimiento respecto al equipo (cada 100 jugadas en defensa)
#27:BPM             +/- rendimiento respecto al equipo (cada 100 posesiones generales)
#28:VORP            Valor respecto a jugador involucrado en el cambio


#Observaciones generaless

length(nba)                 #Numero de columnas
dim(nba)                    #Nº filas y columnas
head(nba)                   #Primeras Observaciones 
class(nba)                  #Comprobamos su clase
str(nba)                    #Estructura interna de nba            
variable.names(nba)         #Nombre de las variables
summary(nba)                #Muestra general de estadisticos más basicos.



a<-sapply(nba, function(nba) sum(is.na(nba)))               #Pedimos los nulos de nba
sum(a)                                                      #Hay un total de 8 valores nulos
na.omit(nba)                                                #Los eliminamos





distinct(nba)
distinct(nba, Player)
duplicated(nba)
nrow(nba[duplicated(nba$Player), ]) 
nba <- nba[!duplicated(nba$Player), ] 
distinct(nba) 
summarise_all(nba, funs(sum(is.na(.)))) 
na.omit(nba)
 








#Modificamos Variables para trabajarlas mejor

nba <- rename_with(nba, ~ tolower(gsub('%', '', .x, fixed = T)))
nba <- rename_with(nba, ~ tolower(gsub('3', 'three', .x, fixed = T)))
nba <- rename_with(nba, ~ tolower(gsub('/', '_', .x, fixed = T)))

nba






#Realizamos la regresión lineal con las variables consideradas convenientes

Regres01=lm(log(salary,base = 20)~nba_draftnumber+age+g+mp+per+ts+threepar+ftr+orb+drb+trb+ast+stl+blk+tov+usg+ows+dws+ws+ws_48+obpm+dbpm+bpm+vorp, data=nba)         
Regres01
plot(Regres01)






#Realizamos un qqplot para comprobar de que modo se da la distribución dentro de la muestra

qqPlot(Regres01, labels=row.names(nba), id.method="identify",
       simulate=TRUE, main="Q-Q Plot")




#HISTOGRAMA + DENSIDAD + NORMAL + RUG

residplot <- function(fit, nbreaks=10) {
  z <- rstudent(fit)
  hist(z, breaks=nbreaks, freq=FALSE,
       xlab="Studentized Residual",
       main="Distribution of Errors")
  rug(jitter(z), col="brown")
  curve(dnorm(x, mean=mean(z), sd=sd(z)),
        add=TRUE, col="blue", lwd=2)
  lines(density(z)$x, density(z)$y,
        col="red", lwd=2, lty=2)
  legend("topright",
         legend = c( "Normal Curve", "Kernel Density Curve"),
         lty=1:2, col=c("blue","red"), cex=.7)
}

residplot(Regres01)


#Homocedasticidad
ncvTest(Regres01)

   #Comprobamos por el valor de la p (muy pequeño ) que debemos rechazar la hipotesis nula que supone la existencia de homocedasticidad





#Linealidad de cada una de las variables
crPlots(Regres01)







#Multicolinealidad

##Queremos saber hasta que punto hay presencia de multicolinealidad, ya que una alta presencia de la misma puede generar serios problemas ya que los intervalos de confianza serian demasiado amplios.

vif(Regres01) 
sqrt(vif(Regres01)) > 2

 #Comprobamos la posible existencia de alta multicolinealidad por parte de la mayoria de las variables seleccionadas, savo nba_draftnumber,age,ftr,ast,stl y tov.  




#Realizamos el Test de validacion global de Peña para contrastar las hipotesis del modelo

library(gvlma)
gvmodel <- gvlma(Regres01) 
summary(gvmodel)

   #Podemos comprobar como solo satisface la presencia de heterocedasticidad con un nivel de significacion del 5%




##Outliers, distorsionan la normalidad
# Son valores significativamente distintos al resto de los datos, en el caso de un supuesto rechazo concluiriamos que no se da su existencia


outlierTest(Regres01)      #El valor que altera la normalidad es 352


##Buscamos valores extremos

hat.plot <- function(fit) {
  p <- length(coefficients(fit))
  n <- length(fitted(fit))
  plot(hatvalues(fit), main="Index Plot of Hat Values")
  abline(h=c(2,3)*p/n, col="red", lty=2)
  identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}
hat.plot(Regres01)



#Valores influyentes


   # Utilizamos la distancia de Cook
   # identificamos valores D > 4/(n-k-1) 

cutoff <- 4/(nrow(nba)-length(Regres01$coefficients)-2)
plot(Regres01, which=4, cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")

# Added variable plots
# add id.method="identify" to interactively identify points


avPlots(Regres01, ask = FALSE, id.method = "identify")

influencePlot(Regres01, id.method="identify", main="Influence Plot",  sub="Circle size is proportial to Cook's Distance" )


  #Puntos de influencia en 114,143,166 y 326







#Seleccion

stepAIC(Regres01, direction="both")


reg_02<-lm(log(salary, base = 20) ~ nba_draftnumber + age + mp + per + ts + 
  trb + ast + tov + usg + dws + ws_48 + obpm + bpm,data=nba)
reg_02


summary(lm(formula = log(salary, base = 20) ~ nba_draftnumber + age + 
             mp + per + ts + trb + ast + tov + usg + dws + ws_48 + obpm + 
             bpm, data = nba))













####--------------------------------
cor.test(nba)
cor.test(nba$salary,nba$nba_draftnumber)
cor.test(nba$Salary,nba$PER)
cor.test(nba$Salary,nba$'TS%')
cor.test(nba$Salary,nba$'3PAr')
cor.test(nba$salary,nba$g)
cor.test(nba$salary,nba$ows)








           



