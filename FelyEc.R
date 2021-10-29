#limpio la memoria
rm(list=ls())

#Selecciono el CRAN
options(repos="http://mirror.fcaglp.unlp.edu.ar/CRAN/")



#cargo la libreria en cuestión
library(plm)


#cargo el archivo que está en el escritorio
tesis<-read.table("C:/Users/lcaravaggio/Desktop/Felicidad 2014/csv/tesis5.1.csv",header=TRUE,sep=",")


#no estoy seguro si es necesario attachar
#attach(tesis)


#ahora hay que convertir los datos a numéricos... 
tesis$actual<-as.numeric(as.character(tesis$actual))
tesis$futura<-as.numeric(as.character(tesis$futura))
tesis$pbi<-as.numeric(as.character(tesis$pbi))
tesis$felicidad<-as.numeric(as.character(tesis$felicidad))

#Creo una matriz con año país indexado
E <- pdata.frame(tesis, index = c("id", "year"), drop.index = TRUE, row.names = TRUE)




#creo la variable promedio
E$promedio<-(E$felicidad+E$actual+E$futura)/3

#creo las variables en tasa
E$tfelicidad <- (E$felicidad - lag(E$felicidad))/lag(E$felicidad)
E$tpromedio <- (E$promedio- lag(E$promedio))/lag(E$promedio)
E$tpbi <- (E$pbi-lag(E$pbi))/lag(E$pbi)


#creo las variables en tasa de las variables estandarizadas (a media cero por país)
E$tfelicidade <- (E$felicidade-lag(E$felicidade))/lag(E$felicidade)
E$tpromedioe <- (E$promedioe-lag(E$promedioe))/lag(E$promedioe)






#Y listo! a correr modelos!




# Estimador Fixed Effects

# Sobre la tasa de la felicidad estandarizada con la tasa del pbi
summary(plm(felicidade~lag(tpbi,1:3),data=E,model="within"))
# Sobre la tasa de la variable promedio estandarizada con la tasa de pbi
summary(plm(promedioe~lag(tpbi,1:3),data=E,model="within"))
# Inversiones
summary(plm(tpbi~lag(felicidade,1:3),data=E,model="within"))
summary(plm(tpbi~lag(promedioe,1:3),data=E,model="within"))


# Paneles dinámicos
summary(pgmm(tpbi~lag(felicidade,1:4)+lag(tpbi,1:2)|lag(felicidade, 1:99),data=E,effect="individual",model="twosteps"))
summary(pgmm(tpbi~lag(promedioe,1:4)+lag(tpbi)|lag(promedioe, 1:99),data=E,effect="individual",model="twosteps"))


summary(pgmm(felicidade~lag(felicidade,1:2)+lag(tpbi,1:2)|lag(felicidade, 1:99),data=E,effect="individual",model="twosteps"))
summary(pgmm(promedioe~lag(promedioe,1:2)+lag(tpbi,1:2)|lag(promedioe, 1:99),data=E,effect="individual",model="twosteps"))










------------
Versiones viejas de modelos














# Paneles dinámicos
summary(pgmm(felicidade~lag(felicidade,1:2)+lag(tpbi)|lag(felicidade, 1:99),data=E,effect="twoways",model="twosteps"))
summary(pgmm(promedioe~lag(promedioe,1:2)+lag(tpbi)|lag(promedioe, 1:99),data=E,effect="twoways",model="twosteps"))


summary(pgmm(tpbi~lag(felicidade,1:4)+lag(tpbi)|lag(felicidade, 1:99),data=E,effect="twoways",model="twosteps"))
summary(pgmm(tpbi~lag(promedioe,1:4)+lag(tpbi)|lag(promedioe, 1:99),data=E,effect="twoways",model="twosteps"))



# Estimador Random Effects

# Sobre felicidad
summary(plm(felicidade~lag(tpbi,1:4),data=E,model="random"))
# Sobre promedio (Este da un error, no sé que onda)
summary(plm(promedioe~lag(tpbi,1:4),data=E,model="random"))









# Estimador Fixed Effects

# Sobre la tasa de la felicidad estandarizada con la tasa del pbi, con gini como control
summary(plm(tfelicidade~lag(log(tpbi),0:2)+lag(log(gini)),data=E,model="within"))
# Sobre la tasa de la variable promedio estandarizada con la tasa de pbi, con gini como control
summary(plm(tpromedioe~lag(log(tpbi),0:2)+lag(log(gini)),data=E,model="within"))



# Estimador Random Effects

# Sobre felicidad
summary(plm(tfelicidade~lag(log(tpbi),0:1)+log(gini),data=E,model="random"))
# Sobre promedio
summary(plm(tpromedioe~lag(log(tpbi),0:1)+log(gini),data=E,model="random"))




# Paneles dinámicos

summary(pgmm(tfelicidade~lag(felicidade,3:5)+tpbi|lag(tfelicidade, 1:99),data=E,effect="twoways",model="twosteps"))
summary(pgmm(tpromedioe~lag(promedioe,3:5)+tpbi|lag(tfelicidade, 1:99),data=E,effect="twoways",model="twosteps"))







# Estimador Fixed Effects
# Sobre la tasa de la felicidad con la tasa del pbi, sin variables de control
summary(plm(tfelicidad~lag(tpbi),data=E,model="within"))



# Estimador Fixed Effects
tesis.fe <- plm(felicidad~log(lag(pbi))+actual+futura,data=E,model="within")
summary(tesis.fe)

# Estimador Random Effects
tesis.re <- plm(felicidad~log(lag(pbi))+actual+futura,data=E,model="random")
summary(tesis.re)


summary(fixef(tesis.fe, type = 'dmean'))
summary(fixef(plm(felicidad~log(lag(pbi))+actual+futura,data=E,model="within",effect="twoways"),effect="time"))



--

	
