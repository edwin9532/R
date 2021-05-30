rm(list = ls())

library(wesanderson)
library(hrbrthemes)
library(openxlsx)
library(dplyr)
library(summarytools)
library(ggplot2)

setwd("C:/Users/elave/OneDrive/Escritorio/Taller Enlec")

base <- read.xlsx("baseConsolidadaENLEC.xlsx")

# CATEGORIZACIÓN POR RANGOS DE EDAD

base_re <- base %>% 
  mutate(RangosEdad = case_when(P5785 >=0 & P5785 <18 ~ "Menor de 18 años",
                                P5785 >=18 & P5785 <=24 ~ "De 18 a 24 años",
                                P5785 >=25 & P5785 <=34 ~ "De 25 a 34 años",
                                P5785 >=35 & P5785 <=44 ~ "De 35 a 44 años",
                                P5785 >=45 & P5785 <=54 ~ "De 45 a 54 años",
                                P5785 >=55 ~ "Mayor de 55 años"))

# FRECUENCIA DE VARIABLES DE INTERÉS

f_Zona <- base_re %>%
  freq(DOMINIO, report.nas = FALSE)

f_Sexo <- base_re %>%
  freq(P6020, report.nas = FALSE)

f_Edad <- base_re %>%
  freq(RangosEdad, report.nas = FALSE)

f_Educ <- base_re %>%
  freq(P260, report.nas = FALSE)

f_EstCiv <- base_re %>%
  freq(P6070, report.nas = FALSE)

f_Etnia <- base_re %>%
  freq(P5465, report.nas = FALSE)

f_LecDig <- base_re %>%
  freq(P1855, report.nas = FALSE)

view(freq_Edad)

sum(base_re$DOMINIO==1)/sum(!is.na(base_re$ID))*100 # Porcentaje Urbano
sum(base_re$DOMINIO==2)/sum(!is.na(base_re$ID))*100 # Porcentaje Rural

# ---------- Punto 3 ----------

# Porcentaje de todos los encuestados que NO lee
sum(!is.na(base_re$P1864S1))/sum(!is.na(base_re$ID))*100 # Digital
sum(!is.na(base_re$P1865S1))/sum(!is.na(base_re$ID))*100 # Impreso

# ----- Digital -----

razonesD <- c("Otras preferencias","Dinero","Salud","Pereza","No tiene dispositivos",
              "Duda qué leer","Acceso a internet","No sabe usar dispositivos","Tiempo",
              "Desinterés","Prefiere impreso","Otra")

base_RazNoLecDig <- base_re %>%
  filter(!is.na(P1864S1)) %>% 
  select(RangosEdad,DOMINIO,P260,P4031S1A1,30,34:41,31:33) # Filtra personas que respondieron P1864

names(base_RazNoLecDig)[5:16] <- razonesD # Cambia nombres de las razones

agrupaD <- base_RazNoLecDig %>% # Cuenta frecuencia de los rangos de edad en las razones
  group_by(RangosEdad) %>%
  summarise(OtrasPrefs = sum(`Otras preferencias`==1), Dinero = sum(Dinero==1),
            Salud = sum(Salud ==1), Pereza = sum(Pereza==1), Falta_dis = sum(`No tiene dispositivos`==1),
            Duda = sum(`Duda qué leer`==1), Internet = sum(`Acceso a internet`==1),
            Uso = sum(`No sabe usar dispositivos`==1),Tiempo = sum(Tiempo==1),
            Desinteres=sum(Desinterés==1),Impreso=sum(`Prefiere impreso`==1),Otra=sum(Otra==1)) # colSums()?

razonesD_ <- c(rep("Otras preferencias",6),rep("Dinero",6),rep("Salud",6),rep("Pereza",6),
               rep("No tiene dispositivos",6),rep("Duda qué leer",6),rep("Acceso a internet",6),
               rep("No sabe usar dispositivos",6),rep("Tiempo",6),rep("Desinterés",6),
               rep("Prefiere impreso",6),rep("Otra",6)) # Necesario para el diagrama de barras apiladas posterior

numsD <- c()
for(i in 2:13){
  numsD <- c(numsD,pull(agrupaD,i)) # Une datos numéricos de agrupa en un solo vector
}

tres_Dig <- data.frame(razonesD_,agrupaD[1],numsD) # Dataframe con los datos necesarios para barras apiladas

tres_plot_Dig <- ggplot(tres_Dig, aes(fill=factor(RangosEdad,
                                                  levels = c("Menor de 18 años","De 18 a 24 años",
                                                             "De 25 a 34 años","De 35 a 44 años",
                                                             "De 45 a 54 años","Mayor de 55 años")),
                                      y=numsD, x=reorder(razonesD_,numsD))) + 
  geom_bar(position=position_stack(reverse=F), stat="identity", colour="black",size=.25) +
  coord_flip()+
  scale_fill_brewer(palette ="Set3") +
  #scale_fill_manual(values=wes_palette("Royal2"))
  labs(x="Razón por la que no lee digital",y="Frecuencia",fill="Rango de Edad",
       title="Razones Digital") # Barras apiladas


# Tendencia con la edad, más viejo/joven -> menos digital

tres_Dig_f_Edad_ <- base_RazNoLecDig %>% 
  freq(RangosEdad,report.nas=FALSE)

# Zona.

agrupaDZ <- base_RazNoLecDig %>% # Cuenta frecuencia de las zonas en las razones
  group_by(DOMINIO) %>%
  summarise(OtrasPrefs = sum(`Otras preferencias`==1), Dinero = sum(Dinero==1),
            Salud = sum(Salud ==1), Pereza = sum(Pereza==1), Falta_dis = sum(`No tiene dispositivos`==1),
            Duda = sum(`Duda qué leer`==1), Internet = sum(`Acceso a internet`==1),
            Uso = sum(`No sabe usar dispositivos`==1),Tiempo = sum(Tiempo==1),
            Desinteres=sum(Desinterés==1),Impreso=sum(`Prefiere impreso`==1),Otra=sum(Otra==1))

razonesDZ <- c(rep("Otras preferencias",2),rep("Dinero",2),rep("Salud",2),rep("Pereza",2),
               rep("No tiene dispositivos",2),rep("Duda qué leer",2),rep("Acceso a internet",2),
               rep("No sabe usar dispositivos",2),rep("Tiempo",2),rep("Desinterés",2),
               rep("Prefiere impreso",2),rep("Otra",2)) # Necesario para el diagrama de barras apiladas posterior

numsDZ <- c()
for(i in 2:13){
  numsDZ <- c(numsDZ,pull(agrupaDZ,i)) # Une datos numéricos de agrupa en un solo vector
}

numsDZ_frac <- round(numsDZ/c(sum(base_re$DOMINIO==1),sum(base_re$DOMINIO==2))*100,1)

tres_DigZ <- data.frame(razonesDZ,agrupaDZ[1],numsDZ,numsDZ_frac) # Dataframe con los datos necesarios para barras apiladas

library(plyr)
tres_DigZ <- ddply(tres_DigZ, .(razonesDZ),
                   transform, pos = cumsum(numsDZ_frac) - (0.5 * numsDZ_frac))
detach("package:plyr",unload = T)

tres_plot_DigZfrac <- ggplot(tres_DigZ, aes(fill=factor(DOMINIO,
                                                        levels = c("1","2"),labels = c("Urbana","Rural")),
                                            y=numsDZ_frac, x=reorder(razonesDZ,numsDZ_frac))) + 
  geom_bar(position=position_stack(reverse=T), stat="identity", colour="black") +
  geom_text(data=tres_DigZ,aes(x=razonesDZ,y=pos,label =ifelse(numsDZ_frac>=2.5,
                                                               paste0(numsDZ_frac,"%"),"")), size=4) +
  coord_flip()+
  scale_fill_brewer(palette ="Pastel1") +
  #scale_fill_manual(values=wes_palette("Royal2"))
  labs(x="Razón por la que no lee digital",y="Porcentaje del total de personas de esa zona",fill="Zona",
       title="Razones Digital") # Barras apiladas


# Distribución de los que no leen digital por zona. No aporta mucho, se esperan mas urbs que rurs por los porcentajes de la muestra
sum(base_RazNoLecDig$DOMINIO==1)/sum(!is.na(base_re$P1864S1))*100 # Urbana
sum(base_RazNoLecDig$DOMINIO==2)/sum(!is.na(base_re$P1864S1))*100 # Rural

# Porcentaje de los que no leen digital en todos los de su zona
sum(base_RazNoLecDig$DOMINIO==1)/sum(base_re$DOMINIO==1)*100 # Urbana
sum(base_RazNoLecDig$DOMINIO==2)/sum(base_re$DOMINIO==2)*100 # Rural



# ----- Impreso -----

razonesI <- c("Otras preferencias","Tiempo","Desinterés","Pereza","Dinero","Duda qué leer",
              "Salud","Acceso a material","Prefiere digital","Otra")

base_RazNoLecImp <- base_re %>%
  filter(!is.na(P1865S1)) %>% 
  select(RangosEdad,DOMINIO,P260,P4031S1A1,42,44:51,43)

names(base_RazNoLecImp)[5:14] <- razonesI # Cambia nombres de las razones

agrupaI <- base_RazNoLecImp %>% # Cuenta frecuencia de los rangos de edad en las razones
  group_by(RangosEdad) %>%
  summarise(OtrasPrefs = sum(`Otras preferencias`==1),Tiempo = sum(Tiempo==1),
            Desinteres=sum(Desinterés==1),Pereza=sum(Pereza==1),Dinero = sum(Dinero==1),
            Duda=sum(`Duda qué leer`==1),Salud=sum(Salud==1),Acceso=sum(`Acceso a material`==1),
            Digital=sum(`Prefiere digital`==1),Otra=sum(Otra==1))

razonesI_ <- c(rep("Otras preferencias",6),rep("Tiempo",6),rep("Desinterés",6),rep("Pereza",6),
               rep("Dinero",6),rep("Duda qué leer",6),rep("Salud",6),
               rep("Acceso a material",6),rep("Prefiere digital",6),rep("Otra",6)) # Necesario para el diagrama de barras apiladas posterior

numsI <- c()
for(i in 2:11){
  numsI <- c(numsI,pull(agrupaI,i)) # Une datos numéricos de agrupa en un solo vector
}

tres_Imp <- data.frame(razonesI_,agrupaI[1],numsI) # Dataframe con los datos necesarios para barras apiladas

tres_plot_Imp <- ggplot(tres_Imp, aes(fill=factor(RangosEdad,
                                                  levels = c("Menor de 18 años","De 18 a 24 años",
                                                             "De 25 a 34 años","De 35 a 44 años",
                                                             "De 45 a 54 años","Mayor de 55 años")),
                                      y=numsI, x=reorder(razonesI_,numsI))) + 
  geom_bar(position=position_stack(reverse=F), stat="identity", colour="black",size=.25) +
  coord_flip()+
  scale_fill_brewer(palette ="Set3") +
  #scale_fill_manual(values=wes_palette("Royal2"))
  labs(x="Razón por la que no lee impreso",y="Frecuencia",fill="Rango de Edad",
       title="Razones Impreso") # Barras apiladas

# Frecuencia edad

tres_Imp_f_Edad_ <- base_RazNoLecImp %>% 
  freq(RangosEdad,report.nas=FALSE)

view(tres_Imp_f_Edad_)  


# Zona.

agrupaIZ <- base_RazNoLecImp %>% # Cuenta frecuencia de las zonas en las razones
  group_by(DOMINIO) %>%
  summarise(OtrasPrefs = sum(`Otras preferencias`==1),Tiempo = sum(Tiempo==1),
            Desinteres=sum(Desinterés==1),Pereza=sum(Pereza==1),Dinero = sum(Dinero==1),
            Duda=sum(`Duda qué leer`==1),Salud=sum(Salud==1),Acceso=sum(`Acceso a material`==1),
            Digital=sum(`Prefiere digital`==1),Otra=sum(Otra==1))

razonesIZ <- c(rep("Otras preferencias",2),rep("Tiempo",2),rep("Desinterés",2),rep("Pereza",2),
               rep("Dinero",2),rep("Duda qué leer",2),rep("Salud",2),
               rep("Acceso a material",2),rep("Prefiere digital",2),rep("Otra",2)) # Necesario para el diagrama de barras apiladas posterior

numsIZ <- c()
for(i in 2:11){
  numsIZ <- c(numsIZ,pull(agrupaIZ,i)) # Une datos numéricos de agrupa en un solo vector
}

numsIZ_frac <- round(numsIZ/c(sum(base_re$DOMINIO==1),sum(base_re$DOMINIO==2))*100,1) # Calcula fracción que representa del total de esa zona

tres_ImpZ <- data.frame(razonesIZ,agrupaIZ[1],numsIZ,numsIZ_frac) # Dataframe con los datos necesarios para barras apiladas

library(plyr)
tres_ImpZ <- ddply(tres_ImpZ, .(razonesIZ),
                   transform, pos = cumsum(numsIZ_frac) - (0.5 * numsIZ_frac))
detach("package:plyr",unload = T)

tres_plot_ImpZfrac <- ggplot(tres_ImpZ, aes(fill=factor(DOMINIO,
                                                        levels = c("1","2"),labels = c("Urbana","Rural")),
                                            y=numsIZ_frac, x=reorder(razonesIZ,numsIZ_frac))) + 
  geom_bar(position=position_stack(reverse=T), stat="identity", colour="black") +
  geom_text(data=tres_ImpZ,aes(x=razonesIZ,y=pos,label =ifelse(numsIZ_frac>=1.2,
                                                               paste0(numsIZ_frac,"%"),"")), size=4) +
  coord_flip()+
  scale_fill_brewer(palette ="Pastel1") +
  #scale_fill_manual(values=wes_palette("Royal2"))
  labs(x="Razón por la que no lee impreso",y="Porcentaje del total de personas de esa zona",fill="Zona",
       title="Razones Impreso") # Barras apiladas


# Distribución de los que no leen impreso por zona. NO aporta mucho, 90% urbano y 10% rural, se espera más urb que rur
sum(base_RazNoLecImp$DOMINIO==1)/sum(!is.na(base_re$P1864S1))*100 # Urbana
sum(base_RazNoLecImp$DOMINIO==2)/sum(!is.na(base_re$P1864S1))*100 # Rural

# Porcentaje de los que no leen impreso en todos los de su zona
sum(base_RazNoLecImp$DOMINIO==1)/sum(base_re$DOMINIO==1)*100 # Urbana
sum(base_RazNoLecImp$DOMINIO==2)/sum(base_re$DOMINIO==2)*100 # Rural


# ---------- Punto 4 ----------

base_NiñezEdu <- base_re %>% # Selecciona variables necesarias, excluye <18 por su educación
  filter(RangosEdad!="Menor de 18 años") %>% 
  select(ID,RangosEdad,DOMINIO,P260,P4031S1A1,11:16)

cuat_noms <- c("Educación","Estrato","Iba a bibliotecas",
               "Iba a librerías","Veía leer",
               "Le leían","Profs motivaban leer",
               "Profs motivaban ir a bibs")

names(base_NiñezEdu)[4:11] <- cuat_noms # Cambia nombres de columnas

# Análisis de una pregunta: ¿Veía a familiares leer?

cuat_p1 <- base_NiñezEdu %>% 
  group_by(Educación) %>% 
  summarise(sí = sum(`Veía leer`==1), no = sum(`Veía leer`==2))

numsP1 <- c()
for(i in 2:3){
  numsP1 <- c(numsP1,pull(cuat_p1,i)) # Une datos numéricos de nivs de educ en un solo vector
}

yn <- c(rep(c("Sí"),13),rep(c("No"),13))
eds <- rep(c("Ninguna","Preescolar","Básica primaria","Básica secundaria",
         "Media académica","Media técnica","Normalista","Técnica","Tecnológica",
         "Universitario","Especialización","Maestría","Doctorado"),2)

cuat_p1a <- data.frame(eds,yn,numsP1)

cuat_plot_p1 <- ggplot(cuat_p1a, aes(fill=factor(yn),
              y=numsP1, x=factor(eds,
  levels = c("Ninguna","Preescolar","Básica primaria","Básica secundaria",
  "Media académica","Media técnica","Normalista","Técnica","Tecnológica",
  "Universitario","Especialización","Maestría","Doctorado")))) + 
  geom_bar(position=position_stack(reverse=T), stat="identity", colour="black",size=.1) +
  scale_fill_brewer(palette ="RdYlGn") +
  coord_flip()+
  #scale_fill_manual(values=wes_palette("Royal2"))
  labs(x="Máximo nivel educativo alcanzado",y="Frecuencia",
       fill=stringr::str_wrap("¿Veía a familiares leer?",12),
       title="Respuesta a P1 y nivel educativo") 
# Se esperaría más sí's en niveles más altos, y más no's en más bajos, sin embargo tendencia no muy clara


# Puntajes. Se crea un puntaje con todas las preguntas, en vez de analizar cada una por aparte. Puntaje resume el entorno en la infancia.

cuat_punts <- base_NiñezEdu %>% # Crea puntaje para cada persona
  group_by(ID) %>%
  summarise(RangosEdad = RangosEdad,Educacion = Educación,
            Puntaje = sum(`Iba a bibliotecas`==1)+sum(`Iba a librerías`==1)+sum(`Veía leer`==1)+
              sum(`Le leían`==1)+sum(`Profs motivaban leer`==1)+sum(`Profs motivaban ir a bibs`==1))

cuat_PuntsEduc <- cuat_punts %>% # Cuenta nivs de educación por puntaje
  group_by(Puntaje) %>% 
  summarise(Ninguna=sum(Educacion==0),Preescolar=sum(Educacion==1),`Básica primaria`=sum(Educacion==2),
            `Básica secundaria`=sum(Educacion==3),`Media académica`=sum(Educacion==4),
            `Media técnica`=sum(Educacion==5),Normalista=sum(Educacion==6),`Técnica`=sum(Educacion==7),
            `Tecnológica`=sum(Educacion==8),Universitario=sum(Educacion==9),Especialización=sum(Educacion==10),
            Maestría=sum(Educacion==11),Doctorado=sum(Educacion==12))

numsEdu <- c()
for(i in 2:14){
  numsEdu <- c(numsEdu,pull(cuat_PuntsEduc,i)) # Une datos numéricos de nivs de educ en un solo vector
}

punts <- rep(c(0:6),13)
NivsEdu <- c(rep("Ninguna",7),rep("Preescolar",7),rep("Básica primaria",7),
             rep("Básica secundaria",7),rep("Media académica",7),rep("Media técnica",7),
             rep("Normalista",7),rep("Técnica",7),rep("Tecnológica",7),rep("Universitario",7),
             rep("Especialización",7),rep("Maestría",7),rep("Doctorado",7))

cuat_all <- data.frame(punts,NivsEdu,numsEdu) # une los puntajes, nivs, y sus correspondientes números

cuat_plot <- ggplot(cuat_all, aes(fill=factor(punts),
            y=numsEdu, x=factor(NivsEdu,
            levels = c("Ninguna","Preescolar","Básica primaria","Básica secundaria",
             "Media académica","Media técnica","Normalista","Técnica","Tecnológica",
             "Universitario","Especialización","Maestría","Doctorado")))) + 
  geom_bar(position=position_stack(reverse=T), stat="identity", colour="black",size=.1) +
  scale_fill_brewer(palette ="RdYlGn") +
  coord_flip()+
  #scale_fill_manual(values=wes_palette("Royal2"))
  labs(x="Máximo nivel educativo alcanzado",y="Frecuencia",
       fill=stringr::str_wrap("Puntaje de lectura en la infancia",12),
       title="Lectura en infancia y máximo nivel educativo") # Barras apiladas

# ZONA. Máximo nivel educativo por zona. No número de personas, sino fracción representativa de cada zona.

cuat_NivZona <- base_NiñezEdu %>% 
  group_by(DOMINIO) %>% 
  summarise(Ninguna=sum(Educación==0),Preescolar=sum(Educación==1),`Básica primaria`=sum(Educación==2),
            `Básica secundaria`=sum(Educación==3),`Media académica`=sum(Educación==4),
            `Media técnica`=sum(Educación==5),Normalista=sum(Educación==6),`Técnica`=sum(Educación==7),
            `Tecnológica`=sum(Educación==8),Universitario=sum(Educación==9),Especialización=sum(Educación==10),
            Maestría=sum(Educación==11),Doctorado=sum(Educación==12))

numsNivZ <- c()
for(i in 2:14){
  numsNivZ <- c(numsNivZ,pull(cuat_NivZona,i)) # Une datos numéricos de nivs de educ en un solo vector
}

numsNivZ_porc <- round(numsNivZ/c(sum(base_re$DOMINIO==1&base_re$RangosEdad!="Menor de 18 años"),
                                  sum(base_re$DOMINIO==2&base_re$RangosEdad!="Menor de 18 años"))*100,1) # Calcula fracción que representa del total de esa zona

for(i in 0:12){
  t <- numsNivZ_porc[2*i+2]
  numsNivZ_porc[2*i+2] <- numsNivZ_porc[2*i+1]
  numsNivZ_porc[2*i+1] <- t
}

doms <- rep(c(2:1),13)
NivsZ <- c(rep("Ninguna",2),rep("Preescolar",2),rep("Básica primaria",2),
             rep("Básica secundaria",2),rep("Media académica",2),rep("Media técnica",2),
             rep("Normalista",2),rep("Técnica",2),rep("Tecnológica",2),rep("Universitario",2),
             rep("Especialización",2),rep("Maestría",2),rep("Doctorado",2))

cuat_NZ_all <- data.frame(doms,NivsZ,numsNivZ_porc)

library(plyr)
cuat_NZ_all <- ddply(cuat_NZ_all, .(NivsZ),
                   transform, pos = cumsum(numsNivZ_porc) - (0.5 * numsNivZ_porc))
detach("package:plyr",unload = T)

cuat_NZ_plot <- ggplot(cuat_NZ_all, aes(fill=factor(doms,
                                                    levels = c("1","2"),labels = c("Urbana","Rural")),
  y=numsNivZ_porc, x=factor(NivsZ,
            levels = c("Ninguna","Preescolar","Básica primaria","Básica secundaria",
                       "Media académica","Media técnica","Normalista","Técnica","Tecnológica",
                       "Universitario","Especialización","Maestría","Doctorado")))) + 
  geom_bar(position=position_stack(reverse=F), stat="identity", colour="black",size=.07) +
  geom_text(data=cuat_NZ_all,aes(x=NivsZ,y=pos,label =ifelse(numsNivZ_porc>=1.2,
                                                               paste0(numsNivZ_porc,"%"),"")), size=3) +
  scale_fill_brewer(palette ="Pastel1") +
  coord_flip()+
  #scale_fill_manual(values=wes_palette("Royal2"))
  labs(x="Máximo nivel educativo alcanzado",y="Porcentaje del total de esa zona",
       fill=stringr::str_wrap("Zona",12),
       title="Máximo nivel educativo y zona") # Barras apiladas



#colours = c( "#A54657",  "#582630", "#F7EE7F", "#4DAA57","#F1A66A","#F26157", "#F9ECCC", "#679289", "#33658A","#F6AE2D","#86BBD8")



# ---------- Punto 5 ----------

# 1 Pregunta, REDES SOCIALES
#1044 excluidos por estrato 0 o 9

base_TiLecEst <- base_re %>% 
  filter(!is.na(P4031S1A1) & P4031S1A1!=0 & P4031S1A1!=9) %>%
  select(P5785,P4031S1A1,P1855S2A1,P1860S2A1,P1862S2A1,P1852S2A1,P1851S2A1)

base_TiLecEst[is.na(base_TiLecEst)] <- 0

colnames(base_TiLecEst) <- c("Rango de Edad","Estrato","Redes sociales","Periódicos impresos",
               "Documentos académicos impresos","Documentos académicos digitales","Páginas web")

cinco_P1 <- base_TiLecEst %>% 
  group_by(Estrato) %>% 
  summarise(`Menos de una hora`=sum(`Redes sociales`==1),`1 hora`=sum(`Redes sociales`==2),
            `2 horas`=sum(`Redes sociales`==3),`3 horas`=sum(`Redes sociales`==4),
            `4 horas`=sum(`Redes sociales`==5),`5 horas`=sum(`Redes sociales`==6),
            `6 a 7 horas`=sum(`Redes sociales`==7),`8 a 10 horas`=sum(`Redes sociales`==8),
            `Más de 10 horas`=sum(`Redes sociales`==9),`No sabe / No informa`=sum(`Redes sociales`==99))

numsTiLecEst <- c()
for(i in 2:11){
  numsTiLecEst <- c(numsTiLecEst,pull(cinco_P1,i)) # Une datos numéricos de nivs de educ en un solo vector
}

p <- c()
for(i in 1:6){
  p = c(p,sum(base_re$P4031S1A1==i))
}

pobs <- rep(p,10)

porcsTiLecEst <- numsTiLecEst/pobs

Tiempo <- c(rep("Menos de una hora",6),rep("1 hora",6),rep("2 horas",6),rep("3 horas",6),
            rep("4 horas",6),rep("5 horas",6),rep("6 a 7 horas",6),rep("8 a 10 horas",6),
            rep("Más de 10 horas",6),rep("No sabe / No informa",6))
Estratos<-rep(1:6,10)

cinco_P1_all <- data.frame(Estratos,Tiempo,porcsTiLecEst)

cinco_plot_P1 <- ggplot(cinco_P1_all, aes(fill=factor(Tiempo,
  levels = c("No sabe / No informa","Más de 10 horas","8 a 10 horas","6 a 7 horas","5 horas",
             "4 horas","3 horas","2 horas","1 hora","Menos de una hora")),
                  y=porcsTiLecEst, x=factor(Estratos))) + 
  geom_bar(position=position_stack(reverse=F), stat="identity", colour="black",size=.1) +
  scale_fill_brewer(palette ="RdYlGn") +
  #coord_flip()+
  #scale_fill_manual(values=wes_palette("Royal2"))
  labs(x="Estrato",y="Frecuencia",
       fill=stringr::str_wrap("Tiempo semanal de lectura de redes sociales",21),
       title="Lectura de redes sociales y estrato") # Barras apiladas

# Otras 5 preguntas también? Función?
# facet_wrap de las 5?

# 6 Preguntas, sí-no, estratos

cinco_YNEst <- base_TiLecEst %>% 
  group_by(Estrato) %>% 
  summarise(P1S=sum(`Redes sociales`!=0),P1N=sum(`Redes sociales`==0),P2S=sum(`Periódicos impresos`!=0),
            P2N=sum(`Periódicos impresos`==0),P3S=sum(`Documentos académicos impresos`!=0),
            P3N=sum(`Documentos académicos impresos`==0),P4S=sum(`Documentos académicos digitales`!=0),
            P4N=sum(`Documentos académicos digitales`==0),P5S=sum(`Páginas web`!=0),P5N=sum(`Páginas web`==0))

YNnums <- c()
for(i in 2:11){
  YNnums <- c(YNnums,pull(cinco_YNEst,i)) # Une datos numéricos de nivs de educ en un solo vector
}

YNEs <- rep(1:6,10)
YNpregs <- c(rep("Redes sociales",12),rep("Periódicos impresos",12),
             rep("Documentos académicos impresos",12),rep("Documentos académicos digitales",12),
             rep("Páginas web",12))
YN <- rep(c(rep("Sí",6),rep("No",6)),5)

cinco_YNEst_all <- data.frame(YNEs,YNpregs,YN,YNnums)

cinco_plot_YNEst <- ggplot(cinco_YNEst_all, aes(x=YN,y=YNnums,fill=factor(YNEs,levels=c(6:1)))) + 
  geom_bar(stat='identity') + 
  #scale_fill_brewer(palette ="Set3",direction=-1) +
  scale_fill_manual(values=c("#4575B4","#74ADD1", "#ABD9E9", "#A6D96A","#66BD63","#1A9850"))+
  geom_bar(position=position_stack(reverse=F), stat="identity", colour="black",size=.1) +
  labs(x="¿Ha leído ... en los últimos 12 meses?",y="Frecuencia",
       fill="Estrato",title="Lectura en los últimos 12 meses por estrato")+
  facet_wrap(~YNpregs)
  #theme(panel.background = element_rect(fill = "white"))

YNPorcs <- YNnums/pobs

cinco_YNEst_all <- data.frame(cinco_YNEst_all,YNPorcs)

cinco_plot_YNEstPorc <- ggplot(cinco_YNEst_all, aes(x=YN,y=YNPorcs,fill=factor(YNEs,levels=c(6:1)))) + 
  geom_bar(stat='identity') + 
  #scale_fill_brewer(palette ="Set3",direction=-1) +
  scale_fill_manual(values=c("#74ADD1","#ABD9E9", "#E0F3F8", "#D9EF8B","#A6D96A","#66BD63"))+
  geom_bar(position=position_stack(reverse=F), stat="identity", colour="black",size=.1) +
  labs(x="¿Ha leído ... en los últimos 12 meses?",y="Porcentaje del total de cada estrato",
       fill="Estrato",title="Lectura en los últimos 12 meses por estrato")+
  facet_wrap(~YNpregs)
#theme(panel.background = element_rect(fill = "white"))



#base_re[is.na(base_re)] <- 1010









