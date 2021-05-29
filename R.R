rm(list = ls())

library(wesanderson)
library(hrbrthemes)
library(openxlsx)
library(dplyr)
library(summarytools)
library(ggplot2)


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
            Desinteres=sum(Desinterés==1),Impreso=sum(`Prefiere impreso`==1),Otra=sum(Otra==1))

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

numsIZ_frac <- round(numsIZ/c(sum(base_re$DOMINIO==1),sum(base_re$DOMINIO==2))*100,1)

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

base_NiñezEdu <- base_re %>% 
  filter(RangosEdad!="Menor de 18 años") %>% 
  select(ID,RangosEdad,DOMINIO,P260,P4031S1A1,11:16)

cuat_noms <- c("Nivel educativo","Estrato","Iba a bibliotecas",
               "Iba a librerías","Veía leer",
               "Le leían","Profs motivaban leer",
               "Profs motivaban ir a bibs")

names(base_NiñezEdu)[4:11] <- cuat_noms

cuat_punts <- base_NiñezEdu %>% 
  group_by(ID) %>%
  summarise(RangosEdad = RangosEdad,Educacion = `Nivel educativo`,
            Puntaje = sum(`Iba a bibliotecas`==1)+sum(`Iba a librerías`==1)+sum(`Veía leer`==1)+
              sum(`Le leían`==1)+sum(`Profs motivaban leer`==1)+sum(`Profs motivaban ir a bibs`==1))

cuat_PuntsEduc <- cuat_punts %>% 
  group_by(Puntaje) %>% 
  summarise(Ninguna=sum(Educacion==0),Preescolar=sum(Educacion==1),`Básica primaria`=sum(Educacion==2),
            `Básica secundaria`=sum(Educacion==3),`Media académica`=sum(Educacion==4),
            `Media técnica`=sum(Educacion==5),Normalista=sum(Educacion==6),`Técnica`=sum(Educacion==7),
            `Tecnológica`=sum(Educacion==8),Universitario=sum(Educacion==9),Especialización=sum(Educacion==10),
            Maestría=sum(Educacion==11),Doctorado=sum(Educacion==12))

numsEdu <- c()
for(i in 2:14){
  numsEdu <- c(numsEdu,pull(cuat_PuntsEduc,i)) # Une datos numéricos de agrupa en un solo vector
}

punts <- c(rep(c(0:6),13))
NivsEdu <- c(rep("Ninguna",7),rep("Preescolar",7),rep("Básica primaria",7),
             rep("Básica secundaria",7),rep("Media académica",7),rep("Media técnica",7),
             rep("Normalista",7),rep("Técnica",7),rep("Tecnológica",7),rep("Universitario",7),
             rep("Especialización",7),rep("Maestría",7),rep("Doctorado",7))

cuat_all <- data.frame(punts,NivsEdu,numsEdu)

cuat_plot <- ggplot(cuat_all, aes(fill=factor(punts),
            y=numsEdu, x=factor(NivsEdu,
            levels = c("Ninguna","Preescolar","Básica primaria","Básica secundaria",
             "Media académica","Media técnica","Normalista","Técnica","Tecnológica",
             "Universitario","Especialización","Maestría","Doctorado")))) + 
  geom_bar(position=position_stack(reverse=T), stat="identity", colour="black",size=.1) +
  scale_fill_brewer(palette ="Set3") +
  coord_flip()+
  #scale_fill_manual(values=wes_palette("Royal2"))
  labs(x="Máximo nivel educativo alcanzado",y="Frecuencia",
       fill=stringr::str_wrap("Puntaje de lectura en la infancia",12),
       title="Lectura en infancia y máximo nivel educativo") # Barras apiladas










#colours = c( "#A54657",  "#582630", "#F7EE7F", "#4DAA57","#F1A66A","#F26157", "#F9ECCC", "#679289", "#33658A","#F6AE2D","#86BBD8")

