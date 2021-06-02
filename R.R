rm(list = ls())

library(wesanderson)
library(hrbrthemes)
library(RColorBrewer)
library(openxlsx)
library(dplyr)
library(summarytools)
library(ggplot2)

#setwd("C:/Users/elave/OneDrive/Escritorio/Taller Enlec")

base <- read.xlsx("baseConsolidadaENLEC.xlsx")

# CATEGORIZACIÓN POR RANGOS DE EDAD

base_re <- base %>% 
  mutate(RangosEdad = case_when(P5785 >=0 & P5785 <18 ~ "Menor de 18 años",
                                P5785 >=18 & P5785 <=24 ~ "De 18 a 24 años",
                                P5785 >=25 & P5785 <=34 ~ "De 25 a 34 años",
                                P5785 >=35 & P5785 <=44 ~ "De 35 a 44 años",
                                P5785 >=45 & P5785 <=54 ~ "De 45 a 54 años",
                                P5785 >=55 ~ "Mayor de 55 años"))
base_re[is.na(base_re)] <- 1010

# ---- FRECUENCIA DE VARIABLES DE INTERÉS ----

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

# -------------------- Punto 3 --------------------

# Porcentaje de todos los encuestados que NO lee
sum(base_re$P1864S1!=1010)/sum(!is.na(base_re$ID))*100 # Digital
sum(base_re$P1865S1!=1010)/sum(!is.na(base_re$ID))*100 # Impreso


# ===== Sí/No Lectura en soportes ... por ZONA =====

# No Imp: No leyó ninguno (o baja frecuencia) de: periódicos, revistas, docs de trabajo, libros, articulos académicos
# Sí Imp: Sí leyó por lo menos uno de los de arriba

# No Dig: No leyó ninguno (o baja frecuencia) de: artículos académicos, blogs, emails, páginas web, redes sociales, noticias, libros, docs de trabajo
# Sí Dig: Sí leyó por lo menos uno de los de arriba

base_YNLecZ <- base_re %>% 
  select(DOMINIO,P1864S1,P1865S1) %>% 
  group_by(DOMINIO) %>% 
  summarise(DigSí=sum(P1864S1==1010),DigNo=sum(P1864S1!=1010),ImpSí=sum(P1865S1==1010),ImpNo=sum(P1865S1!=1010))

numsYNLecZ <- c()
for(i in 2:5){
  numsYNLecZ <- c(numsYNLecZ,pull(base_YNLecZ,i)) # Une datos numéricos de agrupa en un solo vector
}

YNLecZ_Zona <- rep(c("Rural","Urbana"),4)
YNLecZ_DI <- c(rep("Digital",4),rep("Impreso",4))
YNLecZ_YN <- rep(c(rep("Sí",2),rep("No",2)),2)
porcsYNLecZ <- numsYNLecZ/c(sum(base_re$DOMINIO==1),sum(base_re$DOMINIO==2))*100

for(i in 0:3){
  t <- porcsYNLecZ[2*i+2]
  porcsYNLecZ[2*i+2] <- porcsYNLecZ[2*i+1]
  porcsYNLecZ[2*i+1] <- t
}

YNLecZ_all <- data.frame(Fill=YNLecZ_Zona,DI=YNLecZ_DI,YN=YNLecZ_YN,nums=numsYNLecZ,porcs=porcsYNLecZ)

library(plyr)
YNLecZ_all <- ddply(YNLecZ_all, .(YN),
                   transform, pos = cumsum(porcs) - (0.5 * porcs))
detach("package:plyr",unload = T)

YNLecZ_all[3:4,6] <-YNLecZ_all[3:4,6]-sum(YNLecZ_all[1:2,5])
YNLecZ_all[7:8,6] <-YNLecZ_all[7:8,6]-sum(YNLecZ_all[5:6,5])


tres_plot_YNLecZ <- ggplot(YNLecZ_all, aes(x=factor(YN),y=porcs,
                                          fill=factor(Fill,levels=c("Urbana","Rural"))))+
  geom_bar(stat='identity') + 
  scale_fill_brewer(palette ="Pastel1") +
  geom_bar(position=position_stack(reverse=F), stat="identity", colour="black",size=.1) +
  geom_text(data=YNLecZ_all,aes(x=YN,y=pos,label=paste0(round(porcs,1),"%")), size=4) +
  labs(x="¿Leyó en soporte ... en los últimos 12 meses?",y="Frecuencia relativa por zona",
       fill="Zona",
       title="Lectura en soporte ... en los últimos 12 meses por zona")+
  facet_wrap(~DI)


# ===== Sí/No Lectura en soportes ... por EDAD =====

# No Imp: No leyó ninguno (o baja frecuencia) de: periódicos, revistas, docs de trabajo, libros, articulos académicos
# Sí Imp: Sí leyó por lo menos uno de los de arriba

# No Dig: No leyó ninguno (o baja frecuencia) de: artículos académicos, blogs, emails, páginas web, redes sociales, noticias, libros, docs de trabajo
# Sí Dig: Sí leyó por lo menos uno de los de arriba

base_YNLecE <- base_re %>% 
  select(RangosEdad,P1864S1,P1865S1) %>% 
  group_by(RangosEdad) %>% 
  summarise(DigSí=sum(P1864S1==1010),DigNo=sum(P1864S1!=1010),ImpSí=sum(P1865S1==1010),ImpNo=sum(P1865S1!=1010))
base_YNLecE <- rbind(base_YNLecE[6,],base_YNLecE[1:5,])

numsYNLecE <- c()
for(i in 2:5){
  numsYNLecE <- c(numsYNLecE,pull(base_YNLecE,i)) # Une datos numéricos de agrupa en un solo vector
}

YNLecE_Edades <- rep(c("Menor de 18 años","De 18 a 24 años","De 25 a 34 años","De 35 a 44 años","De 45 a 54 años",
                     "Mayor de 55 años"),4)
YNLecE_DI <- c(rep("Digital",12),rep("Impreso",12))
YNLecE_YN <- rep(c(rep("Sí",6),rep("No",6)),2)
pobsEdad <- c(sum(base_re$RangosEdad=="Menor de 18 años"),sum(base_re$RangosEdad=="De 18 a 24 años"),
              sum(base_re$RangosEdad=="De 25 a 34 años"),sum(base_re$RangosEdad=="De 35 a 44 años"),
              sum(base_re$RangosEdad=="De 45 a 54 años"),sum(base_re$RangosEdad=="Mayor de 55 años"))
porcsYNLecE <- numsYNLecE/pobsEdad*100

YNLecE_all <- data.frame(Fill=YNLecE_Edades,DI=YNLecE_DI,YN=YNLecE_YN,nums=numsYNLecE,porcs=porcsYNLecE)

library(plyr)
YNLecE_all <- ddply(YNLecE_all, .(YN),
                    transform, pos = cumsum(porcs) - (0.5 * porcs))
detach("package:plyr",unload = T)

YNLecE_all[7:12,6] <-YNLecE_all[7:12,6]-sum(YNLecE_all[1:6,5])
YNLecE_all[19:24,6] <-YNLecE_all[19:24,6]-sum(YNLecE_all[13:18,5])

tres_plot_YNLecE <- ggplot(YNLecE_all, aes(x=factor(YN),y=porcs,
                       fill=factor(Fill,levels=rev(c("Menor de 18 años","De 18 a 24 años",
                      "De 25 a 34 años","De 35 a 44 años","De 45 a 54 años","Mayor de 55 años")))))+
  geom_bar(stat='identity') +
  theme_bw()+
  scale_fill_brewer(palette ="Set3",direction=-1) +
  geom_bar(position=position_stack(reverse=F), stat="identity", colour="black",size=.1) +
  geom_text(data=YNLecE_all,aes(x=YN,y=pos,label=ifelse(porcs>=33,paste0(round(porcs,1),"%"),"")), size=3.5) +
  labs(x="¿Leyó en soporte ... en los últimos 12 meses?",y="Porcentaje relativo por rango de edad",
       fill="Rango de Edad")+
  facet_wrap(~DI)


# ===== ZONA y EDAD =====

cat <- c(rep("Zona",count(seis_YN_Z_all)),rep("Rango de edad",count(seis_YN_E_all)))

YNLec_all <- data.frame(cat,rbind(YNLecZ_all,YNLecE_all))

ggplot(YNLec_all, aes(x=YN,y=porcs,fill=factor(Fill,levels=c("Rango de edad",
                                                               rev(c("Menor de 18 años","De 18 a 24 años","De 25 a 34 años","De 35 a 44 años",
                                                                     "De 45 a 54 años","Mayor de 55 años"))," ","Zona",c("Urbana","Rural"),
                                                               "  ","   ","    ","     ")),label=ifelse(porcs>=44.5,paste0(round(porcs,1),"%"),""))) +
  #geom_text(data=seis_YN_all,size = 3, position = position_stack(vjust = 0.5),label=ifelse(porcs>=33,paste0(round(porcs,1),"%"),""))+
  geom_bar(position=position_stack(reverse=F), stat="identity", colour="black",size=.1) +
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  scale_fill_manual(values=c(w,rev(c("#8DD3C7","#FFFFB3","#BEBADA","#FB8072","#80B1D3","#FDB462")),w,w,"#FBB4AE","#B3CDE3",w,w,w,w,w),
                    drop=F) +
  guides(fill=guide_legend(ncol=1)) +
  theme(legend.position="right", 
        legend.key = element_rect(fill=NA),
        legend.title=element_blank())+
  theme_bw()+
  labs(x="¿Leyó en soporte ... en los últimos 12 meses?",y="Porcentaje del total de cada categoría",fill="")+
  facet_grid(cat~DI,scales="free")

# ===== DIGITAL =====

# ##### Por EDAD #####

base_RazNoLecDig <- base_re %>%
  filter(!is.na(P1864S1)) %>% 
  select(RangosEdad,DOMINIO,P260,P4031S1A1,30,34:41,31:33) # Filtra personas que respondieron P1864

razonesD <- c("Otras preferencias","Dinero","Salud","Pereza","No tiene dispositivos",
              "Duda qué leer","Acceso a internet","No sabe usar dispositivos","Tiempo",
              "Desinterés","Prefiere impreso","Otra")

names(base_RazNoLecDig)[5:16] <- razonesD # Cambia nombres de las razones

agrupaD <- base_RazNoLecDig %>% # Cuenta frecuencia de los rangos de edad en las razones
  group_by(RangosEdad) %>%
  summarise(OtrasPrefs = sum(`Otras preferencias`==1), Dinero = sum(Dinero==1),
            Salud = sum(Salud ==1), Pereza = sum(Pereza==1), Falta_dis = sum(`No tiene dispositivos`==1),
            Duda = sum(`Duda qué leer`==1), Internet = sum(`Acceso a internet`==1),
            Uso = sum(`No sabe usar dispositivos`==1),Tiempo = sum(Tiempo==1),
            Desinteres=sum(Desinterés==1),Impreso=sum(`Prefiere impreso`==1),Otra=sum(Otra==1)) # colSums()?
agrupaD <- rbind(agrupaD[6,],agrupaD[1:5,])

razonesD_ <- c(rep("Otras preferencias",6),rep("Dinero",6),rep("Salud",6),rep("Pereza",6),
               rep("No tiene dispositivos",6),rep("Duda qué leer",6),rep("Acceso a internet",6),
               rep("No sabe usar dispositivos",6),rep("Tiempo",6),rep("Desinterés",6),
               rep("Prefiere impreso",6),rep("Otra",6)) # Necesario para el diagrama de barras apiladas posterior

numsD <- c()
for(i in 2:13){
  numsD <- c(numsD,pull(agrupaD,i)) # Une datos numéricos de agrupa en un solo vector
}

porcsD <- numsD/pobsEdad*100

tres_Dig <- data.frame(razonesD_,agrupaD[1],numsD,porcsD) # Dataframe con los datos necesarios para barras apiladas

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

tres_plot_DigPorcs <- ggplot(tres_Dig, aes(fill=factor(RangosEdad,
                                                  levels = c("Menor de 18 años","De 18 a 24 años",
                                                             "De 25 a 34 años","De 35 a 44 años",
                                                             "De 45 a 54 años","Mayor de 55 años")),
                                      y=porcsD, x=reorder(razonesD_,porcsD))) + 
  geom_bar(position=position_stack(reverse=F), stat="identity", colour="black",size=.25) +
  coord_flip()+
  scale_fill_brewer(palette ="Set3") +
  #scale_fill_manual(values=wes_palette("Royal2"))
  labs(x="Razón por la que no lee digital",y="Porcentaje relativo por rango de edad",fill="Rango de Edad",
       title="Razones Digital")


# Tendencia con la edad, más viejo/joven -> menos digital

tres_Dig_f_Edad_ <- base_RazNoLecDig %>% 
  freq(RangosEdad,report.nas=FALSE)

# ##### Por ZONA #####

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



# ===== IMPRESO =====


# ##### Por EDAD #####

base_RazNoLecImp <- base_re %>%
  filter(!is.na(P1865S1)) %>% 
  select(RangosEdad,DOMINIO,P260,P4031S1A1,42,44:51,43)

razonesI <- c("Otras preferencias","Tiempo","Desinterés","Pereza","Dinero","Duda qué leer",
              "Salud","Acceso a material","Prefiere digital","Otra")

names(base_RazNoLecImp)[5:14] <- razonesI # Cambia nombres de las razones

agrupaI <- base_RazNoLecImp %>% # Cuenta frecuencia de los rangos de edad en las razones
  group_by(RangosEdad) %>%
  summarise(OtrasPrefs = sum(`Otras preferencias`==1),Tiempo = sum(Tiempo==1),
            Desinteres=sum(Desinterés==1),Pereza=sum(Pereza==1),Dinero = sum(Dinero==1),
            Duda=sum(`Duda qué leer`==1),Salud=sum(Salud==1),Acceso=sum(`Acceso a material`==1),
            Digital=sum(`Prefiere digital`==1),Otra=sum(Otra==1))
agrupaI <- rbind(agrupaI[6,],agrupaI[1:5,])

razonesI_ <- c(rep("Otras preferencias",6),rep("Tiempo",6),rep("Desinterés",6),rep("Pereza",6),
               rep("Dinero",6),rep("Duda qué leer",6),rep("Salud",6),
               rep("Acceso a material",6),rep("Prefiere digital",6),rep("Otra",6)) # Necesario para el diagrama de barras apiladas posterior

numsI <- c()
for(i in 2:11){
  numsI <- c(numsI,pull(agrupaI,i)) # Une datos numéricos de agrupa en un solo vector
}

porcsI <- numsI/pobsEdad*100

tres_Imp <- data.frame(razonesI_,agrupaI[1],numsI,porcsI) # Dataframe con los datos necesarios para barras apiladas

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

tres_plot_ImpPorcs <- ggplot(tres_Imp, aes(fill=factor(RangosEdad,
                                                  levels = c("Menor de 18 años","De 18 a 24 años",
                                                             "De 25 a 34 años","De 35 a 44 años",
                                                             "De 45 a 54 años","Mayor de 55 años")),
                                      y=porcsI, x=reorder(razonesI_,porcsI))) + 
  geom_bar(position=position_stack(reverse=F), stat="identity", colour="black",size=.25) +
  coord_flip()+
  scale_fill_brewer(palette ="Set3") +
  #scale_fill_manual(values=wes_palette("Royal2"))
  labs(x="Razón por la que no lee impreso",y="Porcentaje relativo por rango de edad",fill="Rango de Edad",
       title="Razones Impreso") # Barras apiladas

# Frecuencia edad

tres_Imp_f_Edad_ <- base_RazNoLecImp %>% 
  freq(RangosEdad,report.nas=FALSE)

view(tres_Imp_f_Edad_)  


# ##### Por ZONA #####

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


# -------------------- Punto 4 --------------------

base_NiñezEdu <- base_re %>% # Selecciona variables necesarias, excluye <18 por su educación
  filter(RangosEdad!="Menor de 18 años") %>% 
  select(ID,RangosEdad,DOMINIO,P260,P4031S1A1,11:16)

cuat_noms <- c("Educación","Estrato","Iba a bibliotecas",
               "Iba a librerías","Veía leer",
               "Le leían","Profs motivaban leer",
               "Profs motivaban ir a bibs")

names(base_NiñezEdu)[4:11] <- cuat_noms # Cambia nombres de columnas

# ===== Análisis de una pregunta: ¿Veía a familiares leer? =====

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


# ===== Por PUNTAJES =====
# Se crea un puntaje con todas las preguntas, en vez de analizar cada una por aparte. 
# Puntaje resume el entorno en la infancia.

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

cuat_plot <- ggplot(cuat_all, aes(fill=factor(punts,levels=6:0),
            y=numsEdu, x=factor(NivsEdu,
            levels = c("Ninguna","Preescolar","Básica primaria","Básica secundaria",
             "Media académica","Media técnica","Normalista","Técnica","Tecnológica",
             "Universitario","Especialización","Maestría","Doctorado")))) + 
  geom_bar(position=position_stack(reverse=F), stat="identity", colour="black",size=.1) +
  scale_fill_brewer(palette ="RdYlGn",direction=-1) +
  coord_flip()+
  #scale_fill_manual(values=wes_palette("Royal2"))
  labs(x="Máximo nivel educativo alcanzado",y="Frecuencia",
       fill=stringr::str_wrap("Puntaje de lectura en la infancia",12),
       title="Lectura en infancia y máximo nivel educativo") # Barras apiladas

# ===== Por ZONA =====
#Máximo nivel educativo por zona.

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



# ---------- Punto 5 ----------

base_TiLecEst <- base_re %>% 
  filter(P4031S1A1!=1010 & P4031S1A1!=0 & P4031S1A1!=9) %>% #1044 excluidos por estrato 0 o 9
  select(P5785,P4031S1A1,P1855S2A1,P1860S2A1,P1862S2A1,P1852S2A1,P1851S2A1)

colnames(base_TiLecEst) <- c("Rango de Edad","Estrato","Redes sociales","Periódicos impresos",
                             "Documentos académicos impresos","Documentos académicos digitales","Páginas web")


# ===== 1 Pregunta, REDES SOCIALES, TIEMPO/ESTRATO =====

cinco_P1 <- base_TiLecEst %>% 
  group_by(Estrato) %>% 
  summarise(`Menos de una hora`=sum(`Redes sociales`==1),`1 hora`=sum(`Redes sociales`==2),
            `2 horas`=sum(`Redes sociales`==3),`3 horas`=sum(`Redes sociales`==4),
            `4 horas`=sum(`Redes sociales`==5),`5 horas`=sum(`Redes sociales`==6),
            `6 a 7 horas`=sum(`Redes sociales`==7),`8 a 10 horas`=sum(`Redes sociales`==8),
            `Más de 10 horas`=sum(`Redes sociales`==9),`No sabe / No informa`=sum(`Redes sociales`==99))

numsTiLecEst1 <- c()
for(i in 2:11){
  numsTiLecEst1 <- c(numsTiLecEst1,pull(cinco_P1,i)) # Une datos numéricos de nivs de educ en un solo vector
}

p <- c()
for(i in 1:6){
  p = c(p,sum(base_re$P4031S1A1==i))
}

pobsEstrato1 <- rep(p,10)

porcsTiLecEst1 <- numsTiLecEst1/pobsEstrato1*100

Tiempo <- c(rep("Menos de una hora",6),rep("1 hora",6),rep("2 horas",6),rep("3 horas",6),
            rep("4 horas",6),rep("5 horas",6),rep("6 a 7 horas",6),rep("8 a 10 horas",6),
            rep("Más de 10 horas",6),rep("No sabe / No informa",6))
Estratos<-rep(1:6,10)

cinco_P1_all <- data.frame(Estratos,Tiempo,numsTiLecEst1,porcsTiLecEst1)


cinco_plot_P1 <- ggplot(cinco_P1_all, aes(fill=factor(Tiempo,
              levels = c("No sabe / No informa","Más de 10 horas","8 a 10 horas","6 a 7 horas","5 horas",
                         "4 horas","3 horas","2 horas","1 hora","Menos de una hora")),
  y=numsTiLecEst1, x=factor(Estratos))) + 
  geom_bar(position=position_stack(reverse=F), stat="identity", colour="black",size=.1) +
  scale_fill_brewer(palette ="RdYlGn") +
  labs(x="Estrato",y="Frecuencia",
       fill=stringr::str_wrap("Tiempo semanal de lectura de redes sociales",21),
       title="Lectura de redes sociales y estrato") # Frecuencia


cinco_plot_P1_porcs <- ggplot(cinco_P1_all, aes(fill=factor(Tiempo,
  levels = c("No sabe / No informa","Más de 10 horas","8 a 10 horas","6 a 7 horas","5 horas",
             "4 horas","3 horas","2 horas","1 hora","Menos de una hora")),
                  y=porcsTiLecEst1, x=factor(Estratos))) + 
  geom_bar(position=position_stack(reverse=F), stat="identity", colour="black",size=.1) +
  scale_fill_brewer(palette ="RdYlGn") +
  labs(x="Estrato",y="Porcentaje relativo por estrato",
       fill=stringr::str_wrap("Tiempo semanal de lectura de redes sociales",21),
       title="Lectura de redes sociales y estrato") # Frecuencia relativa

# ===== 5 preguntas, TIEMPO/ESTRATO =====

cinco_P2 <- base_TiLecEst %>%
  group_by(Estrato) %>% 
  summarise(`Menos de una hora`=sum(`Periódicos impresos`==1),`1 hora`=sum(`Periódicos impresos`==2),
            `2 horas`=sum(`Periódicos impresos`==3),`3 horas`=sum(`Periódicos impresos`==4),
            `4 horas`=sum(`Periódicos impresos`==5),`5 horas`=sum(`Periódicos impresos`==6),
            `6 a 7 horas`=sum(`Periódicos impresos`==7),`8 a 10 horas`=sum(`Periódicos impresos`==8),
            `Más de 10 horas`=sum(`Periódicos impresos`==9),`No sabe / No informa`=sum(`Periódicos impresos`==99))

cinco_P3 <- base_TiLecEst %>% 
  group_by(Estrato) %>% 
  summarise(`Menos de una hora`=sum(`Documentos académicos impresos`==1),`1 hora`=sum(`Documentos académicos impresos`==2),
            `2 horas`=sum(`Documentos académicos impresos`==3),`3 horas`=sum(`Documentos académicos impresos`==4),
            `4 horas`=sum(`Documentos académicos impresos`==5),`5 horas`=sum(`Documentos académicos impresos`==6),
            `6 a 7 horas`=sum(`Documentos académicos impresos`==7),`8 a 10 horas`=sum(`Documentos académicos impresos`==8),
            `Más de 10 horas`=sum(`Documentos académicos impresos`==9),`No sabe / No informa`=sum(`Documentos académicos impresos`==99))

cinco_P4 <- base_TiLecEst %>% 
  group_by(Estrato) %>% 
  summarise(`Menos de una hora`=sum(`Documentos académicos digitales`==1),`1 hora`=sum(`Documentos académicos digitales`==2),
            `2 horas`=sum(`Documentos académicos digitales`==3),`3 horas`=sum(`Documentos académicos digitales`==4),
            `4 horas`=sum(`Documentos académicos digitales`==5),`5 horas`=sum(`Documentos académicos digitales`==6),
            `6 a 7 horas`=sum(`Documentos académicos digitales`==7),`8 a 10 horas`=sum(`Documentos académicos digitales`==8),
            `Más de 10 horas`=sum(`Documentos académicos digitales`==9),`No sabe / No informa`=sum(`Documentos académicos digitales`==99))

cinco_P5 <- base_TiLecEst %>% 
  group_by(Estrato) %>% 
  summarise(`Menos de una hora`=sum(`Páginas web`==1),`1 hora`=sum(`Páginas web`==2),
            `2 horas`=sum(`Páginas web`==3),`3 horas`=sum(`Páginas web`==4),
            `4 horas`=sum(`Páginas web`==5),`5 horas`=sum(`Páginas web`==6),
            `6 a 7 horas`=sum(`Páginas web`==7),`8 a 10 horas`=sum(`Páginas web`==8),
            `Más de 10 horas`=sum(`Páginas web`==9),`No sabe / No informa`=sum(`Páginas web`==99))

cinco_PsDF <- data.frame(cinco_P1[2:11],cinco_P2[2:11],cinco_P3[2:11],cinco_P4[2:11],cinco_P5[2:11])

numsTiLecEstPs <-c()
for(i in 1:50){
  numsTiLecEstPs <- c(numsTiLecEstPs,pull(cinco_PsDF,i)) 
}

pobsEstrato <- rep(p,50)

porcsTiLecEstPs <- numsTiLecEstPs/pobsEstrato*100

horas <- rep(c(rep("Menos de una hora",6),rep("1 hora",6),rep("2 horas",6),rep("3 horas",6),
           rep("4 horas",6),rep("5 horas",6),rep("6 a 7 horas",6),rep("8 a 10 horas",6),
           rep("Más de 10 horas",6),rep("No sabe / No informa",6)),5)

pregs <- c(rep("Redes sociales",60),rep("Periódicos impresos",60),
           rep("Documentos académicos impresos",60),rep("Documentos académicos digitales",60),
           rep("Páginas web",60))

ests <- rep(1:6,50)

cinco_Ps_all <- data.frame(horas,pregs,ests,numsTiLecEstPs,porcsTiLecEstPs)

cinco_plot_Ps <- ggplot(cinco_Ps_all, aes(x=factor(ests,levels=1:6),y=porcsTiLecEstPs,
              fill=factor(horas,levels = c("No sabe / No informa",
             "Más de 10 horas","8 a 10 horas","6 a 7 horas","5 horas",
            "4 horas","3 horas","2 horas","1 hora","Menos de una hora")))) + 
  geom_bar(stat='identity') +
  scale_fill_brewer(palette ="RdYlGn") +
  geom_bar(position=position_stack(reverse=F), stat="identity", colour="black",size=.1) +
  labs(x="Estrato",y="Porcentaje relativo por estrato",
       fill=stringr::str_wrap("Tiempo semanal de lectura de redes sociales",21),
       title="Tiempo de lectura de ... en los últimos 12 meses por estrato")+
  facet_wrap(~pregs,scales="free_y") #Le da una escala diferente a cada miniplot, se ve más clara la distribución en algunos, pero queda extraño/misleading~


# ===== 5 Preguntas, SíNo/ESTRATO =====

cinco_YNEst <- base_TiLecEst %>%
  group_by(Estrato) %>% 
  summarise(P1S=sum(`Redes sociales`!=1010),P1N=sum(`Redes sociales`==1010),P2S=sum(`Periódicos impresos`!=1010),
            P2N=sum(`Periódicos impresos`==1010),P3S=sum(`Documentos académicos impresos`!=1010),
            P3N=sum(`Documentos académicos impresos`==1010),P4S=sum(`Documentos académicos digitales`!=1010),
            P4N=sum(`Documentos académicos digitales`==1010),P5S=sum(`Páginas web`!=1010),P5N=sum(`Páginas web`==1010))

YNnums <- c()
for(i in 2:11){
  YNnums <- c(YNnums,pull(cinco_YNEst,i)) # Une datos numéricos de nivs de educ en un solo vector
}

YNEs <- rep(1:6,10)
YNpregs <- c(rep("Redes sociales",12),rep("Periódicos impresos",12),
             rep("Documentos académicos impresos",12),rep("Documentos académicos digitales",12),
             rep("Páginas web",12))
YN <- rep(c(rep("Sí",6),rep("No",6)),5)

YNPorcs <- YNnums/pobsEstrato[1:60]*100

cinco_YNEst_all <- data.frame(YNEs,YNpregs,YN,YNnums,YNPorcs)

cinco_plot_YNEst <- ggplot(cinco_YNEst_all, aes(x=YN,y=YNnums,fill=factor(YNEs,levels=c(6:1)))) + 
  geom_bar(stat='identity') +
  scale_fill_manual(values=c("#4575B4","#74ADD1", "#ABD9E9", "#A6D96A","#66BD63","#1A9850"))+
  geom_bar(position=position_stack(reverse=F), stat="identity", colour="black",size=.1) +
  labs(x="¿Ha leído ... en los últimos 12 meses?",y="Frecuencia",
       fill="Estrato",title="Lectura en los últimos 12 meses por estrato")+
  facet_wrap(~YNpregs)

cinco_plot_YNEstPorc <- ggplot(cinco_YNEst_all, aes(x=YN,y=YNPorcs,fill=factor(YNEs,levels=c(6:1)))) + 
  geom_bar(stat='identity') + 
  #scale_fill_brewer(palette ="Set3",direction=-1) +
  scale_fill_manual(values=c("#74ADD1","#ABD9E9", "#E0F3F8", "#D9EF8B","#A6D96A","#66BD63"))+
  geom_bar(position=position_stack(reverse=F), stat="identity", colour="black",size=.1) +
  labs(x="¿Ha leído ... en los últimos 12 meses?",y="Porcentaje del total de cada estrato",
       fill="Estrato",title="Lectura en los últimos 12 meses por estrato")+
  facet_wrap(~YNpregs)














# -------------------- Punto 6 --------------------


base_LibsLe <- base_re %>% 
  filter(P4031S1A1!=1010&P4031S1A1!=0&P4031S1A1!=9) %>% # ~1% de personas excluidas por estrato no especificado o ambiguo(pirata,no se puede establecer)
  select(RangosEdad,DOMINIO,P4031S1A1,P5381,P210,P211,P1863S1,P1863S2)

colnames(base_LibsLe) <- c("Rango_de_edad","Zona","Estrato","N_libros","N_Imp","N_Dig","N_Gusto","N_Trabajo")

base_LibsLe[base_LibsLe==1010] <- 0

sum(base_LibsLe$N_libros!=0)/count(base_LibsLe)[1,1] # 55% afirma haber leído al menos 1 libro


mean(base_LibsLe$N_libros)
median(base_LibsLe$N_libros)
mean(base_LibsLe$N_Imp)
mean(base_LibsLe$N_Dig)
mean(base_LibsLe$N_Gusto)
mean(base_LibsLe$N_Trabajo)

quantile(base_LibsLe$N_Imp,.99)

# Solo datos de personas que afirman haber leído por lo menos un libro:

base_LibsLeSí <-base_LibsLe %>% 
  filter(N_libros!=0)

mean(base_LibsLeSí$N_libros)
median(base_LibsLeSí$N_libros)
mean(base_LibsLeSí$N_Imp)
median(base_LibsLeSí$N_Imp)
mean(base_LibsLeSí$N_Dig)
mean(base_LibsLeSí$N_Gusto)
mean(base_LibsLeSí$N_Trabajo)



sum(base_LibsLeSí$N_Dig)/sum(base_LibsLeSí$N_libros)
sum(base_LibsLeSí$N_Imp)/sum(base_LibsLeSí$N_libros)

sum(base_LibsLeSí$N_Trabajo)/sum(base_LibsLeSí$N_libros)
sum(base_LibsLeSí$N_Gusto)/sum(base_LibsLeSí$N_libros)

# Bayes?




# ===== Sí/No =====

seis_YN <- base_LibsLe %>% 
  select(N_Imp,N_Dig) %>% 
  summarise(Impreso=c(sum(N_Imp!=0),sum(N_Imp==0)),Digital=c(sum(N_Dig!=0),sum(N_Dig==0)))
numsYN_DI <-c(seis_YN[,1],seis_YN[,2])
seis_YN <- data.frame(ID=c(rep("Impreso",2),rep("Digital",2)),YN=rep(c("Sí","No"),2),numsYN_DI,porcs=numsYN_DI/count(base_LibsLe)[1,1]*100)

seis_plot_YN_ID <- ggplot(seis_YN, aes(x=YN,y=porcs,label=paste0(round(porcs,1),"%")))+
  theme_bw()+
  geom_bar( stat="identity", colour="black",size=.3,fill=c("#B3CDE3","#FBB4AE","#B3CDE3","#FBB4AE")) +
  geom_text(size=4,position=position_stack(vjust=0.5))+
  labs(x="¿Ha leído libros en formato ... en los últimos 12 meses?",y="Porcentaje del total")+
  facet_wrap(~ID)


# ===== Sí/No, Zona, Edad, Estrato =====

seis_YN_Zona <- base_LibsLe %>%
  group_by(Zona) %>% 
  summarise(DigSí=sum(N_Dig!=0),DigNo=sum(N_Dig==0),ImpSí=sum(N_Imp!=0),ImpNo=sum(N_Imp==0))

YNZnums <- c()
for(i in 2:5){
  YNZnums <- c(YNZnums,pull(seis_YN_Zona,i)) # Une datos numéricos de nivs de educ en un solo vector
}

YNZporcs <- YNZnums/c(sum(base_re$DOMINIO==1),sum(base_re$DOMINIO==2))*100

YNZ_DI <- c(rep("Digital",4),rep("Impreso",4))
YNZ_Z <- rep(c("Urbana","Rural"),4)
YNZ_YN <- rep(c(rep("Sí",2),rep("No",2)),2)


seis_YN_Z_all <- data.frame(Fill=YNZ_Z,DI=YNZ_DI,YN=YNZ_YN,nums=YNZnums,porcs=YNZporcs)



seis_YN_Edad <- base_LibsLe %>% 
  group_by(Rango_de_edad) %>% 
  summarise(DigSí=sum(N_Dig!=0),DigNo=sum(N_Dig==0),ImpSí=sum(N_Imp!=0),ImpNo=sum(N_Imp==0))
seis_YN_Edad <- rbind(seis_YN_Edad[6,],seis_YN_Edad[1:5,])

YNEnums <- c()
for(i in 2:5){
  YNEnums <- c(YNEnums,pull(seis_YN_Edad,i)) # Une datos numéricos de nivs de educ en un solo vector
}

pobsEdad <- c(sum(base_re$RangosEdad=="Menor de 18 años"),sum(base_re$RangosEdad=="De 18 a 24 años"),
              sum(base_re$RangosEdad=="De 25 a 34 años"),sum(base_re$RangosEdad=="De 35 a 44 años"),
              sum(base_re$RangosEdad=="De 45 a 54 años"),sum(base_re$RangosEdad=="Mayor de 55 años"))
YNEporcs <- YNEnums/pobsEdad*100

YNE_DI <- c(rep("Digital",12),rep("Impreso",12))
YNE_E <- rep(c("Menor de 18 años","De 18 a 24 años","De 25 a 34 años","De 35 a 44 años",
                  "De 45 a 54 años","Mayor de 55 años"),4)
YNE_YN <- rep(c(rep("Sí",6),rep("No",6)),2)

seis_YN_E_all <- data.frame(Fill=YNE_E,DI=YNE_DI,YN=YNE_YN,nums=YNEnums,porcs=YNEporcs)



seis_YN_Estrato <- base_LibsLe %>% 
  group_by(Estrato) %>% 
  summarise(DigSí=sum(N_Dig!=0),DigNo=sum(N_Dig==0),ImpSí=sum(N_Imp!=0),ImpNo=sum(N_Imp==0))


YNEsnums <- c()
for(i in 2:5){
  YNEsnums <- c(YNEsnums,pull(seis_YN_Estrato,i)) # Une datos numéricos de nivs de educ en un solo vector
}

p <- c()
for(i in 1:6){
  p = c(p,sum(base_re$P4031S1A1==i))
}

pobsEstrato <- rep(p,4)

YNEsporcs <- YNEsnums/pobsEstrato*100

YNEs_DI <- c(rep("Digital",12),rep("Impreso",12))
YNEs_Es <- rep(1:6,4)
YNEs_YN <- rep(c(rep("Sí",6),rep("No",6)),2)

seis_YN_Es_all <- data.frame(Fill=YNEs_Es,DI=YNEs_DI,YN=YNEs_YN,nums=YNEsnums,porcs=YNEsporcs)



cat <- c(rep("Zona",count(seis_YN_Z_all)),rep("Rango de edad",count(seis_YN_E_all)),rep("Estrato",count(seis_YN_Es_all)))

seis_YN_all <- data.frame(cat,rbind(seis_YN_Z_all,seis_YN_E_all,seis_YN_Es_all))

w <-"white"
#seis_plot_YN <-

ggplot(seis_YN_all, aes(x=YN,y=porcs,fill=factor(Fill,levels=c("Estrato",c(6:1),"","Rango de edad",
                   rev(c("Menor de 18 años","De 18 a 24 años","De 25 a 34 años","De 35 a 44 años",
                         "De 45 a 54 años","Mayor de 55 años"))," ","Zona",c("Urbana","Rural"),
                   "  ","   ","    ","     ")),label=ifelse(porcs>=44.5,paste0(round(porcs,1),"%"),""))) +
  #geom_text(data=seis_YN_all,size = 3, position = position_stack(vjust = 0.5),label=ifelse(porcs>=33,paste0(round(porcs,1),"%"),""))+
  geom_bar(position=position_stack(reverse=F), stat="identity", colour="black",size=.1) +
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  scale_fill_manual(values=c(w,"#74ADD1","#ABD9E9", "#E0F3F8", "#D9EF8B","#A6D96A","#66BD63",w,w,rev(c("#8DD3C7","#FFFFB3","#BEBADA","#FB8072","#80B1D3","#FDB462")),w,w,"#FBB4AE","#B3CDE3",w,w,w,w,w),
                    drop=F) +
  guides(fill=guide_legend(ncol=1)) +
  theme(legend.position="right", 
        legend.key = element_rect(fill=NA),
        legend.title=element_blank())+
  theme_bw()+
  labs(x="¿Ha leído libros en formato ... en los últimos 12 meses?",y="Porcentaje del total de cada categoría",
       fill="")+
  facet_grid(cat~DI,scales="free")
  


# ===== Sí/No, Zona, Edad, Estrato, solo gente que ha leído =====



seis_YN_Zona <- base_LibsLe %>% 
  filter(N_libros!=0) %>% 
  group_by(Zona) %>% 
  summarise(DigSí=sum(N_Dig!=0),DigNo=sum(N_Dig==0),ImpSí=sum(N_Imp!=0),ImpNo=sum(N_Imp==0))

YNZnums <- c()
for(i in 2:5){
  YNZnums <- c(YNZnums,pull(seis_YN_Zona,i)) # Une datos numéricos de nivs de educ en un solo vector
}

YNZporcs <- YNZnums/c(sum(base_LibsLeSí$Zona==1),sum(base_LibsLeSí$Zona==2))*100

YNZ_DI <- c(rep("Digital",4),rep("Impreso",4))
YNZ_Z <- rep(c("Urbana","Rural"),4)
YNZ_YN <- rep(c(rep("Sí",2),rep("No",2)),2)


seis_YN_Z_all <- data.frame(Fill=YNZ_Z,DI=YNZ_DI,YN=YNZ_YN,nums=YNZnums,porcs=YNZporcs)



seis_YN_Edad <- base_LibsLe %>% 
  filter(N_libros!=0) %>% 
  group_by(Rango_de_edad) %>% 
  summarise(DigSí=sum(N_Dig!=0),DigNo=sum(N_Dig==0),ImpSí=sum(N_Imp!=0),ImpNo=sum(N_Imp==0))
seis_YN_Edad <- rbind(seis_YN_Edad[6,],seis_YN_Edad[1:5,])

YNEnums <- c()
for(i in 2:5){
  YNEnums <- c(YNEnums,pull(seis_YN_Edad,i)) # Une datos numéricos de nivs de educ en un solo vector
}

pobsEdad <- c(sum(base_LibsLeSí$Rango_de_edad =="Menor de 18 años"),sum(base_LibsLeSí$Rango_de_edad=="De 18 a 24 años"),
              sum(base_LibsLeSí$Rango_de_edad=="De 25 a 34 años"),sum(base_LibsLeSí$Rango_de_edad=="De 35 a 44 años"),
              sum(base_LibsLeSí$Rango_de_edad=="De 45 a 54 años"),sum(base_LibsLeSí$Rango_de_edad=="Mayor de 55 años"))
YNEporcs <- YNEnums/pobsEdad*100

YNE_DI <- c(rep("Digital",12),rep("Impreso",12))
YNE_E <- rep(c("Menor de 18 años","De 18 a 24 años","De 25 a 34 años","De 35 a 44 años",
               "De 45 a 54 años","Mayor de 55 años"),4)
YNE_YN <- rep(c(rep("Sí",6),rep("No",6)),2)

seis_YN_E_all <- data.frame(Fill=YNE_E,DI=YNE_DI,YN=YNE_YN,nums=YNEnums,porcs=YNEporcs)



seis_YN_Estrato <- base_LibsLe %>% 
  filter(N_libros!=0) %>% 
  group_by(Estrato) %>% 
  summarise(DigSí=sum(N_Dig!=0),DigNo=sum(N_Dig==0),ImpSí=sum(N_Imp!=0),ImpNo=sum(N_Imp==0))


YNEsnums <- c()
for(i in 2:5){
  YNEsnums <- c(YNEsnums,pull(seis_YN_Estrato,i)) # Une datos numéricos de nivs de educ en un solo vector
}

p <- c()
for(i in 1:6){
  p = c(p,sum(base_LibsLeSí$Estrato==i))
}

pobsEstrato <- rep(p,4)

YNEsporcs <- YNEsnums/pobsEstrato*100

YNEs_DI <- c(rep("Digital",12),rep("Impreso",12))
YNEs_Es <- rep(1:6,4)
YNEs_YN <- rep(c(rep("Sí",6),rep("No",6)),2)

seis_YN_Es_all <- data.frame(Fill=YNEs_Es,DI=YNEs_DI,YN=YNEs_YN,nums=YNEsnums,porcs=YNEsporcs)



cat <- c(rep("Zona",count(seis_YN_Z_all)),rep("Rango de edad",count(seis_YN_E_all)),rep("Estrato",count(seis_YN_Es_all)))

seis_YN_all <- data.frame(cat,rbind(seis_YN_Z_all,seis_YN_E_all,seis_YN_Es_all))

w <-"white"
#seis_plot_YN <-

ggplot(seis_YN_all, aes(x=YN,y=porcs,fill=factor(Fill,levels=c("Estrato",c(6:1),"","Rango de edad",
                                                               rev(c("Menor de 18 años","De 18 a 24 años","De 25 a 34 años","De 35 a 44 años",
                                                                     "De 45 a 54 años","Mayor de 55 años"))," ","Zona",c("Urbana","Rural"),
                                                               "  ","   ","    ","     ")),label=ifelse(porcs>=44.5,paste0(round(porcs,1),"%"),""))) +
  #geom_text(data=seis_YN_all,size = 3, position = position_stack(vjust = 0.5),label=ifelse(porcs>=33,paste0(round(porcs,1),"%"),""))+
  geom_bar(position=position_stack(reverse=F), stat="identity", colour="black",size=.1) +
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  scale_fill_manual(values=c(w,"#74ADD1","#ABD9E9", "#E0F3F8", "#D9EF8B","#A6D96A","#66BD63",w,w,rev(c("#8DD3C7","#FFFFB3","#BEBADA","#FB8072","#80B1D3","#FDB462")),w,w,"#FBB4AE","#B3CDE3",w,w,w,w,w),
                    drop=F) +
  guides(fill=guide_legend(ncol=1)) +
  theme(legend.position="right", 
        legend.key = element_rect(fill=NA),
        legend.title=element_blank())+
  theme_bw()+
  labs(x="¿Ha leído libros en formato ... en los últimos 12 meses?",y="Porcentaje del total de cada categoría",
       fill="")+
  facet_grid(cat~DI,scales="free")



# ===== Imp vs Dig =====

seis_IvD <- base_LibsLe %>% 
  select(N_Imp,N_Dig)

DI <- c(rep("Impreso",count(seis_IvD)[1,1]),rep("Digital",count(seis_IvD)[1,1]))
nums <- c()
for(i in 1:2){
  nums <- c(nums,pull(seis_IvD,i)) # Une datos numéricos de nivs de educ en un solo vector
}
seis_IvD <- data.frame(DI,nums)

ggplot(seis_IvD, aes(x=DI,y=nums,fill=DI)) +
  geom_violin(width=3) +
  geom_boxplot(width=.5, color="gray") +
  scale_fill_viridis(discrete = TRUE) +
  theme(plot.title = element_text(size=11)) +
  theme_bw()+
  ggtitle("A Violin wrapping a boxplot") +
  coord_cartesian(ylim = c(-1, 12))+
  facet_wrap(~DI,scales = "free")

ggplot(seis_IvD, aes(x=DI, y=nums, fill=DI)) + 
  geom_boxplot(alpha=0.3) +
  scale_y_discrete(limits=factor(0:10))+
  theme(legend.position="none") +
  scale_fill_brewer(palette="BuPu")



ggplot(seis_IvD, aes(x=DI,y=nums)) +
  geom_density(stat="density")
  scale_fill_viridis(discrete = TRUE) +
  theme(plot.title = element_text(size=11)) +
  ggtitle("A Violin wrapping a boxplot") +
  xlab("")










  
  
# -------------------- Punto 7 --------------------
  
base_LecFrec <- base_re %>% 
  filter(P5381!=1010,P4031S1A1!=1010&P4031S1A1!=0&P4031S1A1!=9) %>% 
  select(Digital=P1858S1,Impreso=P5380S1)
  
LecFrecD <- base_LecFrec %>% 
  group_by(Digital) %>% 
  summarise(ND=sum(Digital==Digital))
LecFrecI <- base_LecFrec %>% 
  group_by(Impreso) %>% 
  summarise(NI=sum(Impreso==Impreso))

numsD <- pull(LecFrecD,2)[1:6]
numsI <- pull(LecFrecI,2)[1:6]
porcsD <- numsD/sum(base_LecFrec$Digital!=1010)*100
porcsI <- numsI/sum(base_LecFrec$Impreso!=1010)*100

frec <- c("Todos los días","Varias veces por semana","Una vez por semana","Una vez al mes",
          "Una vez cada 3 meses","Una vez al año")

siete_LecFrec <- data.frame(Formato=c(rep("Digital",6),rep("Impreso",6)),frec,nums=c(numsD,numsI),
                  porcs=c(porcsD,porcsI))

ggplot(siete_LecFrec, aes(x=reorder(frec,porcs),y=porcs,label=ifelse(porcs>4.2,paste0(round(porcs,1),"%"),"")))+
  theme_bw()+
  geom_bar( stat="identity", colour="black",size=.3,fill=rep(brewer.pal(6,"Pastel1"),2)) +
  geom_text(size=4,position=position_stack(vjust=0.5))+
  scale_x_discrete(limits=frec,labels = function(frec) stringr::str_wrap(frec,12))+
  coord_flip()+
  labs(x="¿Con qué frecuencia leyó libros en formato ...?",y="Porcentaje del total")+
  facet_wrap(~Formato,scales = "free_x")



    
    
  
  
  
  
  
