# Analyses of responses to the PHQ-4 in Chile during the COVID pandemic ----------------------------------------------------------------------
# herman.elgueta@umag.cl; @HElgueta
# Developed from Sept 2021 - until now #

# 1. Setup R, load and setup original datasets ----------------------------------------------
options(scipen=999)
library(tidyverse)

#download files "encuesta covid-19, chile"
#http://observatorio.ministeriodesarrollosocial.gob.cl/
download.file("http://observatorio.ministeriodesarrollosocial.gob.cl/storage/docs/covid19/Encuesta_Social_COVID-19.dta.zip","jul2020.dta.zip")
download.file("http://observatorio.ministeriodesarrollosocial.gob.cl/storage/docs/covid19/Encuesta_Social_COVID-19II.dta.zip","nov2020.dta.zip")
download.file("http://observatorio.ministeriodesarrollosocial.gob.cl/storage/docs/covid19/Encuesta_Social_COVID-19_III.dta.zip","jul2021.dta.zip")

#load datasets
jul20 <- haven::read_dta("jul2020.dta.zip", encoding = "UTF-8")
nov20 <- haven::read_dta("nov2020.dta.zip", encoding = "UTF-8")
jul21 <- haven::read_dta("jul2021.dta.zip", encoding = "UTF-8")

#remove files to avoid storing them (following usage policy AFAIK)
unlink("jul2020.dta.zip")
unlink("nov2020.dta.zip")
unlink("jul2021.dta.zip")

# small temporariy fixes
# change some var names to keep consistency across dfs, for now
jul21 <- dplyr::rename(jul21, sm011=sm_1)
jul21 <- dplyr::rename(jul21, sm012=sm_2)
jul21 <- dplyr::rename(jul21, sm013=sm_3)
jul21 <- dplyr::rename(jul21, sm014=sm_4)

# 2. Exploring the datasets ---------------------------------------------

# var list for each df
cat(colnames(jul20), sep="\n")
cat(colnames(nov20), sep="\n")
cat(colnames(jul21), sep="\n")


# 2.1 univariate exploratory analyses for jul20
#jul20$id_vivienda
#jul20$n_hogar
#jul20$id_hogar
unique(jul20$area);table(jul20$area)
unique(jul20$macrozona);table(jul20$macrozona)
#unique(jul20$estrato_covid);table(jul20$estrato_covid)
#jul20$varunit
#unique(jul20$lista_hogar__id);table(jul20$lista_hogar__id)
#unique(jul20$informante);table(jul20$informante)
#jul20$exp_macrozona
#jul20$exp_sm
#unique(jul20$id01);table(jul20$id01)
unique(jul20$id02);table(jul20$id02);hist(jul20$id02)#no personas en la vivienda
#unique(jul20$id03);table(jul20$id03)
#unique(jul20$id04);table(jul20$id04)
unique(jul20$ch00);table(jul20$ch00);hist(jul20$ch00)#no dormitorios
unique(jul20$ch02);table(jul20$ch02)#sexo
unique(jul20$ch03);table(jul20$ch03);hist(jul20$ch03)#edad
#unique(jul20$ch04);table(jul20$ch04)#id jefe hogar
unique(jul20$jh);table(jul20$jh)#jefe o no
unique(jul20$ch05);table(jul20$ch05)#tiene pareja o no, vive o no
#unique(jul20$ch06);table(jul20$ch06)#identificacion no pareja
unique(jul20$ch07);table(jul20$ch07)#nivel educacional
unique(jul20$ch08);table(jul20$ch08)#completo o no
unique(jul20$ch09);table(jul20$ch09)#acceso a internet
unique(jul20$ie01);table(jul20$ie01)#pega before covid
unique(jul20$ie02);table(jul20$ie02)#pega con contrato o no
unique(jul20$ie03);table(jul20$ie03)#situacion en comparación a precovid
#unique(jul20$ie04);table(jul20$ie04)#reunció por tener que cuidar a otros, n pequeño
#unique(jul20$ie05);table(jul20$ie05)#los que cambiaron de empleo
unique(jul20$ie06);table(jul20$ie06)#recursos aumentaron o disminuyeron
#unique(jul20$ie07__1);table(jul20$ie07__1)#porque disminuyeron ingresos
#unique(jul20$ie07__2);table(jul20$ie07__2)#porque disminuyeron ingresos
#unique(jul20$ie07__3);table(jul20$ie07__3)#porque disminuyeron ingresos
#unique(jul20$ie07__4);table(jul20$ie07__4)#porque disminuyeron ingresos
#unique(jul20$ie07__5);table(jul20$ie07__5)#porque disminuyeron ingresos
#unique(jul20$ie07__6);table(jul20$ie07__6)#porque disminuyeron ingresos
#unique(jul20$ie07__7);table(jul20$ie07__7)#porque disminuyeron ingresos
#unique(jul20$ie07_1);table(jul20$ie07_1)#otros motivos, interesante tho
#unique(jul20$ie08);table(jul20$ie08)#personas sin emmpleo antes, la mayoria se mantiene así
#unique(jul20$ie09);table(jul20$ie09)#tiene contrato siono, los que ahora tienen empleo
unique(jul20$ie10);table(jul20$ie10)#mas recursos o menos recursos context covid
#unique(jul20$ie11);table(jul20$ie11)#comparacion ingreso mensual previo
unique(jul20$ie12);table(jul20$ie12)#antes del covid, alcanzaba?
unique(jul20$ie13);table(jul20$ie13)#con el covid, alcanza?
unique(jul20$ie14_1);table(jul20$ie14_1)#ha hecho cosas extra para tener ingresos?
unique(jul20$ie14_2);table(jul20$ie14_2)#ha hecho cosas extra para tener ingresos?
unique(jul20$ie14_3);table(jul20$ie14_3)#ha hecho cosas extra para tener ingresos?
unique(jul20$ie14_4);table(jul20$ie14_4)#ha hecho cosas extra para tener ingresos?
unique(jul20$ie14_5);table(jul20$ie14_5)#ha hecho cosas extra para tener ingresos?
unique(jul20$ie14_6);table(jul20$ie14_6)#ha hecho cosas extra para tener ingresos?
unique(jul20$ie14_7);table(jul20$ie14_7)#ha hecho cosas extra para tener ingresos?
unique(jul20$ie14_8);table(jul20$ie14_8)#ha hecho cosas extra para tener ingresos?
unique(jul20$ie14_9);table(jul20$ie14_9)#ha hecho cosas extra para tener ingresos?
#unique(jul20$ie14_especifique);table(jul20$ie14_especifique)cual?
unique(jul20$ie15_1);table(jul20$ie15_1)#cosas que ha tenido que dejar de pagar?
unique(jul20$ie15_2);table(jul20$ie15_2)#cosas que ha tenido que dejar de pagar?
unique(jul20$ie15_3);table(jul20$ie15_3)#cosas que ha tenido que dejar de pagar?
unique(jul20$ie15_4);table(jul20$ie15_4)#cosas que ha tenido que dejar de pagar?
unique(jul20$ie15_5);table(jul20$ie15_5)#cosas que ha tenido que dejar de pagar?
unique(jul20$ie15_6);table(jul20$ie15_6)#cosas que ha tenido que dejar de pagar?
unique(jul20$ie15_7);table(jul20$ie15_7)#cosas que ha tenido que dejar de pagar?
unique(jul20$ie15_8);table(jul20$ie15_8)#cosas que ha tenido que dejar de pagar?
unique(jul20$ie15_9);table(jul20$ie15_9)#cosas que ha tenido que dejar de pagar?
unique(jul20$ie15_10);table(jul20$ie15_10)#cosas que ha tenido que dejar de pagar?
unique(jul20$ie15_11);table(jul20$ie15_11)#cosas que ha tenido que dejar de pagar?
#unique(jul20$ie15_especifique);table(jul20$ie15_especifique)#otros?
#unique(jul20$ie16_1);table(jul20$ie16_1)#mas detalles sobre lo anterior
#unique(jul20$ie16_2);table(jul20$ie16_2)#mas detalles sobre lo anterior
#unique(jul20$ie16_3);table(jul20$ie16_3)#mas detalles sobre lo anterior
#unique(jul20$ie16_4);table(jul20$ie16_4)#mas detalles sobre lo anterior
#unique(jul20$ie17_1);table(jul20$ie17_1)#mas detalles sobre lo anterior
#unique(jul20$ie17_2);table(jul20$ie17_2)#mas detalles sobre lo anterior
#unique(jul20$ie17_3);table(jul20$ie17_3)#mas detalles sobre lo anterior
#unique(jul20$ie17_4);table(jul20$ie17_4)#mas detalles sobre lo anterior
#unique(jul20$ie18_1);table(jul20$ie18_1)#mas detalles sobre lo anterior
#unique(jul20$ie18_2);table(jul20$ie18_2)#mas detalles sobre lo anterior
#unique(jul20$ie18_3);table(jul20$ie18_3)#mas detalles sobre lo anterior
#unique(jul20$ie18_4);table(jul20$ie18_4)#mas detalles sobre lo anterior
#unique(jul20$ie18_5);table(jul20$ie18_5)#mas detalles sobre lo anterior
#unique(jul20$ie19_1);table(jul20$ie19_1)#mas detalles sobre lo anterior
#unique(jul20$ie19_2);table(jul20$ie19_2)#mas detalles sobre lo anterior
#unique(jul20$ie19_3);table(jul20$ie19_3)#mas detalles sobre lo anterior
#unique(jul20$ie19_4);table(jul20$ie19_4)#mas detalles sobre lo anterior
unique(jul20$ie20_1);table(jul20$ie20_1)#alimentacion##se preocuparon por no tener alimentos wow
unique(jul20$ie20_2);table(jul20$ie20_2)#alimentacion##no pudieron comer alimentos saludables
unique(jul20$ie20_3);table(jul20$ie20_3)#alimentacion##poca variedad de alimentos
unique(jul20$ie20_4);table(jul20$ie20_4)#alimentacion##dejar de desayunar
unique(jul20$ie20_5);table(jul20$ie20_5)#alimentacion#
unique(jul20$ie20_6);table(jul20$ie20_6)#alimentacion#
unique(jul20$ie20_7);table(jul20$ie20_7)#alimentacion#
unique(jul20$ie20_8);table(jul20$ie20_8)#alimentacion#
#unique(jul20$ie21);table(jul20$ie21)#riesgos?
#AYUDAS
#unique(jul20$ay01);table(jul20$ay01)
#unique(jul20$ay02__1);table(jul20$ay02__1)
#unique(jul20$ay02__1_c);table(jul20$ay02__1_c)
#unique(jul20$ay02__2);table(jul20$ay02__2)
#unique(jul20$ay02__2_c);table(jul20$ay02__2_c)
#unique(jul20$ay02__3);table(jul20$ay02__3)
#unique(jul20$ay02__3_c);table(jul20$ay02__3_c)
#unique(jul20$ay02__4);table(jul20$ay02__4)
#unique(jul20$ay02__5);table(jul20$ay02__5)
#unique(jul20$ay02__5_c);table(jul20$ay02__5_c)
#unique(jul20$ay02__6);table(jul20$ay02__6)
#unique(jul20$ay02__7);table(jul20$ay02__7)
#unique(jul20$ay02__7_c);table(jul20$ay02__7_c)
#unique(jul20$ay02__8);table(jul20$ay02__8)
#unique(jul20$ay02__8_c);table(jul20$ay02__8_c)
#unique(jul20$ay02__9);table(jul20$ay02__9)
#unique(jul20$ay02__9_c);table(jul20$ay02__9_c)
#unique(jul20$ay02_especifique);table(jul20$ay02_especifique)
#unique(jul20$ay03);table(jul20$ay03)
#unique(jul20$ay04);table(jul20$ay04)
#unique(jul20$ay04_especifique);table(jul20$ay04_especifique)
#labores domesticas
unique(jul20$cd01);table(jul20$cd01)#quien lo hace, hombres/mujeres
unique(jul20$cd02);table(jul20$cd02)#equitativo
hist(jul20$y01);median(jul20$y01);IQR(jul20$y01)#ingreso mensual
#unique(jul20$y02_1);table(jul20$y02_1)
#unique(jul20$y02_2);table(jul20$y02_2)
#unique(jul20$y02_3);table(jul20$y02_3)
#unique(jul20$y02_4);table(jul20$y02_4)
#unique(jul20$y02_5);table(jul20$y02_5)
#unique(jul20$y02_6);table(jul20$y02_6)
#unique(jul20$y02_7);table(jul20$y02_7)
#unique(jul20$y02_8);table(jul20$y02_8)
#unique(jul20$y03_1);table(jul20$y03_1)#no integrantes cambio
unique(jul20$y03_2);table(jul20$y03_2);hist(jul20$y03_2)#no integrantes
#cor(jul20$id02,jul20$y03_2,use="complete.obs")
#ggplot(jul20, aes(x=y03_2, y=id02)) + geom_point(position=position_jitter(width=0.1, height=0.1), alpha=2/10) +
#  labs(y = "qwqwq") + labs(x = "asas")

hist(jul20$y03);median(jul20$y03);IQR(jul20$y03)#ingreso mensual 2019 #hist(sqrt(sqrt(jul20$y03)))
#plot(sqrt(sqrt(jul20$y03)),sqrt(sqrt(jul20$y01))) #nice scatterplot lol
#unique(jul20$y04_1);table(jul20$y04_1)#more details 2019
#unique(jul20$y04_2);table(jul20$y04_2)#more details 2019
#unique(jul20$y04_3);table(jul20$y04_3)#more details 2019
#unique(jul20$y04_4);table(jul20$y04_4)#more details 2019
#unique(jul20$y04_5);table(jul20$y04_5)#more details 2019
#unique(jul20$y04_6);table(jul20$y04_6)#more details 2019
#unique(jul20$y04_7);table(jul20$y04_7)#more details 2019
#unique(jul20$y04_8);table(jul20$y04_8)#more details 2019
unique(jul20$sm011);table(jul20$sm011)
unique(jul20$sm012);table(jul20$sm012)
unique(jul20$sm013);table(jul20$sm013)
unique(jul20$sm014);table(jul20$sm014)
#unique(jul20$di01);table(jul20$di01)#denuncia violencia?
unique(jul20$th);table(jul20$th);hist(jul20$th)#no integrantes hogar
#cor(jul20$th,jul20$y03_2,use="complete.obs")
#cor(jul20$th,jul20$id02,use="complete.obs")#almost the same as no of people in house
unique(jul20$s);table(jul20$s)#hay personas de ambos generos?
unique(jul20$nna);table(jul20$nna)# hay nnas?
unique(jul20$indicador_cuarentena);table(jul20$indicador_cuarentena)#cuarantena at the moment
unique(jul20$tramo_pobreza);table(jul20$tramo_pobreza)#pobreza de la comuna
unique(jul20$thogar2);table(jul20$thogar2)#tiene adultos mayores
unique(jul20$qytot_19);table(jul20$qytot_19)#quintil del hogar 2019
unique(jul20$qytot_20);table(jul20$qytot_20)#quintil de hogar 2020
unique(jul20$qytot_20_19);table(jul20$qytot_20_19)#quintil wat?

jul20$indicador_cuarentena


# 3. Merging datasets --------------------------------------------------
# subset data with variables and cases of interest
#ss_jul20 <- na.omit(jul20[c(3:5,8,17,18,13,16,20,21,23,25,142:150,137:140)])
#ss_nov20 <- na.omit(nov20[c(3,5:6,10,21,163:166)])
#ss_jul21 <- na.omit(jul21[c(3,7:8,4,21,195:198)])

ss_jul20 <- jul20[c(3:5,8,17,18,13,16,20,21,23,25,142:150,137:140)]
ss_nov20 <- nov20[c(3,5:6,10,21,163:166)]
ss_jul21 <- jul21[c(3,7:8,4,21,195:198)]

# rename variables with same name for each df
ss_jul20 <- dplyr::rename(ss_jul20, j20phq1=sm011)
ss_jul20 <- dplyr::rename(ss_jul20, j20phq2=sm012)
ss_jul20 <- dplyr::rename(ss_jul20, j20phq3=sm013)
ss_jul20 <- dplyr::rename(ss_jul20, j20phq4=sm014)
ss_nov20 <- dplyr::rename(ss_nov20, n20phq1=sm011)
ss_nov20 <- dplyr::rename(ss_nov20, n20phq2=sm012)
ss_nov20 <- dplyr::rename(ss_nov20, n20phq3=sm013)
ss_nov20 <- dplyr::rename(ss_nov20, n20phq4=sm014)
ss_jul21 <- dplyr::rename(ss_jul21, j21phq1=sm011)
ss_jul21 <- dplyr::rename(ss_jul21, j21phq2=sm012)
ss_jul21 <- dplyr::rename(ss_jul21, j21phq3=sm013)
ss_jul21 <- dplyr::rename(ss_jul21, j21phq4=sm014)

#create id var for later merging
ss_jul20$id <- ss_jul20$id_hogar*1000+ss_jul20$lista_hogar__id
ss_nov20$id <- ss_nov20$id_hogar*1000+ss_nov20$lista_hogar__id
ss_jul21$id <- ss_jul21$id_hogar*1000+ss_jul21$lista_hogar__id

#merging part 1
df <- dplyr::inner_join(ss_jul20,ss_nov20, by="id")
#check if successful
#table(df$area.x,df$area.y) #perfect
#table(df$macrozona.x,df$macrozona.y) #perfect
#merging part 2
df <- dplyr::inner_join(df,ss_jul21, by="id")
#remove redundant variables
cat(colnames(df), sep="\n")
df <- df[c("id","area","macrozona","ch02","ch03","id02","th","ch00","jh","ch05","ch07","ch09","s","nna","indicador_cuarentena","tramo_pobreza","thogar2","qytot_19","qytot_20","qytot_20_19","j20phq1","j20phq2","j20phq3","j20phq4","n20phq1","n20phq2","n20phq3","n20phq4","j21phq1","j21phq2","j21phq3","j21phq4")]
cat(colnames(df), sep="\n")



#rename vars for simplicity/consistency
df <- dplyr::rename(df, idno=id)
df <- dplyr::rename(df, area=area)
df <- dplyr::rename(df, zone=macrozona)
df <- dplyr::rename(df, sexr=ch02)
df <- dplyr::rename(df, ager=ch03)
df <- dplyr::rename(df, nph1=id02)
df <- dplyr::rename(df, nph2=th)
df <- dplyr::rename(df, nbed=ch00)
df <- dplyr::rename(df, hhou=jh)
df <- dplyr::rename(df, part=ch05)
df <- dplyr::rename(df, edlv=ch07)
df <- dplyr::rename(df, inta=ch09)
df <- dplyr::rename(df, gmix=s)
df <- dplyr::rename(df, kids=nna)
df <- dplyr::rename(df, quar=indicador_cuarentena)
df <- dplyr::rename(df, cpov=tramo_pobreza)
df <- dplyr::rename(df, elde=thogar2)
df <- dplyr::rename(df, qu19=qytot_19)
df <- dplyr::rename(df, qu20=qytot_20)
df <- dplyr::rename(df, oq20=qytot_20_19)
cat(colnames(df), sep="\n")


#define var types
df$idno <- as.numeric(df$idno)
df$area <- as.numeric(df$area)
df$zone <- as.numeric(df$zone)
df$sexr <- as.numeric(df$sexr)
df$ager <- as.numeric(df$ager)
df$nph1 <- as.numeric(df$nph1)
df$nph2 <- as.numeric(df$nph2)
df$nbed <- as.numeric(df$nbed)
df$hhou <- as.numeric(df$hhou)
df$part <- as.numeric(df$part)
df$edlv <- as.numeric(df$edlv)
df$inta <- as.numeric(df$inta)
df$gmix <- as.numeric(df$gmix)
df$kids <- as.numeric(df$kids)
df$quar <- as.numeric(df$quar)
df$cpov <- as.numeric(df$cpov)
df$elde <- as.numeric(df$elde)
df$qu19 <- as.numeric(df$qu19)
df$qu20 <- as.numeric(df$qu20)
df$oq20 <- as.numeric(df$oq20)
df$j20phq1 <- as.numeric(df$j20phq1)
df$j20phq2 <- as.numeric(df$j20phq2)
df$j20phq3 <- as.numeric(df$j20phq3)
df$j20phq4 <- as.numeric(df$j20phq4)
df$n20phq1 <- as.numeric(df$n20phq1)
df$n20phq2 <- as.numeric(df$n20phq2)
df$n20phq3 <- as.numeric(df$n20phq3)
df$n20phq4 <- as.numeric(df$n20phq4)
df$j21phq1 <- as.numeric(df$j21phq1)
df$j21phq2 <- as.numeric(df$j21phq2)
df$j21phq3 <- as.numeric(df$j21phq3)
df$j21phq4 <- as.numeric(df$j21phq4)

#remove subset dfs to cleanup
rm(ss_jul20,ss_nov20,ss_jul21)


# 4. CFAs -----------------------------------------------------------------
cat(colnames(df), sep="\n")

mod <- 'j20phq =~ j20phq1 + j20phq2 + j20phq3 + j20phq4
        n20phq =~ n20phq1 + n20phq2 + n20phq3 + n20phq4
        j21phq =~ j21phq1 + j21phq2 + j21phq3 + j21phq4'

fit <- lavaan::cfa(mod,df,estimator="ULSMV")
lavaan::fitMeasures(fit, c("chisq.scaled","df.scaled","pvalue.scaled","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr"))
lavaan::standardizedSolution(fit)



#########great stuff


df2 <- data.frame(lavaan::lavPredict(fit,append.data=TRUE))
df2$j20phqs <- df2$j20phq1+df2$j20phq2+df2$j20phq3+df2$j20phq4
df2$n20phqs <- df2$n20phq1+df2$n20phq2+df2$n20phq3+df2$n20phq4
df2$j21phqs <- df2$j21phq1+df2$j21phq2+df2$j21phq3+df2$j21phq4
plot(df2$j20phq,df2$j20phqs)
cor(df2$j20phq,df2$j20phqs)
hist(df2$j20phqs)

cor(df2$j20phq,df2$n20phq)
cor(df2$j20phqs,df2$n20phqs)
hist(scales::rescale(df2$j20phq,to = c(0, 100)))
hist(scales::rescale(df2$j20phq,to = c(0, 16)))

length(unique(df2$j20phq));length(unique(df2$j20phqs))
length(unique(df2$j20phq))
length(unique(df2$n20phq))
length(unique(df2$j21phq))


hist(df2$j20phq)
hist(sqrt(df2$j20phq+100))
hist(log(log(log(log(log(df2$j20phq+10000000))))))
hist(df2$n20phq)
hist(df2$j21phq)
plot(sqrt(df2$j20phq),sqrt(df2$n20phq))
plot(df2$j20phq,df2$j21phq)
plot(log(log(df2$j20phq+1000000)),log(log(df2$j21phq+1000000)))


plot(df2$j20phq,df2$n20phq)
cor(df2$j20phq,df2$n20phq)

df2$area <- df$area
df2$zone <- df$zone
boxplot(df2$j20phq ~ df2$area)
boxplot(df2$j20phq ~ df2$zone)

plot(df2$j20phq ~ df2$zone)

hist(scales::rescale(df2$j20phq,to = c(0, 100)))

boxplot(scales::rescale(df2$j20phq,to = c(0, 100)) ~ df2$zone)
?boxplot
#plots
library(ggplot2)
library(ggExtra)
#  scatterplots


ggplot(df2, aes(x=j20phq, y=n20phq)) + geom_point(position=position_jitter(width=0.1, height=0.1), alpha=2/10) +
  labs(y = "Mental Health November 2020") + labs(x = "Mental Health July 2020")



p <- ggplot(df2, aes(x=j20phq, y=n20phq)) + geom_point(position=position_jitter(width=0.1, height=0.1), alpha=2/10) +
  labs(y = "Mental Health November 2020") + labs(x = "Mental Health July 2020")
p <- ggMarginal(p, type="histogram", size=10)
p

ggplot(df2, aes(x=j20phq, y=n20phq) ) +
  geom_bin2d(bins = 12) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()

ggplot(df2, aes(x=j20phq, y=n20phq) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    legend.position='none'
  )




# Change the point size, and shape
ggplot(mtcars, aes(x=wt, y=mpg)) +
  geom_point(size=2, shape=23)



# 5 Multigroup CFAs -------------------------------------------------------

df$sexr <- as.factor(df$sexr)
fitconfig <- lavaan::cfa(mod, data=df, estimator="ML", group="sexr")
fitmetric <- lavaan::cfa(mod, data=df, estimator="ML", group="sexr", group.equal = c("loadings"))
fitscalar <- lavaan::cfa(mod, data=df, estimator="ML", group="sexr", group.equal = c("loadings","intercepts"))
fitresidu <- lavaan::cfa(mod, data=df, estimator="ML", group="sexr", group.equal = c("loadings","intercepts","residuals"))
paste(round(lavaan::fitMeasures(fitconfig, c("chisq","df","cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","srmr")),3))
paste(round(lavaan::fitMeasures(fitmetric, c("chisq","df","cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","srmr")),3))
paste(round(lavaan::fitMeasures(fitscalar, c("chisq","df","cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","srmr")),3))
paste(round(lavaan::fitMeasures(fitresidu, c("chisq","df","cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","srmr")),3))
lavaan::anova(fitconfig,fitmetric,fitscalar,fitresidu)





# 6 SEMs ------------------------------------------------------------------

