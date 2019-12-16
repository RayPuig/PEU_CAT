#--------------------------------------------------------------------------------#
#[16.11.2019]               (ACTUALITZACIONS)
#--------------------------------------------------------------------------------#
#[09.04.2019]
#--------------------------------------------------------------------------------#
#[08.04.2019]
#--------------------------------------------------------------------------------#
#[05.04.2019]
#[04.04.2019]
#[03.04.2019]
#[27.03.2019]
#[26.03.2019]
#[25.03.2019]
#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
#[05.11.2018]:[17:30]-->[22.03.2109]#
#--------------------------------------------------------------------------------#



#------------------------------------------------------------------------------------------#
#                        1. ETIQUETAR VARIABLES! []
#------------------------------------------------------------------------------------------#
###

etiquetar<-function(d=dadestotal,taulavariables="variables_R.xls") {
  
  ####  Llegir etiquetes i variables a analitzar ####
  variables <- readxl::read_excel(taulavariables)
  variables[is.na(variables)]<- 0
  #
  #
  ###################################   etiquetar variables         
  seleccio<-variables
  camp<- as.vector(seleccio$camp) #
  descripcio<- as.vector(seleccio$descripcio) #
  ### etiquetar variables seleccionades     #
  for (i in 2:length(descripcio)){if (any(colnames(d) == camp[i])) {Hmisc::label(d[[camp[i]]]) <- descripcio[i]}}
  d
}

#------------------------------------------------------------------------------------------#
#                        2. CONVERTIR DATES![de num?ric a codo Data!]
#------------------------------------------------------------------------------------------#
#------------------------------------------------------------------#
convertir_dates<-function(d=dadestotal,taulavariables="variables_R.xls")
  
{
  ####  Llegir etiquetes i variables a analitzar ####
  variables <- readxl::read_excel(taulavariables)
  variables[is.na(variables)]<- 0
  #
  #
  ###################################   etiquetar variables         
  seleccio<-variables
  camp<- as.vector(seleccio$camp) #
  # dates<-as.vector(seleccio$dates)
  # dates<-seleccio[[campdata]]
  ### etiquetar variables seleccionades     #
  
  for (i in 1:length(camp)){if (seleccio$dates[i]==1) { 
    
    pepito<-paste0("as.Date(d[[camp[",i,"]]], '%Y%d%m')")
    
    d[[camp[i]]]<-eval(parse(text=pepito))
    
  } }
  
  d
  
}
#------------------------------------------------------------------#




#------------------------------------------------------------------#
LAB_ETIQ_v2<-function(dt=dades,variables_factors=conductor_variables,fulla="etiquetes",idioma="etiqueta1"){
  
  #dt=dades
  #variables_factors=conductor_variables
  
  
  #------------------------------------------------------------------#
  variables_factors<-readxl::read_excel(variables_factors,sheet=fulla)
  #------------------------------------------------------------------#
  
  
  
  if (idioma=="etiqueta1") {
    
    k<-variables_factors%>%select(camp, valor,etiqueta1)
    
  } else if (idioma=="etiqueta2") {
    
    k<-variables_factors%>%select(camp, valor,etiqueta2)
    k<-k%>%mutate(etiqueta1=etiqueta2)
    k<-k%>%select(camp, valor,etiqueta1)
    
  }
  
  
  #------------------------------------------------------------------#
  pepe<-k %>% split(list(.$camp))
  #------------------------------------------------------------------#
  #
  noms_variables<-names(pepe)
  num_vars<-length(noms_variables)
  
  for (i in 1:num_vars) {
    
    dt[noms_variables[i]]<-lapply(dt[noms_variables[i]],function(y) factor(y,levels=pepe[[i]]$valor,labels=pepe[[i]]$etiqueta1))
    
  }
  
  dt}
#------------------------------------------------------------------#
#install.packages("readxl")
#--------------------------------------------------------------------------------#
library("readxl")
library(magrittr)
library(svglite)
library(rsvg)
library(jpeg)
library(plotrix)
library(rasterImage) 
library(png)
library(grid)
library(Rcpp)
library(htmlTable)
library(Gmisc)
library(ggplot2)
library(stats)
library(graphics)
library(plotly)
library(Hmisc)
library(gdata)
library(xtable)
library(SNPassoc)
library(survival)
library(epitools)
library(tools)
library(HardyWeinberg)
library(rmarkdown)
library(knitr)
library(psych)
library(qgraph)
library(MASS)
library(labelled)
library(haplo.stats)
library(compareGroups)
library(naniar)
library(glue)
library(svglite)
library(lubridate)
library(dplyr)
library(tibble)
library(DiagrammeR)
library(DiagrammeRsvg)
library(tidyverse)
library(compareGroups)



#--------------------------------------------------------------------------------#
#idcrd                                  [idcrd]
#estPacInc	                            [Estado paciente.Incluido]
#estPacDisAb	                          [Estado paciente.Discontinuado por abandono]
#estPacDisFa	                          [Estado paciente.Discontinuado por fallecimiento]
#estCrdCur	                            [Estado CRD.En curso]
#estCrdFin	                            [Estado CRD.Finalizado]
#idInvest	                              [idinvestigador]
#--------------------------------------------------------------------------------#
#fConsenti	                            [Fecha consentimiento informado]
#codPac	                                [C?digo paciente]
#cip	                                  [CIP]
#inclu1	                                [Criterios inclusi?n 1]
#inclu2	                                [Criterios inclusi?n 2]
#inclu3	                                [Criterios inclusi?n 3]
#exclu1	                                [Criterios exclusi?n 1]
#exclu2	                                [Criterios exclusi?n 2]
#exclu3	                                [Criterios exclusi?n 3]

#--------------------------------------------------------------------------------#
#                 Base de datos CRDS GENERAL                                     #
#--------------------------------------------------------------------------------#

#fNac	                                  [Fecha nacimiento]
      #i--> Demogr?afiques: edat i sexe
#edad	                                  [Edad]
#sexoHombre	                            [Sexo Hombre]
#sexoMujer	                            [Sexo Mujer]
#etniaCau	                              [Etnia Cauc?sica]
#etinaHisp	                            [Etnia Hispanoamericana]
#etniaIndos	                            [Etnia Indostan?]
#etniaAfr	                              [Etnia Africana]
#etniaOtr	                              [Etnia Otras]
#espEtnia	                              [Especificar etnia]
      #ii--> H?bits t?xics: tabac i alcohol
#tabNo	                                [Tabaco No fumador]
#tabacoSi	                              [Tabaco Fumador]
#tabacoEx	                              [Tabaco Exfumador]
#cigaDia	                              [Cigarrillos/d?a]
#anoTab	                                [A?os tabaquismo]
#paqTab	                                [Paquetes A?o]
#alcAbs	                                [Alcohol Abstemio]
#alcBaj	                                [Alcohol Bajo Riesgo]
#alcAlt	                                [Alcohol Alto Riesgo]
#antAlcohol	                            [Antecedentes de consumo excesivo]
      #iii-->Factors de risc cardiovascular:[antecedents personals d'hipertensi? areterial i dislip?mia]
#paSis1	                                [PA sist?lica 1]
#paDia2	                                [Pa diast?lica 1]
#paSis2	                                [PA sist?lica 2]
#paDia2	                                [PA diast?lica 2]
#medPs	                                [MEDIA PAS]
#medPd	                                [MEDIA PAD]
#diaHta	                                [Diagn?stico HTA]
#diaDis	                                [Diagn?stico dislipemia]
      #iv-->Antecedents personals de malaltia cardiovascular:AVC,cardiopatia isqu?mica,arteriopatia perif?rica
#avc	                                  [AVC]
#acvAno	                                [AVC a?o]
#carIsq	                                [Cardiopat?a isqu?mica]
#carIsqAno	                            [Cardiopat?a isqu?mica a?o]
#artPer	                                [Arteriopat?a perif?rica]
#artPerAno	                            [Arteriopat?a perif?rica a?o]
#antArt	                                [Antecedentes por intervenci?n de arteriopat?a perif?rica]
#antArtAno	                            [Antecedentes por intervenci?n de arteriopat?a perif?rica a?o]
      #v-->Antecedents personals d'insufici?ncia card?aca
#antInsCar	                            [Antecedentes personales de insuficiencia card?aca]
#antInsCarA	                            [Antecedentes personales de insuficiencia card?aca a?o]
#--------------------------------------------------------------------------------#
      #vi-->En relaci? a la DM2
#dVisBa	                                [Diagn?stico en la visita basal]
#anoDiagDm	                            [A?o de diagn?stico definitivo de DM]
#sinTrat	                              [TratamientoDM2.Sin tratamiento]
#metfor	                                [TratamientoDM2.Metformina]
#su	                                    [TratamientoDM2.SU]
#piog	                                  [TratamientoDM2.Pioglitazona]
#repag	                                [TratamientoDM2.Repaglinida]
#idpp4	                                [TratamientoDM2.iDPP4]
#isglt2	                                [TratamientoDM2.iSGLT2]
#anaGlp1	                              [TratamientoDM2.An?logos GLP1]
#insu	                                  [TratamientoDM2.Insulina]
#alfaG	                                [TratamientoDM2.??glucosidasa]
#otro	                                  [TratamientoDM2.Otros]
#explOtro	                              [TratamientoDM2.Explicacion.Otros]
#noFarma	                              [Tratamiento diabetes no farmacol?gico]
#Reti	                                  [Retinopat?a]
#retiAno	                              [Retinopat?a a?o]
#NeuPer	                                [Neuropat?a perif?rica]
#NeuPerAno	                            [Neuropat?a perif?rica a?o]
#NeuAut	                                [Neuropat?a aut?noma]
#NeuAutAno	                            [Neuropat?a aut?noma a?o]
#inReCr	                                [Insuficiencia renal cr?nica]
#inReCrAno	                            [Insuficiencia renal cr?nica a?o]
#microA	                                [Microalbuminuria]
#microAAno	                            [Microalbuminuria a?o]
#macro	                                [Macroalbuminuria o proteinuria/24 horas]
#macroAno	                              [Macroalbuminuria o proteinuria/24 horas a?o]
#actDial	                              [Actualmente en di?lisis]
#actDialAno	                            [Actualmente en di?lisis a?o]
#traRen	                                [Transplante renal]
#traRenAno	                            [Transplante renal a?o]
#neuCh	                                [Neuroartropat?a de Charcot]
#neuChAno	                              [Neuroartropat?a de Charcot a?o]
#--------------------------------------------------------------------------------#
        #vii-->Laboratori
#fAna	                                  [Fecha anal?tica]
#hba1c	                                [HBA1C]
#colTot	                                [Colesterol total]
#unColTot	                              [Unidad Colesterol total]
#hdl	                                  [HDL]
#unHdl	                                [Unidad HDL]
#ldl	                                  [LDL]
#unLdl	                                [Unidad LDL]
#tri	                                  [Trigliceridos]
#unTri	                                [Unidad Trigliceridos]
#crea	                                  [Creatinina]
#unCrea	                                [Unidad Creatinina]
#filGlo	                                [Filtrado glomerular estimado]
#unFilGlo	                              [Unidad Filtrado glomerular estimado]
#coAlCre	                              [Cociente Alb?mina/Creatinina o proteinuria]
#unCoAlCre	                            [Unidad Cociente Alb?mina/Creatinina o proteinuria]
#fExp	                                  [Fecha exploraci?n]
        #viii-->Pes i Talla
#peso	                                  [Peso]
#Talla	                                [Talla]
#Imc	                                  [IMC]
        #ix-->Valoraci? de l'autocura,prese?ncia de cuidador,visita al pod?leg en el darrer any
#disAgVis                             	[Disminuci?n de la agudeza visual]
#prMovil	                              [Problemas con la movilidad]
#cuidador	                              [Tiene cuidador]
#visPodAno	                            [Visita al pod?logo en el ?ltimo a?o]
        #x--> inspecci? Peus
#calCorrec	                            [Calzado correcto]
#hidCorrec	                            [Hidrataci?n correcta]
#espIntLes	                            [Espacios interdigitales con lesiones mic?ticas]
#patUng	                                [Patolog?a ungueal]
#cuiU?aCor	                            [Cuidado de las u?as correcto]
#callosi	                              [Hiperqueratosis/callosidades]
#edemaMal	                              [Edema maleolar]
#otrosExp	                              [Otros]
#expOtroExp	                            [Explicaci?n otros]
#def	                                  [Deformidades]
#deGarra	                              [Dedos en garra/martillo]
#hallux	                                [Halluxvalgus]
#pieCon	                                [Pie plano/cavo]
#sobreMeta	                            [Sobrecarga metatarsianos]
#charcot	                              [Charcot]
#otraExp	                              [Otras]
#tibPosDer	                            [Tibial posterior pie derecho]
#tibPosIzq	                            [Tibial posterior pie izquierdo]
#pedioDer	                              [Pediopie derecho]
#pedioIzq	                              [Pediopie izquierdo]
#valorable	                            [?Es valorable?]
#fITB	                                  [Fecha de ITB]
#itbDer	                                [Derecho]
#itbIzq	                                [Izquierdo]
#motNoReal	                            [Motivo no realizaci?n]
#NeuPerVal	                            [Neuropat?a perif?rica ?Es valorable?]
#monoDer	                              [Monofilamento pie derecho]
#monoIzq	                              [Monofilamento pie izquierdo]
#sensVibDer	                            [Sensibilidad vibratoria pie derecho]
#sensVibIzq	                            [Sensibilidad vibratoria pie izquierdo]
#ipsDer	                                [Ipswitch Touch Test derecho]
#ipsIzq	                                [Ipswitch Touch Test izquierdo]
#ipsNoReal	                            [Motivo no realizaci?n]
#--------------------------------------------------------------------------------#
#casa-->Av Estatut de Catalunya!.
#----------------------------------------------------
#setwd("C:/Users/Suta/Desktop/peucat2/peucat3")
#"C:/Users/Suta/Desktop/peucat2/peucat3" %>% file.path("FUNCIONS_PROPIES.R") %>% source()
#----------------------------------------------------
#Sant Pau.[C:\Users\Suta\Desktop\4.3.2019]
#setwd("C:/Users/Suta/Desktop/4.3.2019")
#"C:/Users/Suta/Desktop/4.3.2019" %>% file.path("FUNCIONS_PROPIES.R") %>% source()
#----------------------------------------------------
#feina-->c/Sardenya![C:\Users\38122893W\Desktop\PeuCat_Final]
#----------------------------------------------------

#----------------------------------------------------
#casa-->Av Estatut de Catalunya!
#setwd("C:/Users/Suta/Desktop/peucat_FINAL")
#"C:/Users/Suta/Desktop/peucat_FINAL"%>% file.path("FUNCIONS_PROPIES3.R") %>% source()
#----------------------------------------------------


#setwd("C:/Users/38122893W/Desktop/PeuCat_Final2")
#"C:/Users/38122893W/Desktop/PeuCat_Final2"%>% file.path("FUNCIONS_PROPIES3.R") %>% source()
#----------------------------------------------------


#----------------------------------------------------#
#setwd("C:/Users/38122893W/Desktop/PeuCat")
#"C:/Users/38122893W/Desktop/PeuCat"%>% file.path("funcions_propies_2019.R") %>% source()





#----------------------------------------------------#
#setwd("C:/Users/38122893W/Desktop/PeuCat_Final2")
#"C:/Users/38122893W/Desktop/PeuCat_Final2"%>% file.path("funcions_propies_2019.R") %>% source()
#----------------------------------------------------


#----------------------------------------------------
#setwd("C:/Users/Suta/Desktop/peuCat_FINAL2")
#"C:/Users/Suta/Desktop/peucat_FINAL2"%>% file.path("funcions_propies_2019.R") %>% source()
#----------------------------------------------------
fitxer_dades<-"exportacionCRD_peu_cat_final3.xls"
PEU_CAT_CDRS<-read_excel(fitxer_dades,sheet="CRDS")
########################################################################
#[Transforamcions de les variables!]: [R E C O D I F I C A C I O N S] 
########################################################################
#############################################################################
# 1.- Monofilamento
# -	Normal: 4 puntos encontrados
# -	Patol?gico:  ??? 3 puntos
# 2.- Monofilamento patol?gico + sensibilidad vibratoria ausente
# 3.- Ipswith Touch Test
# -	normal: 3 puntos
# -	patol?gico: ??? 2 puntos
# 4.- Arteriopat?a perif?rica
# -	presencia dos pulsos (P o TP)
# -	ausencia un pulso (P o TP)
# -	ausencia dos pulsos (P y TP)
# Falta tambi?n
# 1.- antecedentes de HTA: si/no
# 2.- antecedentes de dislipemia: si/no
# 3.- tiempo evoluci?n de la diabetes mellitus
# 4.- antecedentes personales de:
#   -	neuropat?a perif?rica
#   -	neuropat?a aut?noma
#   -	insuficiencia renal
#   -	microalbuminuria o macroalbuminuria
#   -	actualmente en di?lisis
#   -	trasplante renal
#   -	neuroartropatia de Charcot
#   -	antecedentes de ulceras
#   -	antecedentes de amputaci?n
#############################################################################


#***********************************************************************#
#Tots els pacients DIABETIS TIPUS 2, atesos als centres de salut i que participin en l'estudi.
#S'inclouran en l'estudi tots els pacients que presentin una ?lcera en un peu durant el periode de recollida de dades,
#i COMPLEIXIN TOTS ELS CRITERIS D'INCLUSI? I CAP EXLUSI?.
#***********************************************************************#





#----------------------------------------------------------------#
#2.- Arteriopat?a perif?rica
#       -	presencia dos pulsos (P o TP)
#       -	ausencia un pulso (P o TP)
#       -	ausencia dos pulsos (P y TP)
#----------------------------------------------------------------#
#3.- tiempo evoluci?n de la diabetes mellitus
#----------------------------------------------------------------#
#4. -	antecedentes de ulceras
# -	antecedentes de amputaci?n
#----------------------------------------------------------------#




#----------------------------------------------------------------#
#i)-Tiempo evoluci?n de la diabetes mellitus==fConsenti2
#----------------------------------------------------------------#
PEU_CAT_CDRS<-PEU_CAT_CDRS%>%
  mutate(fConsenti2=as.numeric(substring(fConsenti,1,4)))
#PEU_CAT_CDRS$fConsenti2

PEU_CAT_CDRS<-PEU_CAT_CDRS%>%
  mutate(TiempDM=fConsenti2-anoDiagDm)
#PEU_CAT_CDRS$TiempDM

#------------------------------------------------------------------#

#----------------------------------------------------------------#
#ii)-Pacients Incluits(s/n)==PacInc, Pacients Excluits(s/n)==PacExc, PacIncExc(0	Pacients Excluidos 1:Pacientes Incluidos)
#----------------------------------------------------------------#
PEU_CAT_CDRS<-PEU_CAT_CDRS%>%
  mutate(PacInc=ifelse(inclu1==1 & inclu2==1 & inclu3==1 ,1,0))

PEU_CAT_CDRS<-PEU_CAT_CDRS%>%
  mutate(PacExc=ifelse(exclu1==1  | exclu2==1  | exclu3==1,1,0))

PEU_CAT_CDRS<-PEU_CAT_CDRS%>%
  mutate(PacIncExc=ifelse(PacInc==1 & PacExc==0 ,1,0))
#------------------------------------------------------------------#

#----------------------------------------------------------------#
#iii)-Monofilamento==(monoIzq,monoDer) 
# -	Normal: 4 puntos encontrados
# -	Patol?gico:  ??? 3 puntos
#----------------------------------------------------------------#
#------------------------------------------------------------------#
#monoDer  
PEU_CAT_CDRS<-PEU_CAT_CDRS%>%
  mutate(monoDer=case_when(monoDer==4 ~ 0,monoDer<=3  ~ 1,TRUE~ NA_real_))
#------------------------------------------------------------------#
#monoIzq    
PEU_CAT_CDRS<-PEU_CAT_CDRS%>%
  mutate(monoIzq=case_when(monoIzq==4 ~ 0,monoIzq<=3  ~ 1,TRUE~ NA_real_))
#------------------------------------------------------------------#


#------------------------------------------------------------------#
#iv)-Ipswith Touch Test==(ipsIzq,ipsDer)
# -	normal: 3 puntos
# -	patol?gico: ??? 2 puntos
#------------------------------------------------------------------#
#------------------------------------------------------------------#
#ipsDer   
PEU_CAT_CDRS<-PEU_CAT_CDRS%>%
  mutate(ipsDer=case_when(ipsDer==3 ~ 0,ipsDer<=2  ~ 1,TRUE~ NA_real_))
#------------------------------------------------------------------#
#ipsIzq
PEU_CAT_CDRS<-PEU_CAT_CDRS%>%
  mutate(ipsIzq=case_when(ipsIzq==3 ~ 0,ipsIzq<=2  ~ 1,TRUE~ NA_real_))
#------------------------------------------------------------------#



#------------------------------------------------------------------#
#v)-Monofilamento patol?gico + sensibilidad vibratoria ausente
#------------------------------------------------------------------#
#PEU_CAT_CDRS$sensVibDer
#PEU_CAT_CDRS$sensVibIzq
PEU_CAT_CDRS<-PEU_CAT_CDRS%>%mutate(monoPatSensAustente_Der=ifelse(monoDer==1 & sensVibDer==0  ,1,0))
PEU_CAT_CDRS<-PEU_CAT_CDRS%>%mutate(monoPatSensAustente_Izq=ifelse(monoIzq==1 & sensVibIzq==0  ,1,0))
#------------------------------------------------------------------#



#------------------------------------------------------------------#
#vi)-Sexo
#------------------------------------------------------------------#
#------------------------------------------------------------------#
# Hombre      [1]	
# Mujer       [2]	
#------------------------------------------------------------------#
PEU_CAT_CDRS<-PEU_CAT_CDRS%>%
  mutate(Sexo=case_when(sexoHombre==1 & sexoMujer==0 ~ 1,sexoHombre==0 & sexoMujer==1 ~ 2,TRUE~ NA_real_))
#------------------------------------------------------------------#


#------------------------------------------------------------------#
#vii)-Etnia
#------------------------------------------------------------------#
#------------------------------------------------------------------#
# etniaCau    [1]	
# etinaHisp   [2]	
# etniaIndos  [3]	
# etniaAfr    [4]	
# etniaOtr    [5]
#------------------------------------------------------------------#
PEU_CAT_CDRS<-PEU_CAT_CDRS%>%
  mutate(Etnia=case_when(etniaCau==1 & etinaHisp==0 & etniaIndos==0 & etniaAfr==0 & etniaOtr==0 ~ 1,
                         etniaCau==0 & etinaHisp==1 & etniaIndos==0 & etniaAfr==0 & etniaOtr==0 ~ 2,
                         etniaCau==0 & etinaHisp==0 & etniaIndos==1 & etniaAfr==0 & etniaOtr==0 ~ 3,
                         etniaCau==0 & etinaHisp==0 & etniaIndos==0 & etniaAfr==1 & etniaOtr==0 ~ 4,
                         etniaCau==0 & etinaHisp==0 & etniaIndos==0 & etniaAfr==0 & etniaOtr==1 ~ 5
                                                  ,TRUE~ NA_real_))
#------------------------------------------------------------------#


#------------------------------------------------------------------#
#viii)-Tabaco
#------------------------------------------------------------------#
#------------------------------------------------------------------#
#Tabaco:[]
#tabNo	      [1]
#tabacoSi	    [2]
#tabacoEx     [3]
#------------------------------------------------------------------#
PEU_CAT_CDRS<-PEU_CAT_CDRS%>%
  mutate(Tabaco=case_when(tabNo==1 & tabacoSi==0 & tabacoEx==0  ~ 1,
                          tabNo==0 & tabacoSi==1 & tabacoEx==0  ~ 2,
                          tabNo==0 & tabacoSi==0 & tabacoEx==1  ~ 3
                         ,TRUE~ NA_real_))


#------------------------------------------------------------------#
#ix)-Alcohol
#------------------------------------------------------------------#
#------------------------------------------------------------------#
#Alc:[]
#alcAbs[1]	
#alcBaj[2]	
#alcAlt[3]
#------------------------------------------------------------------#
PEU_CAT_CDRS<-PEU_CAT_CDRS%>%
  mutate(Alc=case_when(alcAbs==1 & alcBaj==0 & alcAlt==0  ~ 1,
                          alcAbs==0 & alcBaj==1 & alcAlt==0  ~ 2,
                          alcAbs==0 & alcBaj==0 & alcAlt==1  ~ 3
                          ,TRUE~ NA_real_))


#------------------------------------------------------------------#
#x)-Estado paciente
#------------------------------------------------------------------#
#------------------------------------------------------------------#
#EstPac:[]
#estPacInc	                            [Estado paciente.Incluido]
#estPacDisAb	                          [Estado paciente.Discontinuado por abandono]
#estPacDisFa	                          [Estado paciente.Discontinuado por fallecimiento]
#------------------------------------------------------------------#
PEU_CAT_CDRS<-PEU_CAT_CDRS%>%
  mutate(EstPac=case_when(estPacInc==1 & estPacDisAb==0 & estPacDisFa==0  ~ 1,
                          estPacInc==0 & estPacDisAb==1 & estPacDisFa==0  ~ 2,
                          estPacInc==0 & estPacDisAb==0 & estPacDisFa==1  ~ 3
                       ,TRUE~ NA_real_))


#------------------------------------------------------------------#
#xi)-Estado CRD
#------------------------------------------------------------------#
#------------------------------------------------------------------#
#EstCrd:[]
#estCrdCur	                            [Estado CRD.En curso]
#estCrdFin	                            [Estado CRD.Finalizado]
#------------------------------------------------------------------#
PEU_CAT_CDRS<-PEU_CAT_CDRS%>%
  mutate(EstCrd=case_when(estCrdCur==1 & estCrdFin==0  ~ 1,
                          estCrdCur==0 & estCrdFin==1   ~ 2
                                                    ,TRUE~ NA_real_))
#------------------------------------------------------------------#


#------------------------------------------------------------------#
#xii)-Arteriopatia perif?rica!
#------------------------------------------------------------------#
#
#tibPosDer[TPd]  	       [Tibial posterior pie derecho]
#tibPosIzq[TPi]	         [Tibial posterior pie izquierdo]
#pedioDer	[Pd]           [Pediopie derecho]
#pedioIzq	[Pi]           [Pediopie izquierdo]
#PEU_CAT_CDRS$tibPosDer
#PEU_CAT_CDRS$tibPosIzq	
#PEU_CAT_CDRS$pedioDer	
#PEU_CAT_CDRS$pedioIzq	


#table(PEU_CAT_CDRS$tibPosDer,useNA="always")
#table(PEU_CAT_CDRS$tibPosIzq,useNA="always")
#table(PEU_CAT_CDRS$pedioDer,useNA="always")
#table(PEU_CAT_CDRS$pedioIzq	,useNA="always")

PEU_CAT_CDRS<-PEU_CAT_CDRS%>%mutate(ArtPer_Der=tibPosDer+pedioDer)
PEU_CAT_CDRS<-PEU_CAT_CDRS%>%mutate(ArtPer_Izq=tibPosIzq+pedioIzq)

#table(PEU_CAT_CDRS$ArtPer_Izq	,useNA="always")

----------------------------------------------------------------#
#08.03.2019:[]
#names(dades)
#------------------------------------------------------------------#



#- [[antecedentes de ulceras   ]]
#- [[antecedentes de amputaci?n]]







#ok


#Antecedentes?lceras
#------------------------------------------------------------------#

PEU_CAT_CDRS_ulceras<-read_excel(fitxer_dades,sheet="Antecedentesulceras")

#PEU_CAT_CDRS_ulceras 

PEU_CAT_CDRS_ulceras2<-PEU_CAT_CDRS_ulceras%>%
  dplyr::group_by(idcrd) %>%
  dplyr::summarise(sum_latIzq = sum(latIzq),sum_latDer = sum(latDer))%>%
  ungroup 

PEU_CAT_CDRS_ulceras2<-PEU_CAT_CDRS_ulceras2%>%mutate(ulceras=sum_latIzq+sum_latDer)
PEU_CAT_CDRS_ulceras2<-PEU_CAT_CDRS_ulceras2%>%select(idcrd,ulceras)%>%arrange(idcrd) 
PEU_CAT_CDRS_ulceras2

#AntecedentesAmputaci?n
#------------------------------------------------------------------#

#PEU_CAT_CDRS_amputaciones2
PEU_CAT_CDRS_amputaciones<-read_excel(fitxer_dades,sheet="AntecedentesAmputaci?n")
#PEU_CAT_CDRS_amputaciones

PEU_CAT_CDRS_amputaciones2<-PEU_CAT_CDRS_amputaciones%>%
  dplyr::group_by(idcrd)%>%
  dplyr::summarize(sum_latIzq = sum(latIzq),sum_latDer = sum(latDer))%>% 
  ungroup 


PEU_CAT_CDRS_amputaciones2<-PEU_CAT_CDRS_amputaciones2%>%mutate(amputaciones=sum_latIzq+sum_latDer)
PEU_CAT_CDRS_amputaciones2
PEU_CAT_CDRS_amputaciones2<-PEU_CAT_CDRS_amputaciones2%>%select(idcrd,amputaciones)%>%arrange(idcrd) 



PEU_CAT_CDRS<-PEU_CAT_CDRS%>%left_join(PEU_CAT_CDRS_ulceras2, by="idcrd")
PEU_CAT_CDRS<-PEU_CAT_CDRS%>%left_join(PEU_CAT_CDRS_amputaciones2, by="idcrd")


names(PEU_CAT_CDRS)


#------------------------------------------------------------------#
conductor_variables<-"taulavariables_v2_PEU3.xls"
#------------------------------------------------------------------#
PEU_CAT_CDRS<-convertir_dates(d=PEU_CAT_CDRS,taulavariables=conductor_variables)
PEU_CAT_CDRS<-LAB_ETIQ_v2(dt=PEU_CAT_CDRS,variables_factors=conductor_variables,fulla="etiquetes",idioma="etiqueta2")
PEU_CAT_CDRS<-etiquetar(d=PEU_CAT_CDRS,taulavariables=conductor_variables)
#------------------------------------------------------------------#


#names(PEU_CAT_CDRS)



# TAULES 


#names(LAB_ETIQ_PEU_CAT)
#***********************************************************************#
#taula00 Criteris Inclusi?
formula_taula00<-formula_compare("taula00",y="",taulavariables = conductor_variables)
T00<-descrTable(formula_taula00,method = 2,data=PEU_CAT_CDRS)
#***********************************************************************#

T00
PEU_CAT_CDRS<-PEU_CAT_CDRS%>%filter(PacInc=="Si")
table(PEU_CAT_CDRS$PacInc)

#N=198.
#------------------------------------------------------------------#
#names(LAB_ETIQ_PEU_CAT)
#***********************************************************************#
#taula0 Criteris  Exclusi?.
formula_taula0<-formula_compare("taula0",y="",taulavariables = conductor_variables)
T0<-descrTable(formula_taula0,method = 2,data=PEU_CAT_CDRS)
#***********************************************************************#
T0
#flow-chart!:[]
table(PEU_CAT_CDRS$exclu1)
table(PEU_CAT_CDRS$exclu2)
table(PEU_CAT_CDRS$exclu3)



#N=198.
#diagramaFlowchart

#diagramaFlowchart

T13<-diagramaFlowchart(
  grups=1,
  pob_lab1=c("Pacientes con criterio de Inclusi?n ","Pacientes con criterio de Inclusi?n y sin criterios de Exclusion  "),
  pob1=c(198,191),
  exc1=c(2,1,7),
  exc_lab1=c('Pacientes diagnosticados de DM1/Diabetis Gestacional/Diabetis Secundaria a medicamentos','Pacientes con una esperanza de vida < 1 a?o','Ulceras activas a inicio del estudio'),
  colors=c('white','grey'),
  forma=c('box','box'))


T13

PEU_CAT_CDRS<-PEU_CAT_CDRS%>%filter(PacExc=="No")

#N=191


#Q1 = 0.025, Q3 = 0.975

#LAB_ETIQ_PEU_CAT$coAlCre

#taula1
formula_taula1<-formula_compare("taula1",y="",taulavariables = conductor_variables)
T1<-descrTable(formula_taula1,method = 2,Q1 = 0, Q3 =1 ,data=PEU_CAT_CDRS)
T1
#***********************************************************************#
#taula2
formula_taula2<-formula_compare("taula2",y="",taulavariables = conductor_variables)
T2<-descrTable(formula_taula2,method = 2,Q1 = 0, Q3 =1,data=PEU_CAT_CDRS)
T2
#***********************************************************************#
#taula3
formula_taula3<-formula_compare("taula3",y="",taulavariables = conductor_variables)
T3<-descrTable(formula_taula3,method = 2,Q1 = 0, Q3 =1,data=PEU_CAT_CDRS)
T3
formula_taula3Error<-formula_compare("Taula3Error",y="",taulavariables = conductor_variables)
T3Error<-descrTable(formula_taula3Error,method = 2,Q1 = 0, Q3 =1,data=PEU_CAT_CDRS)
T3Error


#[1.3.2019]

#Canvis per corregir els errors

#table(LAB_ETIQ_PEU_CAT$colTot)
#table(LAB_ETIQ_PEU_CAT$colTot2)

#http://www.sediabetes.org/calculadoras/calculadoras/conversor.aspx

#***********************************************************************#
#taula3
formula_taula3_Corregit<-formula_compare("Taula3Corregit",y="",taulavariables = conductor_variables)
T3corregit<-descrTable(formula_taula3_Corregit,method = 2,Q1 = 0, Q3 =1,data=PEU_CAT_CDRS)
T3corregit




#***********************************************************************#
#taula4
formula_taula4<-formula_compare("taula4",y="",taulavariables = conductor_variables)
T4<-descrTable(formula_taula4,method = 2,Q1 = 0, Q3 =1,data=PEU_CAT_CDRS)
T4
#***********************************************************************#


#errors:[]
#[noFarma][itbDer][itbIzq][motNoReal]

#LAB_ETIQ_PEU_CAT$noFarma
#LAB_ETIQ_PEU_CAT$itbDer
#LAB_ETIQ_PEU_CAT$itbIzq
#LAB_ETIQ_PEU_CAT$motNoReal
#---------------#
#T00
#T0
#T13
#T1
#T2
#T3
#T4
#---------------#

T13


save(T00,T0,T1,T2,T3,T3Error,T3corregit,T4,T13,file="PEUCAT.Rdata")






#1.- Monofilamento
#- Normal: 4 puntos encontrados
#- Patol?gico: ??? 3 puntos
#2.- Monofilamento patol?gico + sensibilidad vibratoria ausente
#3.- Ipswith Touch Test
#- normal: 3 puntos
#- patol?gico: ??? 2 puntos
#4.- Arteriopat?a perif?rica
#- presencia dos pulsos (P o TP)
#- ausencia un pulso (P o TP)
#- ausencia dos pulsos (P y TP)
#Falta tambi?n
#1.- antecedentes de HTA: si/no
#2.- antecedentes de dislipemia: si/no
#3.- tiempo evoluci?n de la diabetes mellitus
#4.- antecedentes personales de:
#  - neuropat?a perif?rica
#- neuropat?a aut?noma
#- insuficiencia renal
#- microalbuminuria o macroalbuminuria
#- actualmente en di?lisis
#- trasplante renal
#- neuroartropatia de Charcot
#- antecedentes de ulceras
#- antecedentes de amputaci?n






#-----------------------------------------------------------------------#
library(arsenal)
library(table1)
#taula1
#-----------------------------------------------------------------------#
T1_b<-table1::table1(formula_taula1, data = PEU_CAT_CDRS)
T1_b

#-----------------------------------------------------------------------#
#taula2
T2_b<-table1::table1(formula_taula2, data = PEU_CAT_CDRS)
T2_b
#-----------------------------------------------------------------------#
#taula3
T3_b<-table1::table1(formula_taula3_Corregit, data = PEU_CAT_CDRS)
T3_b
#-----------------------------------------------------------------------#
#taula4
T4_b<-table1::table1(formula_taula4, data = PEU_CAT_CDRS)
T4_b
#-----------------------------------------------------------------------#
T1_b
T2_b
T3_b
T4_b
#-----------------------------------------------------------------------#
T4






