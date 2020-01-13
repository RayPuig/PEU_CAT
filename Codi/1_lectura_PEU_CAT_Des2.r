#------------------------------------------------------------------------------------------#
#[13.01.2020]

#------------------------------------------------------------------------------------------#
#10.01.2020
#------------------------------------------------------------------------------------------#
#[31.12.2019]
#------------------------------------------------------------------------------------------#
#[30.12.2019]
#------------------------------------------------------------------------------------------#
#[27.12.2019]    
#------------------------------------------------------------------------------------------#
#[24.12.2019]
#(ACTUALITZACIONS)
#
#------------------------------------------------------------------------------------------#
#                        1. ETIQUETAR VARIABLES! []
#------------------------------------------------------------------------------------------#
###
#

#etiquetar<-function(d=dadestotal,taulavariables="variables_R.xls") {
#  
#  ####  Llegir etiquetes i variables a analitzar ####
#  variables <- readxl::read_excel(taulavariables)
#  variables[is.na(variables)]<- 0
#  #
#  #
#  ###################################   etiquetar variables         
#  seleccio<-variables
#  camp<- as.vector(seleccio$camp) #
#  descripcio<- as.vector(seleccio$descripcio) #
#  ### etiquetar variables seleccionades     #
#  for (i in 2:length(descripcio)){if (any(colnames(d) == camp[i])) {Hmisc::label(d[[camp[i]]]) <- descripcio[i]}}
#  d
#}

#------------------------------------------------------------------------------------------#
#                        2. CONVERTIR DATES![de numeric a codo Data!]
#------------------------------------------------------------------------------------------#
#------------------------------------------------------------------#

#convertir_dates<-function(d=dadestotal,taulavariables="variables_R.xls")
#  
#{
#  ####  Llegir etiquetes i variables a analitzar ####
#  variables <- readxl::read_excel(taulavariables)
#  variables[is.na(variables)]<- 0
#  #
#  #
#  ###################################   etiquetar variables         
#  seleccio<-variables
#  camp<- as.vector(seleccio$camp) #
#  # dates<-as.vector(seleccio$dates)
#  # dates<-seleccio[[campdata]]
#  ### etiquetar variables seleccionades     #
#  
#  for (i in 1:length(camp)){if (seleccio$dates[i]==1) { 
#    
#    pepito<-paste0("as.Date(d[[camp[",i,"]]], '%Y%d%m')")
#    
#    d[[camp[i]]]<-eval(parse(text=pepito))
#    
#  } }
#  
#  d
# }
#------------------------------------------------------------------#
#------------------------------------------------------------------#
#LAB_ETIQ_v2<-function(dt=dades,variables_factors=conductor_variables,fulla="etiquetes",idioma="etiqueta1"){
#  
#  #dt=dades
#  #variables_factors=conductor_variables
#  
#  
#  #------------------------------------------------------------------#
#  variables_factors<-readxl::read_excel(variables_factors,sheet=fulla)
#  #------------------------------------------------------------------#
#  
#  
#  
#  if (idioma=="etiqueta1") {
#    
#    k<-variables_factors%>%select(camp, valor,etiqueta1)
#    
#  } else if (idioma=="etiqueta2") {
#    
#    k<-variables_factors%>%select(camp, valor,etiqueta2)
#    k<-k%>%mutate(etiqueta1=etiqueta2)
#    k<-k%>%select(camp, valor,etiqueta1)
#    
#  }
#  
#  
#  #------------------------------------------------------------------#
#  pepe<-k %>% split(list(.$camp))
#  #------------------------------------------------------------------#
#  #
#  noms_variables<-names(pepe)
#  num_vars<-length(noms_variables)
#  
#  for (i in 1:num_vars) {
#    
#    dt[noms_variables[i]]<-lapply(dt[noms_variables[i]],function(y) factor(y,levels=pepe[[i]]$valor,labels=pepe[[i]]$etiqueta1))
#    
#  }
#  
#  dt}
##############




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
library(lubridate)

#--------------------------------------------------------------------------#
link_source<-paste0("https://github.com/jrealgatius/Stat_codis/blob/master/funcions_propies.R","?raw=T")
devtools::source_url(link_source)
#--------------------------------------------------------------------------#


#[actual! ]
#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#
#idcrd
#Estado_paciente_Incluido
#Estado_paciente_Discontinuado_abandono
#Estado_paciente_Discontinuado_fallecimiento
#Estado_Crd_En_Curso
#Estado_Crd_Finalizado
#idinvestigador
#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#
#Fecha_consentimiento_informado
#Codigo_paciente
#CIP
#Criterios_inclusion_1
#Criterios_inclusion_2
#Criterios_inclusion_3
#Criterios_exclusion_1
#Criterios_exclusion_2
#Criterios_exclusion_3
#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#
#Fecha_nacimiento
#Edad
#Sexo_Hombre
#Sexo_Mujer
#Etnia_Caucasica
#Etnia_Hispanoamericana
#Etnia_Indostani
#Etnia_Africana
#Etnia_Otras
#Especificar_etnia
#Tabaquismo_No
#Tabaquismo_Si
#Tabaquismo_Exfumador
#Cigarrillos_dia
#Anos_tabaquismo
#Paquetes_Ano
#Alcohol_Abstemio
#Alcohol_Bajo_Riesgo
#Alcohol_Riesgo_elevado
#Antecedentes_de_consumo_excesivo
#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#
#PA_sistolica_1
#Pa_diastolica_1
#PA_sistolica_2
#PA_diastolica_2
#MEDIA_PAS
#MEDIA_PAD
#Diagnostico_HTA
#Diagnostico_dislipemia
#AVC
#AVC_ano
#Cardiopatia_isquemica
#Cardiopatia_isquemica_ano
#Arteriopatia_periferica
#Arteriopatia_periferica_ano
#Antecedentes_por_intervención_de_arteriopatia_periferica
#Antecedentes_por_intervencion_de_arteriopatia_periferica_ano
#Antecedentes_personales_de_insuficiencia_cardiaca
#Antecedentes_personales_de_insuficiencia_cardiaca_ano
#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#
#Diagnostico_en_la_visita_basal
#Ano_de_diagnostico_definitivo_de_DM
#TratamientoDM2_Sin_tratamiento
#TratamientoDM2_Metformina
#TratamientoDM2_SU
#TratamientoDM2_Pioglitazona
#TratamientoDM2_Repaglinida
#TratamientoDM2_iDPP4
#TratamientoDM2_iSGLT2
#TratamientoDM2_Analogos_GLP1
#TratamientoDM2_Insulina
#TratamientoDM2_αglucosidasa
#TratamientoDM2_Otros
#TratamientoDM2_Explicacion_Otros
#Tratamiento_diabetes_no_farmacologico
#Tratamiento_diabetes_no_farmacologico_dieta_y_ejercicios
#Retinopatia
#Retinopatia_ano
#Neuropatia_periferica
#Neuropatia_periferica_ano
#Neuropatia_autonoma
#Neuropatia_autonoma_ano
#Insuficiencia_renal_cronica
#Insuficiencia_renal_cronica_ano
#Microalbuminuria
#Microalbuminuria_ano
#Macroalbuminuria_o_proteinuria_24_horas
#Macroalbuminuria_o_proteinuria_24_horas_ano
#Actualmente_en_dialisis
#Actualmente_en_dialisis_ano
#Transplante_renal
#Transplante_renal_ano
#Neuroartropatia_de_Charcot
#Neuroartropatia_de_Charcot_ano
#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#
#Fecha_analitica
#HBA1C
#Colesterol_total
#Unidad_Colesterol_total
#HDL
#Unidad_HDL
#LDL
#Unidad_LDL
#Trigliceridos
#Unidad_Trigliceridos
#Creatinina
#Unidad_Creatinina
#Filtrado_glomerular_estimado
#Unidad_Filtrado_glomerular_estimado
#Cociente_Albumina_Creatinina_o_proteinuria
#Unidad_Cociente_Albumina_Creatinina_o_proteinuria
#Fecha_exploracion
#Peso
#Talla
#IMC
#Disminucion_de_la_agudeza_visual
#Problemas_con_la_movilidad
#Tiene_cuidador
#Visita_al_podologo_en_el_ultimo_ano
#Calzado_correcto
#Hidratacion_correcta
#Espacios_interdigitales_con_lesiones_micoticas
#Patologia_ungueal
#Cuidado_de_las_unas_correcto
#Hiperqueratosis_callosidades
#Edema_maleolar
#Otros137
#Explicacion_otros
#Deformidades
#Dedos_en_garra_martillo
#Halluxvalgus
#Pie_plano_cavo
#Sobrecarga_metatarsianos
#Charcot
#Otras145
#Tibial_posterior_pie_derecho
#Tibial_posterior_pie_izquierdo
#Pediopie_derecho
#Pediopie_izquierdo
#Es_valorable
#Fecha_de_ITB
#Derecho
#Izquierdo
#Motivo_no_realizacion
#Neuropatia_periferica_valorable
#Monofilamento_pie_derecho
#Monofilamento_pie_izquierdo
#Sensibilidad_vibratoria_pie_derecho
#Sensibilidad_vibratoria_pie_izquierdo
#Ipswitch_Touch_Test_derecho
#Ipswitch_Touch_Test_izquierdo
#Motivo_no_realizacion



#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#
#[abans! ]
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
#codPac	                                [Codigo paciente]
#cip	                                  [CIP]
#inclu1	                                [Criterios inclusion 1]
#inclu2	                                [Criterios inclusion 2]
#inclu3	                                [Criterios inclusion 3]
#exclu1	                                [Criterios exclusion 1]
#exclu2	                                [Criterios exclusion 2]
#exclu3	                                [Criterios exclusion 3]
#--------------------------------------------------------------------------------#
#                 Base de datos CRDS GENERAL                                     #
#--------------------------------------------------------------------------------#
#fNac	                                  [Fecha nacimiento]
        #i--> Demografiques: edat i sexe
#edad	                                  [Edad]
#sexoHombre	                            [Sexo Hombre]
#sexoMujer	                            [Sexo Mujer]
#etniaCau	                              [Etnia Caucasica]
#etinaHisp	                            [Etnia Hispanoamericana]
#etniaIndos	                            [Etnia Indostani]
#etniaAfr	                              [Etnia Africana]
#etniaOtr	                              [Etnia Otras]
#espEtnia	                              [Especificar etnia]
      #ii--> Habits toxics: tabac i alcohol
#tabNo	                                [Tabaco No fumador]
#tabacoSi	                              [Tabaco Fumador]
#tabacoEx	                              [Tabaco Exfumador]
#cigaDia	                              [Cigarrillos/dia]
#anoTab	                                [Años tabaquismo]
#paqTab	                                [Paquetes Año]
#alcAbs	                                [Alcohol Abstemio]
#alcBaj	                                [Alcohol Bajo Riesgo]
#alcAlt	                                [Alcohol Alto Riesgo]
#antAlcohol	                            [Antecedentes de consumo excesivo]
      #iii-->Factors de risc cardiovascular:[antecedents personals d'hipertensio areterial i dislipemia]
#paSis1	                                [PA sist?lica 1]
#paDia2	                                [Pa diast?lica 1]
#paSis2	                                [PA sist?lica 2]
#paDia2	                                [PA diast?lica 2]
#medPs	                                [MEDIA PAS]
#medPd	                                [MEDIA PAD]
#diaHta	                                [Diagn?stico HTA]
#diaDis	                                [Diagn?stico dislipemia]
      #iv-->Antecedents personals de malaltia cardiovascular:AVC,cardiopatia isquemica,arteriopatia periferica
#avc	                                  [AVC]
#acvAno	                                [AVC año]
#carIsq	                                [Cardiopatia isquemica]
#carIsqAno	                            [Cardiopatia isquemica año]
#artPer	                                [Arteriopatia periferica]
#artPerAno	                            [Arteriopatia periferica año]
#antArt	                                [Antecedentes por intervencion de arteriopatia periferica]
#antArtAno	                            [Antecedentes por intervencion de arteriopatia periferica año]
      #v-->Antecedents personals d'insuficiencia cardiaca
#antInsCar	                            [Antecedentes personales de insuficiencia cardiaca]
#antInsCarA	                            [Antecedentes personales de insuficiencia cardiaca año]
#--------------------------------------------------------------------------------#
      #vi-->En relacio a la DM2
#dVisBa	                                [Diagnostico en la visita basal]
#anoDiagDm	                            [Año de diagnostico definitivo de DM]
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
#Reti	                                  [Retinopatia]
#retiAno	                              [Retinopatia año]
#NeuPer	                                [Neuropatia periferica]
#NeuPerAno	                            [Neuropatia periferica año]
#NeuAut	                                [Neuropatia autonoma]
#NeuAutAno	                            [Neuropatia autonoma año]
#inReCr	                                [Insuficiencia renal cronica]
#inReCrAno	                            [Insuficiencia renal cronica año]
#microA	                                [Microalbuminuria]
#microAAno	                            [Microalbuminuria año]
#macro	                                [Macroalbuminuria o proteinuria/24 horas]
#macroAno	                              [Macroalbuminuria o proteinuria/24 horas año]
#actDial	                              [Actualmente en dialisis]
#actDialAno	                            [Actualmente en dialisis año]
#traRen	                                [Transplante renal]
#traRenAno	                            [Transplante renal año]
#neuCh	                                [Neuroartropatia de Charcot]
#neuChAno	                              [Neuroartropatia de Charcot año]
#--------------------------------------------------------------------------------#
        #vii-->Laboratori
#fAna	                                  [Fecha analitica]
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
#coAlCre	                              [Cociente Albumina/Creatinina o proteinuria]
#unCoAlCre	                            [Unidad Cociente Albumina/Creatinina o proteinuria]
#fExp	                                  [Fecha exploracion]
        #viii-->Pes i Talla
#peso	                                  [Peso]
#Talla	                                [Talla]
#Imc	                                  [IMC]
        #ix-->Valoracio de l'autocura,presencia de cuidador,visita al podoleg en el darrer any
#disAgVis                             	[Disminucion de la agudeza visual]
#prMovil	                              [Problemas con la movilidad]
#cuidador	                              [Tiene cuidador]
#visPodAno	                            [Visita al podologo en el ultimo año]
        #x--> inspeccio Peus
#calCorrec	                            [Calzado correcto]
#hidCorrec	                            [Hidratacion correcta]
#espIntLes	                            [Espacios interdigitales con lesiones mic ticas]
#patUng	                                [Patologia ungueal]
#cuiU?aCor	                            [Cuidado de las uñas correcto]
#callosi	                              [Hiperqueratosis/callosidades]
#edemaMal	                              [Edema maleolar]
#otrosExp	                              [Otros]
#expOtroExp	                            [Explicacion otros]
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
#valorable	                            [Es valorable?]
#fITB	                                  [Fecha de ITB]
#itbDer	                                [Derecho]
#itbIzq	                                [Izquierdo]
#motNoReal	                            [Motivo no realizacion]
#NeuPerVal	                            [Neuropatia periferica Es valorable?]
#monoDer	                              [Monofilamento pie derecho]
#monoIzq	                              [Monofilamento pie izquierdo]
#sensVibDer	                            [Sensibilidad vibratoria pie derecho]
#sensVibIzq	                            [Sensibilidad vibratoria pie izquierdo]
#ipsDer	                                [Ipswitch Touch Test derecho]
#ipsIzq	                                [Ipswitch Touch Test izquierdo]
#ipsNoReal	                            [Motivo no realizacion]
#--------------------------------------------------------------------------------#
#fitxer_dades<-"exportacion22112019_Dep_Magda2.xls"

fitxer_dades<-"exportacion22112019_Dep_Magda3_bog.xls"

#----------------------------------------------------#
#read_excel("Dades/exportacion22112019_Dep_Magda2.xls",sheet="exportacionCRDS")
#read_excel(paste0("Dades/","exportacion22112019_Dep_Magda2.xls"),sheet="exportacionCRDS")
#read_excel(paste0("Dades/",fitxer_dades),sheet="exportacionCRDS")
#read_excel(paste0("Dades/",fitxer_dades),sheet="exportacionCRDS")
#PEU_CAT_CDRS<-read_excel(fitxer_dades,sheet="exportacionCRDS")
PEU_CAT_CDRS<-read_excel(here::here("Dades",fitxer_dades),sheet="exportacionCRDS")
#----------------------------------------------------#

variable.names(PEU_CAT_CDRS)

########################################################################
#[Transforamcions de les variables!]: [R E C O D I F I C A C I O N S] 
########################################################################
#############################################################################
# 1.- Monofilamento
# -	Normal: 4 puntos encontrados
# -	Patologico:  ??? 3 puntos
# 2.- Monofilamento patologico + sensibilidad vibratoria ausente
# 3.- Ipswith Touch Test
# -	normal: 3 puntos
# -	patologico: ??? 2 puntos
# 4.- Arteriopatia periferica
# -	presencia dos pulsos (P o TP)
# -	ausencia un pulso (P o TP)
# -	ausencia dos pulsos (P y TP)
# Falta tambien
# 1.- antecedentes de HTA: si/no
# 2.- antecedentes de dislipemia: si/no
# 3.- tiempo evolucion de la diabetes mellitus
# 4.- antecedentes personales de:
#   -	neuropatia periferica
#   -	neuropatia autonoma
#   -	insuficiencia renal
#   -	microalbuminuria o macroalbuminuria
#   -	actualmente en dialisis
#   -	trasplante renal
#   -	neuroartropatia de Charcot
#   -	antecedentes de ulceras
#   -	antecedentes de amputacion
#############################################################################
#***********************************************************************#
#Tots els pacients DIABETIS TIPUS 2, atesos als centres de salut i que participin en l'estudi.
#S'inclouran en l'estudi tots els pacients que presentin una Ulcera en un peu durant el periode de recollida de dades,
#i COMPLEIXIN TOTS ELS CRITERIS D'INCLUSIO I CAP EXLUSIO.
#***********************************************************************#





#----------------------------------------------------------------#
#2.- Arteriopatia periferica
#       -	presencia dos pulsos (P o TP)
#       -	ausencia un pulso (P o TP)
#       -	ausencia dos pulsos (P y TP)
#----------------------------------------------------------------#
#3.- tiempo evolucion de la diabetes mellitus
#----------------------------------------------------------------#
#4. -	antecedentes de ulceras
# -	antecedentes de amputacion
#----------------------------------------------------------------#

#----------------------------------------------------------------#
#i)-Tiempo evolucion de la diabetes mellitus==fConsenti2
#----------------------------------------------------------------#
PEU_CAT_CDRS<-PEU_CAT_CDRS %>% 
  mutate(Fecha_consentimiento_informado=data_convert_text(Fecha_consentimiento_informado)) 
#----------------------------------------------------------------#
PEU_CAT_CDRS<-PEU_CAT_CDRS %>%
  mutate(Ano_consentimiento_informado=as.numeric(format(PEU_CAT_CDRS$Fecha_consentimiento_informado,'%Y'))) 
#----------------------------------------------------------------#  
PEU_CAT_CDRS<-PEU_CAT_CDRS%>%
  mutate(TiempDM=(Ano_consentimiento_informado) -Ano_de_diagnostico_definitivo_de_DM)
#----------------------------------------------------------------#
#PEU_CAT_CDRS$Ano_de_diagnostico_definitivo_de_DM
#PEU_CAT_CDRS$Ano_consentimiento_informado
#PEU_CAT_CDRS$TiempDM
#----------------------------------------------------------------#

#------------------------------------------------------------------#
#PEU_CAT_CDRS<-PEU_CAT_CDRS%>%mutate(fConsenti2=as.numeric(substring(fConsenti,1,4)))
#PEU_CAT_CDRS$fConsenti2
#------------------------------------------------------------------#
#PEU_CAT_CDRS<-PEU_CAT_CDRS%>%mutate(TiempDM=fConsenti2-anoDiagDm)
#PEU_CAT_CDRS$TiempDM
#------------------------------------------------------------------#



#----------------------------------------------------------------#
#ii)

# Pacients Incluits(s/n)==PacInc, 
# Pacients Excluits(s/n)==PacExc, 
# PacIncExc(0	Pacients Excluidos 1:Pacientes Incluidos)

#----------------------------------------------------------------#

#Criterios_inclusion_1
#Criterios_inclusion_2
#Criterios_inclusion_3
#.....................#
#Criterios_exclusion_1
#Criterios_exclusion_2
#Criterios_exclusion_3

#   x | y	 OR
#   x & y	 AND 


PEU_CAT_CDRS<-PEU_CAT_CDRS%>%
  mutate(PacInc=ifelse(Criterios_inclusion_1==1 &
                         Criterios_inclusion_2==1 & 
                          Criterios_inclusion_3==1 ,1,0))

PEU_CAT_CDRS<-PEU_CAT_CDRS%>%
  mutate(PacExc=ifelse(Criterios_exclusion_1==1  | 
                         Criterios_exclusion_2==1  | 
                          Criterios_exclusion_3==1,1,0))

PEU_CAT_CDRS<-PEU_CAT_CDRS%>%
  mutate(PacIncExc=ifelse(PacInc==1 & 
                            PacExc==0 ,1,0))



#PEU_CAT_CDRS<-PEU_CAT_CDRS%>%
#  mutate(PacInc=ifelse(inclu1==1 & inclu2==1 & inclu3==1 ,1,0))

#PEU_CAT_CDRS<-PEU_CAT_CDRS%>%
#  mutate(PacExc=ifelse(exclu1==1  | exclu2==1  | exclu3==1,1,0))

#PEU_CAT_CDRS<-PEU_CAT_CDRS%>%
#  mutate(PacIncExc=ifelse(PacInc==1 & PacExc==0 ,1,0))

#------------------------------------------------------------------#



#----------------------------------------------------------------#
#iii)

#Monofilamento_pie_derecho
#Monofilamento_pie_izquierdo

#-Monofilamento==(monoIzq,monoDer) 
# -	Normal: 4 puntos encontrados
# -	PatolOgico:  ??? 3 puntos
#----------------------------------------------------------------#




#------------------------------------------------------------------#
#monoDer  
PEU_CAT_CDRS<-PEU_CAT_CDRS%>%
  mutate(Monofilamento_pie_derecho=case_when(Monofilamento_pie_derecho==4 ~ 0,Monofilamento_pie_derecho<=3  ~ 1,TRUE~ NA_real_))
#------------------------------------------------------------------#
#monoIzq    
PEU_CAT_CDRS<-PEU_CAT_CDRS%>%
  mutate(Monofilamento_pie_izquierdo=case_when(Monofilamento_pie_izquierdo==4 ~ 0,Monofilamento_pie_izquierdo<=3  ~ 1,TRUE~ NA_real_))
#------------------------------------------------------------------#



#------------------------------------------------------------------#
#monoDer  
#PEU_CAT_CDRS<-PEU_CAT_CDRS%>%
#  mutate(monoDer=case_when(monoDer==4 ~ 0,monoDer<=3  ~ 1,TRUE~ NA_real_))
#------------------------------------------------------------------#
#monoIzq    
#PEU_CAT_CDRS<-PEU_CAT_CDRS%>%
#  mutate(monoIzq=case_when(monoIzq==4 ~ 0,monoIzq<=3  ~ 1,TRUE~ NA_real_))
#------------------------------------------------------------------#




#------------------------------------------------------------------#
#iv)-Ipswith Touch Test==(ipsIzq,ipsDer)
# -	normal: 3 puntos
# -	patol?gico: ??? 2 puntos
#------------------------------------------------------------------#





#------------------------------------------------------------------#
#ipsDer   
PEU_CAT_CDRS<-PEU_CAT_CDRS%>%
  mutate(Ipswitch_Touch_Test_derecho=case_when(Ipswitch_Touch_Test_derecho==3 ~ 0,Ipswitch_Touch_Test_derecho<=2  ~ 1,TRUE~ NA_real_))
#------------------------------------------------------------------#
#ipsIzq
PEU_CAT_CDRS<-PEU_CAT_CDRS%>%
  mutate(Ipswitch_Touch_Test_izquierdo=case_when(Ipswitch_Touch_Test_izquierdo==3 ~ 0,Ipswitch_Touch_Test_izquierdo<=2  ~ 1,TRUE~ NA_real_))
#------------------------------------------------------------------#

#PEU_CAT_CDRS$Ipswitch_Touch_Test_izquierdo


#------------------------------------------------------------------#
#ipsDer   
#PEU_CAT_CDRS<-PEU_CAT_CDRS%>%
#  mutate(ipsDer=case_when(ipsDer==3 ~ 0,ipsDer<=2  ~ 1,TRUE~ NA_real_))
#------------------------------------------------------------------#
#ipsIzq
#PEU_CAT_CDRS<-PEU_CAT_CDRS%>%
#  mutate(ipsIzq=case_when(ipsIzq==3 ~ 0,ipsIzq<=2  ~ 1,TRUE~ NA_real_))
#------------------------------------------------------------------#



#------------------------------------------------------------------#
#v)-Monofilamento patologico + sensibilidad vibratoria ausente
#------------------------------------------------------------------#
#PEU_CAT_CDRS$sensVibDer
#PEU_CAT_CDRS$sensVibIzq

PEU_CAT_CDRS<-PEU_CAT_CDRS%>%
  mutate(monoPatSensAustente_Der=ifelse(Monofilamento_pie_derecho==1 & Sensibilidad_vibratoria_pie_derecho==0,1,0))

PEU_CAT_CDRS<-PEU_CAT_CDRS%>%
  mutate(monoPatSensAustente_Izq=ifelse(Monofilamento_pie_izquierdo==1 & Sensibilidad_vibratoria_pie_izquierdo==0,1,0))
#------------------------------------------------------------------#

#PEU_CAT_CDRS<-PEU_CAT_CDRS%>%mutate(monoPatSensAustente_Der=ifelse(monoDer==1 & sensVibDer==0  ,1,0))
#PEU_CAT_CDRS<-PEU_CAT_CDRS%>%mutate(monoPatSensAustente_Izq=ifelse(monoIzq==1 & sensVibIzq==0  ,1,0))
#------------------------------------------------------------------#



#------------------------------------------------------------------#
#vi)-Sexo
#------------------------------------------------------------------#
#------------------------------------------------------------------#
# Hombre      [1]	
# Mujer       [2]	

#Sexo.Hombre
#Sexo.Mujer

#------------------------------------------------------------------#
PEU_CAT_CDRS<-PEU_CAT_CDRS%>%mutate(Sexo=case_when(Sexo_Hombre==1 & Sexo_Mujer==0 ~ 1,Sexo_Hombre==0 & Sexo_Mujer==1 ~ 2,TRUE~ NA_real_))
#------------------------------------------------------------------#
#PEU_CAT_CDRS$Sexo
#------------------------------------------------------------------#
#PEU_CAT_CDRS<-PEU_CAT_CDRS%>%
#  mutate(Sexo=case_when(sexoHombre==1 & sexoMujer==0 ~ 1,sexoHombre==0 & sexoMujer==1 ~ 2,TRUE~ NA_real_))
#------------------------------------------------------------------#


#------------------------------------------------------------------#
#vii)-Etnia
#------------------------------------------------------------------#
#Etnia.Caucásica
#Etnia.Hispanoamericana
#Etnia.Indostaní
#Etnia.Africana
#Etnia.Otras
#Especificar etnia
#------------------------------------------------------------------#
# etniaCau    [1]	
# etinaHisp   [2]	
# etniaIndos  [3]	
# etniaAfr    [4]	
# etniaOtr    [5]
#------------------------------------------------------------------#


PEU_CAT_CDRS<-PEU_CAT_CDRS%>%
  mutate(Etnia=case_when(Etnia_Caucasica==1 & Etnia_Hispanoamericana==0 & Etnia_Indostani==0 & Etnia_Africana==0 & Etnia_Otras==0 ~ 1,
                         Etnia_Caucasica==0 & Etnia_Hispanoamericana==1 & Etnia_Indostani==0 & Etnia_Africana==0 & Etnia_Otras==0 ~ 2,
                         Etnia_Caucasica==0 & Etnia_Hispanoamericana==0 & Etnia_Indostani==1 & Etnia_Africana==0 & Etnia_Otras==0 ~ 3,
                         Etnia_Caucasica==0 & Etnia_Hispanoamericana==0 & Etnia_Indostani==0 & Etnia_Africana==1 & Etnia_Otras==0 ~ 4,
                         Etnia_Caucasica==0 & Etnia_Hispanoamericana==0 & Etnia_Indostani==0 & Etnia_Africana==0 & Etnia_Otras==1 ~ 5
                         ,TRUE~ NA_real_))
#-----------


#PEU_CAT_CDRS<-PEU_CAT_CDRS%>%
#  mutate(Etnia=case_when(etniaCau==1 & etinaHisp==0 & etniaIndos==0 & etniaAfr==0 & etniaOtr==0 ~ 1,
#                         etniaCau==0 & etinaHisp==1 & etniaIndos==0 & etniaAfr==0 & etniaOtr==0 ~ 2,
#                         etniaCau==0 & etinaHisp==0 & etniaIndos==1 & etniaAfr==0 & etniaOtr==0 ~ 3,
#                         etniaCau==0 & etinaHisp==0 & etniaIndos==0 & etniaAfr==1 & etniaOtr==0 ~ 4,
#                         etniaCau==0 & etinaHisp==0 & etniaIndos==0 & etniaAfr==0 & etniaOtr==1 ~ 5
#                                                  ,TRUE~ NA_real_))
#------------------------------------------------------------------#


#------------------------------------------------------------------#
#viii)-Tabaco
#------------------------------------------------------------------#
#Tabaquismo.No
#Tabaquismo.Si
#Tabaquismo.Exfumador

PEU_CAT_CDRS<-PEU_CAT_CDRS%>%
  mutate(Tabaco=case_when(Tabaquismo_No==1 & Tabaquismo_Si==0 & Tabaquismo_Exfumador==0  ~ 1,
                          Tabaquismo_No==0 & Tabaquismo_Si==1 & Tabaquismo_Exfumador==0  ~ 2,
                          Tabaquismo_No==0 & Tabaquismo_Si==0 & Tabaquismo_Exfumador==1  ~ 3
                          ,TRUE~ NA_real_))

#------------------------------------------------------------------#
#Tabaco:[]
#tabNo	      [1]
#tabacoSi	    [2]
#tabacoEx     [3]
#------------------------------------------------------------------#
#PEU_CAT_CDRS<-PEU_CAT_CDRS%>%
#  mutate(Tabaco=case_when(tabNo==1 & tabacoSi==0 & tabacoEx==0  ~ 1,
#                          tabNo==0 & tabacoSi==1 & tabacoEx==0  ~ 2,
#                          tabNo==0 & tabacoSi==0 & tabacoEx==1  ~ 3
#                         ,TRUE~ NA_real_))


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
  mutate(Alc=case_when(Alcohol_Abstemio==1 & Alcohol_Bajo_Riesgo==0 & Alcohol_Riesgo_elevado==0  ~ 1,
                       Alcohol_Abstemio==0 & Alcohol_Bajo_Riesgo==1 & Alcohol_Riesgo_elevado==0  ~ 2,
                       Alcohol_Abstemio==0 & Alcohol_Bajo_Riesgo==0 & Alcohol_Riesgo_elevado==1  ~ 3
                       ,TRUE~ NA_real_))
#------------------------------------------------------------------#
#PEU_CAT_CDRS<-PEU_CAT_CDRS%>%
#  mutate(Alc=case_when(alcAbs==1 & alcBaj==0 & alcAlt==0  ~ 1,
#                          alcAbs==0 & alcBaj==1 & alcAlt==0  ~ 2,
#                          alcAbs==0 & alcBaj==0 & alcAlt==1  ~ 3
#                          ,TRUE~ NA_real_))
#------------------------------------------------------------------#



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
  mutate(EstPac=case_when(Estado_paciente_Incluido==1 & Estado_paciente_Discontinuado_abandono==0 & Estado_paciente_Discontinuado_fallecimiento==0  ~ 1,
                          Estado_paciente_Incluido==0 & Estado_paciente_Discontinuado_abandono==1 & Estado_paciente_Discontinuado_fallecimiento==0  ~ 2,
                          Estado_paciente_Incluido==0 & Estado_paciente_Discontinuado_abandono==0 & Estado_paciente_Discontinuado_fallecimiento==1  ~ 3
                       ,TRUE~ NA_real_))


#PEU_CAT_CDRS<-PEU_CAT_CDRS%>%
#  mutate(EstPac=case_when(estPacInc==1 & estPacDisAb==0 & estPacDisFa==0  ~ 1,
#                          estPacInc==0 & estPacDisAb==1 & estPacDisFa==0  ~ 2,
#                          estPacInc==0 & estPacDisAb==0 & estPacDisFa==1  ~ 3
#                          ,TRUE~ NA_real_))

#------------------------------------------------------------------#
#xi)-Estado CRD
#------------------------------------------------------------------#
#------------------------------------------------------------------#
#EstCrd:[]
#estCrdCur	                            [Estado CRD.En curso]
#estCrdFin	                            [Estado CRD.Finalizado]
#------------------------------------------------------------------#
PEU_CAT_CDRS<-PEU_CAT_CDRS%>%
  mutate(EstCrd=case_when(Estado_Crd_En_Curso==1 & Estado_Crd_Finalizado==0  ~ 1,
                          Estado_Crd_En_Curso==0 & Estado_Crd_Finalizado==1   ~ 2
                          ,TRUE~ NA_real_))
#------------------------------------------------------------------#
#PEU_CAT_CDRS<-PEU_CAT_CDRS%>%
#  mutate(EstCrd=case_when(estCrdCur==1 & estCrdFin==0  ~ 1,
#                          estCrdCur==0 & estCrdFin==1   ~ 2
#                                                    ,TRUE~ NA_real_))
#------------------------------------------------------------------#


#------------------------------------------------------------------#
#xii)-Arteriopatia periferica!
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

PEU_CAT_CDRS<-PEU_CAT_CDRS%>%mutate(ArtPer_Der=Tibial_posterior_pie_derecho+Pediopie_derecho)
PEU_CAT_CDRS<-PEU_CAT_CDRS%>%mutate(ArtPer_Izq=Tibial_posterior_pie_izquierdo+Pediopie_izquierdo)


#PEU_CAT_CDRS<-PEU_CAT_CDRS%>%mutate(ArtPer_Der=tibPosDer+pedioDer)
#PEU_CAT_CDRS<-PEU_CAT_CDRS%>%mutate(ArtPer_Izq=tibPosIzq+pedioIzq)


#----------------------------------------------------------------#
#08.03.2019:[]
#names(dades)
#------------------------------------------------------------------#


  
#------------------------------------------------------------------#
#xii)-La media de PAS /PAD
#MEDIA_PAS
#MEDIA_PAD#
PEU_CAT_CDRS<-PEU_CAT_CDRS%>%mutate(MEDIA_PAS_PAD=(MEDIA_PAS/MEDIA_PAD))
#------------------------------------------------------------------#  


#- [[antecedentes de ulceras   ]]
#- [[antecedentes de amputacion]]


#--------------------#  
#Antecedentes ulceras
#--------------------#
PEU_CAT_CDRS_ulceras<-read_excel(here::here("Dades",fitxer_dades),sheet="exportacionAntecedentesUlceras")
PEU_CAT_CDRS_ulceras2<-PEU_CAT_CDRS_ulceras%>%
  dplyr::group_by(idcrd) %>%dplyr::summarise(sum_latIzq = sum(Lateralidad_Izquierda),sum_latDer = sum(Lateralidad_Derecha))%>%ungroup 
PEU_CAT_CDRS_ulceras2<-PEU_CAT_CDRS_ulceras2%>%mutate(ulceras=sum_latIzq+sum_latDer)
PEU_CAT_CDRS_ulceras2<-PEU_CAT_CDRS_ulceras2%>%select(idcrd,ulceras)%>%arrange(idcrd) 
#PEU_CAT_CDRS_ulceras2
#--------------------#
#PEU_CAT_CDRS_ulceras<-read_excel(fitxer_dades,sheet="Antecedentesulceras")
#PEU_CAT_CDRS_ulceras 
#PEU_CAT_CDRS_ulceras2<-PEU_CAT_CDRS_ulceras%>%
#  dplyr::group_by(idcrd) %>%
#  dplyr::summarise(sum_latIzq = sum(latIzq),sum_latDer = sum(latDer))%>%
#  ungroup 
#PEU_CAT_CDRS_ulceras2<-PEU_CAT_CDRS_ulceras2%>%mutate(ulceras=sum_latIzq+sum_latDer)
#PEU_CAT_CDRS_ulceras2<-PEU_CAT_CDRS_ulceras2%>%select(idcrd,ulceras)%>%arrange(idcrd) 
#PEU_CAT_CDRS_ulceras2
#--------------------#


#---------------------#
#AntecedentesAmputacion
#---------------------#
#exportacionAntecedentesUlceras
#exportacionAntecedentesAmputaci



PEU_CAT_CDRS_amputaciones<-read_excel(here::here("Dades",fitxer_dades),sheet="exportacionAntecedentesAmputaci")
PEU_CAT_CDRS_amputaciones2<-PEU_CAT_CDRS_amputaciones%>%
  dplyr::group_by(idcrd)%>%
  dplyr::summarize(sum_latIzq = sum(Lateralidad_Izquierda),sum_latDer = sum(Lateralidad_Derecha))%>% 
  ungroup 
PEU_CAT_CDRS_amputaciones2<-PEU_CAT_CDRS_amputaciones2%>%mutate(amputaciones=sum_latIzq+sum_latDer)
#PEU_CAT_CDRS_amputaciones2
PEU_CAT_CDRS_amputaciones2<-PEU_CAT_CDRS_amputaciones2%>%select(idcrd,amputaciones)%>%arrange(idcrd) 




#PEU_CAT_CDRS_amputaciones<-read_excel(fitxer_dades,sheet="Antecedentesamputacion")
#PEU_CAT_CDRS_amputaciones2<-PEU_CAT_CDRS_amputaciones%>%
#  dplyr::group_by(idcrd)%>%
#  dplyr::summarize(sum_latIzq = sum(latIzq),sum_latDer = sum(latDer))%>% 
#  ungroup 
#PEU_CAT_CDRS_amputaciones2<-PEU_CAT_CDRS_amputaciones2%>%mutate(amputaciones=sum_latIzq+sum_latDer)
#PEU_CAT_CDRS_amputaciones2
#PEU_CAT_CDRS_amputaciones2<-PEU_CAT_CDRS_amputaciones2%>%select(idcrd,amputaciones)%>%arrange(idcrd) 





PEU_CAT_CDRS<-PEU_CAT_CDRS%>%left_join(PEU_CAT_CDRS_ulceras2, by="idcrd")
PEU_CAT_CDRS<-PEU_CAT_CDRS%>%left_join(PEU_CAT_CDRS_amputaciones2, by="idcrd")


names(PEU_CAT_CDRS)






#     feina a fer dilluns:

#i) Neuropatía periférica
#Años de duracion ( entre año de diagnostico y fecha inclusion)	Media (SD) ( calcular de nuevo) 	falta 



#ii)  Neuropatía autónoma		
#Años de duracion ( entre año de diagnostico y fecha inclusion)	Media (SD) ( calcular de nuevo) 	falta 

#iii) Insuficiencia renal crónica
#Años de duracion ( entre año de diagnostico y fecha inclusion)	Media (SD) ( calcular de nuevo) 	falta 

#iv)  Microalbuminuria		
#Años de duracion ( entre año de diagnostico y fecha inclusion)	Media (SD) ( calcular de nuevo) 	falta 

#v)   Macroalbuminuria o proteinuria/24 horas
#Años de duracion ( entre año de diagnostico y fecha inclusion)	Media (SD) ( calcular de nuevo) 	falta 

#vi)  Actualmente en diálisis	
#Años de duracion ( entre año de diagnostico y fecha inclusion)	Media (SD) ( calcular de nuevo) 	falta 

#vii) Transplante renal
#Años de duracion ( entre año de diagnostico y fecha inclusion)	Media (SD) ( calcular de nuevo) 	falta 

#viii)Neuroartropatía de Charcot	
#Años de duracion ( entre año de diagnostico y fecha inclusion)	Media (SD) ( calcular de nuevo) 	falta 


#Los que tienen ITB ( tienen fecha ITB) 	N ( frecuencia) 	falta 
#Derecho	Media (SD)	falta 
#Izquierdo	Media (SD)	falta 
#los que tienen monofilamento ( Variables valor=1,2,3,4)	N ( frecuencia) 	falta 
#Monofilamento pie derecho	Media (SD)	falta 
#Monofilamento pie izquierdo	Media (SD)	falta 
#los que tienen Sensibilidad vibratoria ( Variables valor=1)	N ( frecuencia) 	falta 
#Sensibilidad vibratoria pie derecho	Media (SD)	falta 
#Sensibilidad vibratoria pie izquierdo	Media (SD)	falta 
#los que tienen Ipswitch Touch ( Variables valor=1,2,3,)	N ( frecuencia) 	falta 
#Ipswitch Touch Test derecho	Media (SD)	falta 
#Ipswitch Touch Test izquierdo	Media (SD)	falta 




#FALTA FER:
###########################################################################
#i)     AVC_ano
#       Años de duracion ( entre año de diagnostico y fecha inclusion)
PEU_CAT_CDRS<-PEU_CAT_CDRS%>%mutate(duracion_AVC_ano=(lubridate::year(Fecha_consentimiento_informado))- (AVC_ano))

#ii)    Cardiopatia_isquemica_ano
#       Años de duracion ( entre año de diagnostico y fecha inclusion)
PEU_CAT_CDRS<-PEU_CAT_CDRS%>%mutate(duracion_Cardiopatia_isquemica_ano=(lubridate::year(Fecha_consentimiento_informado))- (Cardiopatia_isquemica_ano))

#iii)   Arteriopatia_periferica_ano
#       Años de duracion ( entre año de diagnostico y fecha inclusion)
PEU_CAT_CDRS<-PEU_CAT_CDRS%>%mutate(duracion_Arteriopatia_periferica_ano=(lubridate::year(Fecha_consentimiento_informado))- (Arteriopatia_periferica_ano))


#iv)    Antecedentes_por_intervencion_de_arteriopatia_periferica_ano
#       Años de duracion ( entre año de diagnostico y fecha inclusion)
PEU_CAT_CDRS<-PEU_CAT_CDRS%>%mutate(duracion_Antecedentes_por_intervencion_de_arteriopatia_periferica_ano=(lubridate::year(Fecha_consentimiento_informado))- (Antecedentes_por_intervencion_de_arteriopatia_periferica_ano))

#v)     Antecedentes_personales_de_insuficiencia_cardiaca_ano
#       Años de duracion ( entre año de diagnostico y fecha inclusion)
PEU_CAT_CDRS<-PEU_CAT_CDRS%>%mutate(duracion_Antecedentes_personales_de_insuficiencia_cardiaca_ano=(lubridate::year(Fecha_consentimiento_informado))- (Antecedentes_personales_de_insuficiencia_cardiaca_ano))

#vi)    Ano_de_diagnostico_definitivo_de_DM  ???
#       Años de duracion ( entre año de diagnostico y fecha inclusion)
PEU_CAT_CDRS<-PEU_CAT_CDRS%>%mutate(duracion_diagnostico_definitivo_de_DM_ano=(lubridate::year(Fecha_consentimiento_informado))- (Ano_de_diagnostico_definitivo_de_DM))

#via)
#Retinopatia_ano
PEU_CAT_CDRS<-PEU_CAT_CDRS%>%mutate(duracion_Retinopatia_ano=(lubridate::year(Fecha_consentimiento_informado))- (Retinopatia_ano))

#vii)   Neuropatia_periferica_ano
#       Años de duracion ( entre año de diagnostico y fecha inclusion)
PEU_CAT_CDRS<-PEU_CAT_CDRS%>%mutate(duracion_Neuropatia_periferica_ano=(lubridate::year(Fecha_consentimiento_informado))- (Neuropatia_periferica_ano))


#viii)  Neuropatia_autonoma_ano
#       Años de duracion ( entre año de diagnostico y fecha inclusion)
PEU_CAT_CDRS<-PEU_CAT_CDRS%>%mutate(duracion_Neuropatia_autonoma_ano=(lubridate::year(Fecha_consentimiento_informado))- (Neuropatia_autonoma_ano))


#ix)    Insuficiencia_renal_cronica_ano
#       Años de duracion ( entre año de diagnostico y fecha inclusion)
PEU_CAT_CDRS<-PEU_CAT_CDRS%>%mutate(duracion_Insuficiencia_renal_cronica_ano=(lubridate::year(Fecha_consentimiento_informado))- (Insuficiencia_renal_cronica_ano))


#x)     Microalbuminuria_ano
#       Años de duracion ( entre año de diagnostico y fecha inclusion)
PEU_CAT_CDRS<-PEU_CAT_CDRS%>%mutate(duracion_Microalbuminuria_ano=(lubridate::year(Fecha_consentimiento_informado))- (Microalbuminuria_ano))


#xi)    Macroalbuminuria_o_proteinuria_24_horas_ano
#       Años de duracion ( entre año de diagnostico y fecha inclusion)
PEU_CAT_CDRS<-PEU_CAT_CDRS%>%mutate(duracion_Macroalbuminuria_o_proteinuria_24_horas_ano=(lubridate::year(Fecha_consentimiento_informado))- ( Macroalbuminuria_o_proteinuria_24_horas_ano))


#xii)   Actualmente_en_dialisis_ano
#       Años de duracion ( entre año de diagnostico y fecha inclusion)
PEU_CAT_CDRS<-PEU_CAT_CDRS%>%mutate(duracion_Actualmente_en_dialisis_ano=(lubridate::year(Fecha_consentimiento_informado))- (Actualmente_en_dialisis_ano))

#xiii)  Transplante_renal_ano
#       Años de duracion ( entre año de diagnostico y fecha inclusion)
PEU_CAT_CDRS<-PEU_CAT_CDRS%>%mutate(duracion_Transplante_renal_ano=(lubridate::year(Fecha_consentimiento_informado))- ( Transplante_renal_ano))


#xiv)    Neuroartropatia_de_Charcot_ano
#       Años de duracion ( entre año de diagnostico y fecha inclusion)
PEU_CAT_CDRS<-PEU_CAT_CDRS%>%mutate(duracion_Neuroartropatia_de_Charcot_ano=(lubridate::year(Fecha_consentimiento_informado))- (Neuroartropatia_de_Charcot_ano))




variable.names(PEU_CAT_CDRS)
#PEU_CAT_CDRS$Fecha_de_ITB



#PEU_CAT_CDRS$ITB




#el que falta:
###########################################################################
#xv)    Los que tienen ITB ( tienen fecha ITB) 
PEU_CAT_CDRS<-PEU_CAT_CDRS%>%mutate(ITB=ifelse(is.na(Fecha_de_ITB),0,1))

#xvi)   Derecho
#PEU_CAT_CDRS$Derecho

#xvii)  Izquierdo
#PEU_CAT_CDRS$Izquierdo


#xviii) los que tienen monofilamento ( Variables valor=1,2,3,4)????
PEU_CAT_CDRS<-PEU_CAT_CDRS%>%mutate(Monofilamento=ifelse((Monofilamento_pie_derecho==1 | Monofilamento_pie_izquierdo==1),0,1))

#xix)   Monofilamento pie derecho
#PEU_CAT_CDRS$Monofilamento_pie_derecho

#xx)    Monofilamento pie izquierdo
#PEU_CAT_CDRS$Monofilamento_pie_izquierdo



#xxi)   los que tienen Sensibilidad vibratoria ( Variables valor=1)?????
PEU_CAT_CDRS<-PEU_CAT_CDRS%>%mutate(Sensibilidad_vibratoria=ifelse((Sensibilidad_vibratoria_pie_derecho==1 | Sensibilidad_vibratoria_pie_izquierdo==1),0,1))

#xxii)  Sensibilidad vibratoria pie derecho
#PEU_CAT_CDRS$Sensibilidad_vibratoria_pie_derecho

#xxiii) Sensibilidad vibratoria pie izquierdo
#PEU_CAT_CDRS$Sensibilidad_vibratoria_pie_izquierdo



#xxiv)  los que tienen Ipswitch Touch ( Variables valor=1,2,3,)????
PEU_CAT_CDRS<-PEU_CAT_CDRS%>%mutate(Ipswitch_Touch_Test=ifelse((Ipswitch_Touch_Test_derecho==1 | Ipswitch_Touch_Test_izquierdo==1),0,1))

#xxv)   Ipswitch Touch Test derecho
#PEU_CAT_CDRS$Ipswitch_Touch_Test_derecho

#xxvi)  Ipswitch Touch Test izquierdo
#PEU_CAT_CDRS$Ipswitch_Touch_Test_izquierdo
###########################################################################







#etiquetar
#etiquetar_taula
#etiquetar_valors




  



#------------------------------------------------------------------#
#conductor_variables<-"taulavariables_v2_PEU3.xls"
#PEU_CAT_CDRS_amputaciones<-read_excel(here::here("Dades",fitxer_dades),sheet="exportacionAntecedentesAmputaci")
#------------------------------------------------------------------#

#------------------------------------------------------------------#
conductor_variables<-"taulavariables_v2_PEU4.xls"
#------------------------------------------------------------------#

#LAB_ETIQ_v2  






#------------------------------------------------------------------#
PEU_CAT_CDRS<-convertir_dates(d=PEU_CAT_CDRS,taulavariables=conductor_variables)
PEU_CAT_CDRS<-etiquetar_valors(dt=PEU_CAT_CDRS,variables_factors=conductor_variables,fulla="etiquetes",camp_etiqueta="etiqueta2")
#PEU_CAT_CDRS<-LAB_ETIQ_v2(dt=PEU_CAT_CDRS,variables_factors=conductor_variables,fulla="etiquetes",idioma="etiqueta2")
PEU_CAT_CDRS<-etiquetar(d=PEU_CAT_CDRS,taulavariables=conductor_variables)
#------------------------------------------------------------------#

#variable.names(PEU_CAT_CDRS)

#names(LAB_ETIQ_PEU_CAT)
#***********************************************************************#
#taula00 Criteris Inclusio
formula_taula00<-formula_compare("taula00",y="",taulavariables = conductor_variables)
T00<-descrTable(formula_taula00,method = 2,data=PEU_CAT_CDRS)
#***********************************************************************#
T00




#***********************************************************************#
#***********************************************************************#
#[N=345]
#***********************************************************************#
#***********************************************************************#
PEU_CAT_CDRS<-PEU_CAT_CDRS%>%filter(PacInc=="Si")
table(PEU_CAT_CDRS$PacInc)


#PACIENTS INCLUITS!!!!
#***********************************************************************#
#***********************************************************************#
#[[N=264]]
#***********************************************************************#
#***********************************************************************#

#flow-chart!:[]
#***********************************************************************#
table(PEU_CAT_CDRS$Criterios_exclusion_1)
#Num_Excluits-->  1
table(PEU_CAT_CDRS$Criterios_exclusion_2)
#Num_Excluits--> 2
table(PEU_CAT_CDRS$Criterios_exclusion_3)
#Num_Excluits-->  6
#***********************************************************************#

table(PEU_CAT_CDRS$PacIncExc)



#[N=257]




T13<-diagramaFlowchart(
  grups=1,
  pob_lab1=c("Pacientes con criterio de Inclusion ","Pacientes con criterio de Inclusion y sin criterios de Exclusion  "),
  pob1=c(264,257),
  exc1=c(1,2,6),
  exc_lab1=c('Pacientes diagnosticados de DM1/Diabetis Gestacional/Diabetis Secundaria a medicamentos',
             'Pacientes con una esperanza de vida < 1 año',
             'Ulceras activas a inicio del estudio'),
  colors=c('white','grey'),
  forma=c('box','box'))


T13




#PACIENTS INCLUITS!
#[[N=264]]




#------------------------------------------------------------------#
#names(LAB_ETIQ_PEU_CAT)
#***********************************************************************#
#Taula1 Criteris  Exclusio.
formula_taula1<-formula_compare("taula01",y="",taulavariables = conductor_variables)
T1<-descrTable(formula_taula1,method = 2,data=PEU_CAT_CDRS)
#***********************************************************************#
T1
#***********************************************************************#
#
#

#PACIENTS INCLUITS!. i sense EXCLUSIONS![]
#N=257.

PEU_CAT_CDRS<-PEU_CAT_CDRS%>%filter(PacIncExc=="Pacientes Incluidos")
table(PEU_CAT_CDRS$PacInc)



#***********************************************************************#
#Taula2
formula_taula2<-formula_compare("taula02",y="",taulavariables = conductor_variables)
#----
T2a<-descrTable(formula_taula2,method = 1,Q1 = 0, Q3 =1 ,data=PEU_CAT_CDRS)
#media/sd
#T2a
#----
T2b<-descrTable(formula_taula2,method = 2,Q1 = 0, Q3 =1 ,data=PEU_CAT_CDRS)
#median(min-max)
#T2b
#***********************************************************************#
#Taula3
formula_taula3<-formula_compare("taula03",y="",taulavariables = conductor_variables)
#----
T3a<-descrTable(formula_taula3,method = 1,Q1 = 0, Q3 =1,data=PEU_CAT_CDRS)
#T3a
#----
T3b<-descrTable(formula_taula3,method = 2,Q1 = 0, Q3 =1,data=PEU_CAT_CDRS)
#T3b
#***********************************************************************#
#Taula4
formula_taula4<-formula_compare("taula04",y="",taulavariables = conductor_variables)
#----
T4a<-descrTable(formula_taula4,method = 1,Q1 = 0, Q3 =1,data=PEU_CAT_CDRS)
#T4a
#----
T4b<-descrTable(formula_taula4,method = 2,Q1 = 0, Q3 =1,data=PEU_CAT_CDRS)
#T4b
#***********************************************************************#
#Taula5 
formula_taula5<-formula_compare("taula05",y="",taulavariables = conductor_variables)
#----
T5a<-descrTable(formula_taula5,method = 1,Q1 = 0, Q3 =1,data=PEU_CAT_CDRS)
#T5a
#----
T5b<-descrTable(formula_taula5,method = 2,Q1 = 0, Q3 =1,data=PEU_CAT_CDRS)
#T5b
#***********************************************************************#
T00
T13
T1
T2a
#T2b
T3a
#T3b
T4a
#T4b
T5a
#T5b
#***********************************************************************#
#[T00/T01/T1/T2/T3/T4/T5]
#***********************************************************************#





#[1.3.2019]
#Canvis per corregir els errors
#table(LAB_ETIQ_PEU_CAT$colTot)
#table(LAB_ETIQ_PEU_CAT$colTot2)

#http://www.sediabetes.org/calculadoras/calculadoras/conversor.aspx

#***********************************************************************#
#taula3
#formula_taula3_Corregit<-formula_compare("Taula3Corregit",y="",taulavariables = conductor_variables)
#T3corregit<-descrTable(formula_taula3_Corregit,method = 2,Q1 = 0, Q3 =1,data=PEU_CAT_CDRS)
#T3corregit




#***********************************************************************#
#taula4
#formula_taula4<-formula_compare("taula4",y="",taulavariables = conductor_variables)
#T4<-descrTable(formula_taula4,method = 2,Q1 = 0, Q3 =1,data=PEU_CAT_CDRS)
#T4
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


#http://theautomatic.net/2018/07/11/manipulate-files-r/

T00
T13
T1
T2a
#T2b
T3a
#T3b
T4a
#T4b
T5a
#T5b

save(T00,T1,T2a,T3a,T4a,T5a,T13,file="ResultatsPEUCAT2.Rdata")


#1.- Monofilamento
#- Normal: 4 puntos encontrados
#- Patologico: ??? 3 puntos
#2.- Monofilamento patologico + sensibilidad vibratoria ausente
#3.- Ipswith Touch Test
#- normal: 3 puntos
#- patologico: ??? 2 puntos
#4.- Arteriopatia periferica
#- presencia dos pulsos (P o TP)
#- ausencia un pulso (P o TP)
#- ausencia dos pulsos (P y TP)
#Falta tambien
#1.- antecedentes de HTA: si/no
#2.- antecedentes de dislipemia: si/no
#3.- tiempo evolucion de la diabetes mellitus
#4.- antecedentes personales de:
#  - neuropatia periferica
#- neuropatia autonoma
#- insuficiencia renal
#- microalbuminuria o macroalbuminuria
#- actualmente en di?lisis
#- trasplante renal
#- neuroartropatia de Charcot
#- antecedentes de ulceras
#- antecedentes de amputacion




# errors


#eps fallo!

#PEU_CAT_CDRS$Edad
#"confirmar"---idcrd:[1330]





#variable.names(PEU_CAT_CDRS)

#write.csv2(names(PEU_CAT_CDRS),"varibles_peucat.csv")








# i TAULA2



#fer el 31.12.2019:

# Repassar errors taula1!
# Funció que posa les noves variables al conductor!!!
# Fer R-markdown
# ...i si és possible taula2
# Tema  DataHarmonization!!!

#10.01.2020

#Tabla 2 ulceras basales 	

#Variable	analisis 
#-------------------------------------------------------------------------------#
#Desencadenante.Uñas	N( frecuencia) 
#Desencadenante.Zapatos	N( frecuencia) 
#Desencadenante.Herida	N( frecuencia) 
#Desencadenante.Golpe	N( frecuencia) 
#Desencadenante.Callosidad	N( frecuencia) 
#Desencadenante.Ejercicio	N( frecuencia) 
#Desencadenante.Quemadura	N( frecuencia) 
#Desencadenante.Hongos	N( frecuencia) 
#Desencadenante.Desconocido	N( frecuencia) 
#Desencadenante.Otros	N( frecuencia) 
#Desencadenante.ExplicacionOtros	N( frecuencia) 
#-------------------------------------------------------------------------------#
#Localización.Izquierda	N( frecuencia) 
#Localización.Derecha	N( frecuencia) 
#-------------------------------------------------------------------------------#
#Lugar.PlantaDedos	N( frecuencia) 
#Lugar.Dorso dedos o espacios interdigitales	N( frecuencia) 
#Lugar.Dorso o lateral del pie	N( frecuencia) 
#Lugar.Planta antepie o medio pie	N( frecuencia) 
#Lugar.Talón	N( frecuencia) 
#-------------------------------------------------------------------------------#


#-------------------------------------------------------------------------------#
#Si fué derivado al hospital ¿Le han dado el alta?	N( frecuencia) 
#Superficie	Media (SD)
#Profundidad.Superficial	N( frecuencia) 
#Profundidad.Profunda	N( frecuencia) 
#Profundidad.Muy Profunda	N( frecuencia) 
#Eritema perilesional < 2cm	N( frecuencia) 
#Eritema perilesional > 2cm	N( frecuencia) 
#Induración	N( frecuencia) 
#-------------------------------------------------------------------------------#
#Calor	N( frecuencia) 
#Dolor	N( frecuencia) 
#Secreción purulenta	N( frecuencia) 
#Afectación sistémica	N( frecuencia) 
#Linfangitis	N( frecuencia) 
#Gangrena	N( frecuencia) 
#Antiseptico	N( frecuencia) 
#Apósito	N( frecuencia) 
#-------------------------------------------------------------------------------#
#Descarga con fieltro	N( frecuencia) 
#Desbridamiento	N( frecuencia) 
#Desbridamiento Tipo.Cortante	N( frecuencia) 
#Desbridamiento Tipo.Quirúrgico	N( frecuencia) 
#Desbridamiento Tipo.Enzimático	N( frecuencia) 
#Desbridamiento Tipo.Autolítico	N( frecuencia) 
#Antibiótico tópico	N( frecuencia) 
#Antibiótico oral	N( frecuencia) 
#¿Se realiza cultivo?	N( frecuencia) 
#¿Se deriva?	N( frecuencia) 
#Se deriva.Unidad de pie diabético	N( frecuencia) 
#Se deriva.Cirugía vascular	N( frecuencia) 
#Se deriva.Endocrinología	N( frecuencia) 
#Se deriva.Podólogo	N( frecuencia) 
#Se deriva.Otros	N( frecuencia) 
#Explicación derivación otros	N( frecuencia) 
#-------------------------------------------------------------------------------#


# exportacionUlceras
# exportacionUlcerasVisitas



#----------------------------------------------------#
fitxer_dades<-"exportacion22112019_Dep_Magda3_bog.xls"
#----------------------------------------------------#
PEU_CAT_CDRS2<-read_excel(here::here("Dades",fitxer_dades),sheet="exportacionUlceras")
variable.names(PEU_CAT_CDRS2)
#----------------------------------------------------#


PEU_CAT_CDRS3<-read_excel(here::here("Dades",fitxer_dades),sheet="exportacionUlcerasVisitas")
variable.names(PEU_CAT_CDRS3)
PEU_CAT_CDRS3<-PEU_CAT_CDRS3%>%filter(idvisita==0) 

PEU_CAT_CDRS3$Superficie <- as.numeric(PEU_CAT_CDRS3$Superficie)




#mean(PEU_CAT_CDRS3$Superficie)

conductor_variables<-"taulavariables_v2_PEU5.xls"
#------------------------------------------------------------------#
PEU_CAT_CDRS2<-convertir_dates(d=PEU_CAT_CDRS2,taulavariables=conductor_variables)
PEU_CAT_CDRS2<-etiquetar_valors(dt=PEU_CAT_CDRS2,variables_factors=conductor_variables,fulla="etiquetes1",camp_etiqueta="etiqueta2")
PEU_CAT_CDRS2<-etiquetar(d=PEU_CAT_CDRS2,taulavariables=conductor_variables)
#------------------------------------------------------------------#
formula_taula0<-formula_compare("taula0",y="",taulavariables = conductor_variables)
T6a<-descrTable(formula_taula0,method = 1,data=PEU_CAT_CDRS2,max.xlev = 100)

#descrTable(~.,data=DT_PACIENT,show.p.overall=F,method = 2,Q1=0,Q3=1,max.xlev = 40)


#------------------------------------------------------------------#
T6a
#------------------------------------------------------------------#

#PEU_CAT_CDRS3$Explicacionderivacionotros

#Eritemaperilesionalmenos2cm
#Eritemaperilesionalmas2cm

conductor_variables<-"taulavariables_v2_PEU6.xls"
#------------------------------------------------------------------#
PEU_CAT_CDRS3<-convertir_dates(d=PEU_CAT_CDRS3,taulavariables=conductor_variables)
PEU_CAT_CDRS3<-etiquetar_valors(dt=PEU_CAT_CDRS3,variables_factors=conductor_variables,fulla="etiquetes2",camp_etiqueta="etiqueta2")
PEU_CAT_CDRS3<-etiquetar(d=PEU_CAT_CDRS3,taulavariables=conductor_variables)

#------------------------------------------------------------------#
formula_taula1<-formula_compare("taula1",y="",taulavariables = conductor_variables)
T7a<-descrTable(formula_taula1,method = 1,data=PEU_CAT_CDRS3,max.xlev = 100)
#------------------------------------------------------------------#
T7a
#------------------------------------------------------------------#




save(T6a,T7a,file="ResultatsPEUCAT3.Rdata")


#   Les  pàgines Excel : [fitxer_dades<-"exportacion22112019_Dep_Magda3_bog.xls"]

#   [exportacionCRDS]
#   [exportacionUlceras]
#   [exportacionUlcerasVisitas]

#   EXCEL. --> S'HA CANVIAT  [A.B --> A_B ; A B --> AB ; À --> A ]    


