#(ACTUALITZACIONS)
#--------------------------------------------------------------------------------#
#[21.12.2019]
#--------------------------------------------------------------------------------#
#[20.12.2019]
#--------------------------------------------------------------------------------#
#[19.12.2019]                      
#--------------------------------------------------------------------------------#
#[16.11.2019]               
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
#                        2. CONVERTIR DATES![de numeric a codo Data!]
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






















#----------------------------------------------------#
#casa-->Av Estatut de Catalunya!.
#----------------------------------------------------#
#setwd("C:/Users/Suta/Desktop/peucat2/peucat3")
#"C:/Users/Suta/Desktop/peucat2/peucat3" %>% file.path("FUNCIONS_PROPIES.R") %>% source()
#----------------------------------------------------#
#Sant Pau.[C:\Users\Suta\Desktop\4.3.2019]
#setwd("C:/Users/Suta/Desktop/4.3.2019")
#"C:/Users/Suta/Desktop/4.3.2019" %>% file.path("FUNCIONS_PROPIES.R") %>% source()
#----------------------------------------------------#
#feina-->c/Sardenya![C:\Users\38122893W\Desktop\PeuCat_Final]
#----------------------------------------------------#
#casa-->Av Estatut de Catalunya!
#setwd("C:/Users/Suta/Desktop/peucat_FINAL")
#"C:/Users/Suta/Desktop/peucat_FINAL"%>% file.path("FUNCIONS_PROPIES3.R") %>% source()
#----------------------------------------------------#
#setwd("C:/Users/38122893W/Desktop/PeuCat_Final2")
#"C:/Users/38122893W/Desktop/PeuCat_Final2"%>% file.path("FUNCIONS_PROPIES3.R") %>% source()
#----------------------------------------------------
#----------------------------------------------------#
#setwd("C:/Users/38122893W/Desktop/PeuCat")
#"C:/Users/38122893W/Desktop/PeuCat"%>% file.path("funcions_propies_2019.R") %>% source()
#----------------------------------------------------#
#----------------------------------------------------#
#setwd("C:/Users/38122893W/Desktop/PeuCat_Final2")
#"C:/Users/38122893W/Desktop/PeuCat_Final2"%>% file.path("funcions_propies_2019.R") %>% source()
#----------------------------------------------------#
#setwd("C:/Users/Suta/Desktop/peuCat_FINAL2")
#"C:/Users/Suta/Desktop/peucat_FINAL2"%>% file.path("funcions_propies_2019.R") %>% source()
#----------------------------------------------------#
#EPPPPS!!!! dubte preguntar Jordi Real!!

fitxer_dades<-"exportacion22112019_Dep_Magda2.xls"

#----------------------------------------------------#
#read_excel("Dades/exportacion22112019_Dep_Magda2.xls",sheet="exportacionCRDS")
#read_excel(paste0("Dades/","exportacion22112019_Dep_Magda2.xls"),sheet="exportacionCRDS")
#read_excel(paste0("Dades/",fitxer_dades),sheet="exportacionCRDS")
#read_excel(paste0("Dades/",fitxer_dades),sheet="exportacionCRDS")

PEU_CAT_CDRS<-read_excel(here::here("Dades",fitxer_dades),sheet="exportacionCRDS")
#----------------------------------------------------#
#PEU_CAT_CDRS<-read_excel(fitxer_dades,sheet="exportacionCRDS")

variable.names(PEU_CAT_CDRS)
#fitxer_dades="exportacionCRD_peu_cat_final3.xls"
#PEU_CAT_CDRS<-read_excel(fitxer_dades,sheet="CRDS")
#[1] "idcrd"                                                       
#[2] "Estado_paciente_Incluido"                                    
#[3] "Estado_paciente_Discontinuado_abandono"                      
#[4] "Estado_paciente_Discontinuado_fallecimiento"                 
#[5] "Estado_Crd_En_Curso"                                         
#[6] "Estado_Crd_Finalizado"                                       
#[7] "idinvestigador"                                              
#[8] "Fecha_consentimiento_informado"                              
#[9] "Codigo_paciente"                                             
#[10] "CIP"                                                         
#[11] "Criterios_inclusion_1"                                       
#[12] "Criterios_inclusion_2"                                       
#[13] "Criterios_inclusion_3"                                       
#[14] "Criterios_exclusion_1"                                       
#[15] "Criterios_exclusion_2"                                       
#[16] "Criterios_exclusion_3"                                       
#[17] "Fecha_nacimiento"                                            
#[18] "Edad"                                                        
#[19] "Sexo_Hombre"                                                 
#[20] "Sexo_Mujer"                                                  
#[21] "Etnia_Caucasica"                                             
#[22] "Etnia_Hispanoamericana"                                      
#[23] "Etnia_Indostani"                                             
#[24] "Etnia_Africana"                                              
#[25] "Etnia_Otras"                                                 
#[26] "Especificar_etnia"                                           
#[27] "Tabaquismo_No"                                               
#[28] "Tabaquismo_Si"                                               
#[29] "Tabaquismo_Exfumador"                                        
#[30] "Cigarrillos_dia"                                             
#[31] "Anos_tabaquismo"                                             
#[32] "Paquetes_Ano"                                                
#[33] "Alcohol_Abstemio"                                            
#[34] "Alcohol_Bajo_Riesgo"                                         
#[35] "Alcohol_Riesgo_elevado"                                      
#[36] "Antecedentes_de_consumo_excesivo"                            
#[37] "PA_sistolica_1"                                              
#[38] "Pa_diastolica_1"                                             
#[39] "PA_sistolica_2"                                              
#[40] "PA_diastolica_2"                                             
#[41] "MEDIA_PAS"                                                   
#[42] "MEDIA_PAD"                                                   
#[43] "Diagnostico_HTA"                                             
#[44] "Diagnostico_dislipemia"                                      
#[45] "AVC"                                                         
#[46] "AVC_ano"                                                     
#[47] "Cardiopatia_isquemica"                                       
#[48] "Cardiopatia_isquemica_ano"                                   
#[49] "Arteriopatia_periferica"                                     
#[50] "Arteriopatia_periferica_ano"                                 
#[51] "Antecedentes_por_intervención_de_arteriopatia_periferica"    
#[52] "Antecedentes_por_intervencion_de_arteriopatia_periferica_ano"
#[53] "Antecedentes_personales_de_insuficiencia_cardiaca"           
#[54] "Antecedentes_personales_de_insuficiencia_cardiaca_ano"       
#[55] "Diagnostico_en_la_visita_basal"                              
#[56] "Ano_de_diagnostico_definitivo_de_DM"                         
#[57] "TratamientoDM2_Sin_tratamiento"                              
#[58] "TratamientoDM2_Metformina"                                   
#[59] "TratamientoDM2_SU"                                           
#[60] "TratamientoDM2_Pioglitazona"                                 
#[61] "TratamientoDM2_Repaglinida"                                  
#[62] "TratamientoDM2_iDPP4"                                        
#[63] "TratamientoDM2_iSGLT2"                                       
#[64] "TratamientoDM2_Analogos_GLP1"                                
#[65] "TratamientoDM2_Insulina"                                     
#[66] "TratamientoDM2_αglucosidasa"                                 
#[67] "TratamientoDM2_Otros"                                        
#[68] "TratamientoDM2_Explicacion_Otros"                            
#[69] "Tratamiento_diabetes_no_farmacologico"                       
#[70] "Tratamiento_diabetes_no_farmacologico_dieta_y_ejercicios"    
#[71] "Retinopatia"                                                 
#[72] "Retinopatia_ano"                                             
#[73] "Neuropatia_periferica"                                       
#[74] "Neuropatia_periferica_ano"                                   
#[75] "Neuropatia_autonoma"                                         
#[76] "Neuropatia_autonoma_ano"                                     
#[77] "Insuficiencia_renal_cronica"                                 
#[78] "Insuficiencia_renal_cronica_ano"                             
#[79] "Microalbuminuria"                                            
#[80] "Microalbuminuria_ano"                                        
#[81] "Macroalbuminuria_o_proteinuria_24_horas"                     
#[82] "Macroalbuminuria_o_proteinuria_24_horas_ano"                 
#[83] "Actualmente_en_dialisis"                                     
#[84] "Actualmente_en_dialisis_ano"                                 
#[85] "Transplante_renal"                                           
#[86] "Transplante_renal_ano"                                       
#[87] "Neuroartropatia_de_Charcot"                                  
#[88] "Neuroartropatia_de_Charcot_ano"                              
#[89] "Fecha_analitica"                                             
#[90] "HBA1C"                                                       
#[91] "Colesterol_total"                                            
#[92] "Unidad_Colesterol_total"                                     
#[93] "HDL"                                                         
#[94] "Unidad_HDL"                                                  
#[95] "LDL"                                                         
#[96] "Unidad_LDL"                                                  
#[97] "Trigliceridos"                                               
#[98] "Unidad_Trigliceridos"                                        
#[99] "Creatinina"                                                  
#[100] "Unidad_Creatinina"                                           
#[101] "Filtrado_glomerular_estimado"                                
#[102] "Unidad_Filtrado_glomerular_estimado"                         
#[103] "Cociente_Albumina_Creatinina_o_proteinuria"                  
#[104] "Unidad_Cociente_Albumina_Creatinina_o_proteinuria"           
#[105] "Fecha_exploracion"                                           
#[106] "Peso"                                                        
#[107] "Talla"                                                       
#[108] "IMC"                                                         
#[109] "Disminucion_de_la_agudeza_visual"                            
#[110] "Problemas_con_la_movilidad"                                  
#[111] "Tiene_cuidador"                                              
#[112] "Visita_al_podologo_en_el_ultimo_ano"                         
#[113] "Calzado_correcto"                                            
#[114] "Hidratacion_correcta"                                        
#[115] "Espacios_interdigitales_con_lesiones_micoticas"              
#[116] "Patologia_ungueal"                                           
#[117] "Cuidado_de_las_unas_correcto"                                
#[118] "Hiperqueratosis_callosidades"                                
#[119] "Edema_maleolar"                                              
#[120] "Otros"                                                       
#[121] "Explicacion_otros"                                           
#[122] "Deformidades"                                                
#[123] "Dedos_en_garra_martillo"                                     
#[124] "Halluxvalgus"                                                
#[125] "Pie_plano_cavo"                                              
#[126] "Sobrecarga_metatarsianos"                                    
#[127] "Charcot"                                                     
#[128] "Otras"                                                       
#[129] "Tibial_posterior_pie_derecho"                                
#[130] "Tibial_posterior_pie_izquierdo"                              
#[131] "Pediopie_derecho"                                            
#[132] "Pediopie_izquierdo"                                          
#[133] "Es_valorable"                                                
#[134] "Fecha_de_ITB"                                                
#[135] "Derecho"                                                     
#[136] "Izquierdo"                                                   
#[137] "Motivo_no_realizacion...137"                                 
#[138] "Neuropatia_periferica_valorable"                             
#[139] "Monofilamento_pie_derecho"                                   
#[140] "Monofilamento_pie_izquierdo"                                 
#[141] "Sensibilidad_vibratoria_pie_derecho"                         
#[142] "Sensibilidad_vibratoria_pie_izquierdo"                       
#[143] "Ipswitch_Touch_Test_derecho"                                 
#[144] "Ipswitch_Touch_Test_izquierdo"                               
#[145] "Motivo_no_realizacion...145"  
#################################################



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


#   ??????

#PEU_CAT_CDRS$Fecha_consentimiento_informado





#PEU_CAT_CDRS$Ano_de_diagnostico_definitivo_de_DM








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


#table(PEU_CAT_CDRS$ArtPer_Izq	,useNA="always")

----------------------------------------------------------------#
#08.03.2019:[]
#names(dades)
#------------------------------------------------------------------#



#- [[antecedentes de ulceras   ]]
#- [[antecedentes de amputacion]]


  
#Antecedentes ulceras
#---------------------------------------------------------------------------------------------------------------------#
PEU_CAT_CDRS_ulceras<-read_excel(here::here("Dades",fitxer_dades),sheet="exportacionAntecedentesUlceras")
PEU_CAT_CDRS_ulceras2<-PEU_CAT_CDRS_ulceras%>%
  dplyr::group_by(idcrd) %>%dplyr::summarise(sum_latIzq = sum(Lateralidad_Izquierda),sum_latDer = sum(Lateralidad_Derecha))%>%ungroup 
PEU_CAT_CDRS_ulceras2<-PEU_CAT_CDRS_ulceras2%>%mutate(ulceras=sum_latIzq+sum_latDer)
PEU_CAT_CDRS_ulceras2<-PEU_CAT_CDRS_ulceras2%>%select(idcrd,ulceras)%>%arrange(idcrd) 
PEU_CAT_CDRS_ulceras2
#---------------------------------------------------------------------------------------------------------------------#
#PEU_CAT_CDRS_ulceras<-read_excel(fitxer_dades,sheet="Antecedentesulceras")
#PEU_CAT_CDRS_ulceras 
#PEU_CAT_CDRS_ulceras2<-PEU_CAT_CDRS_ulceras%>%
#  dplyr::group_by(idcrd) %>%
#  dplyr::summarise(sum_latIzq = sum(latIzq),sum_latDer = sum(latDer))%>%
#  ungroup 
#PEU_CAT_CDRS_ulceras2<-PEU_CAT_CDRS_ulceras2%>%mutate(ulceras=sum_latIzq+sum_latDer)
#PEU_CAT_CDRS_ulceras2<-PEU_CAT_CDRS_ulceras2%>%select(idcrd,ulceras)%>%arrange(idcrd) 
#PEU_CAT_CDRS_ulceras2
#---------------------------------------------------------------------------------------------------------------------#



#AntecedentesAmputacion
#------------------------------------------------------------------#

#exportacionAntecedentesUlceras
#exportacionAntecedentesAmputaci



PEU_CAT_CDRS_amputaciones<-read_excel(here::here("Dades",fitxer_dades),sheet="exportacionAntecedentesAmputaci")
PEU_CAT_CDRS_amputaciones2<-PEU_CAT_CDRS_amputaciones%>%
  dplyr::group_by(idcrd)%>%
  dplyr::summarize(sum_latIzq = sum(Lateralidad_Izquierda),sum_latDer = sum(Lateralidad_Derecha))%>% 
  ungroup 
PEU_CAT_CDRS_amputaciones2<-PEU_CAT_CDRS_amputaciones2%>%mutate(amputaciones=sum_latIzq+sum_latDer)
PEU_CAT_CDRS_amputaciones2
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










#------------------------------------------------------------------#
#conductor_variables<-"taulavariables_v2_PEU3.xls"
#PEU_CAT_CDRS_amputaciones<-read_excel(here::here("Dades",fitxer_dades),sheet="exportacionAntecedentesAmputaci")
#------------------------------------------------------------------#

#------------------------------------------------------------------#
conductor_variables<-"taulavariables_v2_PEU4.xls"
#------------------------------------------------------------------#
PEU_CAT_CDRS<-convertir_dates(d=PEU_CAT_CDRS,taulavariables=conductor_variables)
PEU_CAT_CDRS<-LAB_ETIQ_v2(dt=PEU_CAT_CDRS,variables_factors=conductor_variables,fulla="etiquetes",idioma="etiqueta2")
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
T2<-descrTable(formula_taula2,method = 2,Q1 = 0, Q3 =1 ,data=PEU_CAT_CDRS)
T2
#***********************************************************************#
#Taula3
formula_taula3<-formula_compare("taula03",y="",taulavariables = conductor_variables)
T3<-descrTable(formula_taula3,method = 2,Q1 = 0, Q3 =1,data=PEU_CAT_CDRS)
T3
#***********************************************************************#
#Taula4
formula_taula4<-formula_compare("taula04",y="",taulavariables = conductor_variables)
T4<-descrTable(formula_taula4,method = 2,Q1 = 0, Q3 =1,data=PEU_CAT_CDRS)
T4
PEU_CAT_CDRS$Tratamiento_diabetes_no_farmacologico

#***********************************************************************#
#Taula5 
formula_taula5<-formula_compare("taula05",y="",taulavariables = conductor_variables)
T5<-descrTable(formula_taula5,method = 2,Q1 = 0, Q3 =1,data=PEU_CAT_CDRS)
T5
#***********************************************************************#


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



save(T00,T1,T2,T3,T4,T5,file="ResultatsPEUCAT2.Rdata")


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








