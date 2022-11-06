
library(shiny)
library(plotly)
library(shinyjs)
library(shinycssloaders)
#shinycssloaders option
options(spinner.type=5, spinner.size=0.75)
library(jsonlite)
library(DT)

library(dbplyr)
library(DBI)
library(dplyr)
library(ggplot2)
library(RSQLite)


#con <- dbConnect(RSQLite::SQLite(), "/mnt/d/Repository_ubuntu/HRT_Atlas_db/LogMCF_Housekeeping_human_mouse.sqlite")

#con1 <- dbConnect(RSQLite::SQLite(), "/mnt/d/Repository_ubuntu/HRT_Atlas_db/MCF_Housekeeping_Mouse_mouse.sqlite")

con <- dbConnect(RSQLite::SQLite(), "/srv/shiny-server/housekeepingAtlas/db/LogMCF_Housekeeping_human_mouse.sqlite")

con1 <- dbConnect(RSQLite::SQLite(), "/srv/shiny-server/housekeepingAtlas/db/MCF_Housekeeping_human_mouse.sqlite")


load("www/Housekeeping_TranscriptsMouse.RData")
#load("External/Data/DesignedPrimer/sampleTable.RData")
load("External/Data/DesignedPrimer/RefInfoHumanHK2.RData")
load("External/Data/primer/RefInfoMouseHK2.RData")
#Load HK_RPKM
load("External/Data/HK_RPKM1.RData")
load("External/Data/HK_RPKM2.RData")
load("External/Data/HK_RPKM3.RData")
load("External/Data/HK_RPKM4.RData")
#Merge HK_RPKM

HK_RPKM = cbind(HK_RPKM1,HK_RPKM2,HK_RPKM3,HK_RPKM4)
#Remove after merging
rm(HK_RPKM1,HK_RPKM2,HK_RPKM3,HK_RPKM4)

load("External/Data/Housekeeping_Genes.RData")
#sampleTable <- filter(sampleTable, Designed_Primers>0)


load("External/Data/tab_echant_HRTA.RData")
load("External/Data/GeneInfo.RData")

load("External/Data/GeneMouse.RData")

tab_echant <- tab_echant_HRTA
 
rm(tab_echant_HRTA)

load("External/Data/tab_echant_mouse_HRTA.RData")

load("External/Data/Housekeeping_TranscriptFiltered.RData")
tab_echant_mouse <- tab_echant_mouse_HRTA
rm(tab_echant_mouse_HRTA)
#### Primer table

humanPrimer <- read.csv(file = "www/Validation/Primers_Table/Human_Primer_Validation.csv", sep = ";")
humanPrimer <- humanPrimer[,c(2,1,3:9)]
colnames(humanPrimer)[1] = "Transcript_ID"

mousePrimer <- read.csv(file = "www/Validation/Primers_Table/Mouse_Primer_Validation.csv", sep = ";")
mousePrimer <- mousePrimer[,c(2,1,3:9)]

stringsasfactors = TRUE
###########################
###svgico
###########################
svgico1 ='<svg onclick="myModal()" height="20pt" viewBox="-59 0 511 512" width="20pt" xmlns="http://www.w3.org/2000/svg"'
svgico2 = '><path d="m386.136719 430.289062-115.097657-207.175781v-11.113281h-45c-8.285156 0-15-6.714844-15-15s6.714844-15 15-15h45v-30h-75c-8.285156 0-15-6.714844-15-15s6.714844-15 15-15h75v-30h-45c-8.285156 0-15-6.714844-15-15s6.714844-15 15-15h45v-32h15c8.28125 0 15-6.714844 15-15s-6.71875-15-15-15h-180c-8.285156 0-15 6.714844-15 15s6.714844 15 15 15h15v193.15625l-113.691406 207.40625c-20.078125 36.632812 6.394532 81.4375 48.230469 81.4375h282.480469c41.914062 0 68.460937-45.019531 48.078125-81.710938zm0 0"/></svg>'


############### Load gene for epiregio REM computation

load("External/Data/Epiregio/EpiRegio_Gene_HRTAtlas.RData")

###################### Load mouse HK genes
load("www/Housekeeping_Genes_Mouse.RData")
colnames(Mouse_HK_genes)[2]="Ensembl"
Mouse_HK_genes = mutate(Mouse_HK_genes, choice=paste0(Gene, " ", "(", Ensembl, ")"))

                            