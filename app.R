#
#if (!requireNamespace("BiocManager", quietly=TRUE))
#  install.packages("BiocManager")

#
#BiocManager::install()
#chooseCRANmirror()

install.packages("pkgbuild")
install.packages("devtools")
library(pkgbuild) 
library(devtools)
#devtools::install_github("KechrisLab/multiMiR")


library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinybusy)
#library(multiMiR)
library(DT)
library(ngram)
library(easyPubMed)
library(pubmed.mineR)
library(lsa)
library(gdata)
library(stringr)
library(org.Hs.eg.db)
library(rclinicaltrials)
library(GOSim)
library(miRBaseConverter)
library(cgdsr)





ui <- dashboardPage(
                     
)          
                        

                


server <- function(input, output, session) { 
  
}

shinyApp(ui, server)