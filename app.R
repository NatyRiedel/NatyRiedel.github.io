
library(BiocManager)
options(repos = BiocManager::repositories())

library(multiMiR)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinybusy)
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





choicesgen<- c("PTEN", "AFP", "STAT3", "SIRT7", "GC", "XPO5", "HMGA2", "VEGFA", "FOXO1",  "ZEB1", "PDCD4", "GPC3", "EGFR", "SIRT1", "MET", "DNMT1", "IGF1R", "TET1", "CDC42", "KLF5", "GRN", "FOXA1", "MACC1","SOCS1", "TP53", "E2F1", "E2F3", "ROCK1", "SP1",  "CTGF", "DICER1", "HGF", "ROCK2", "BMI1", "CDK1", "FUT8", "METTL3", "PIK3R3", "AKAP12", "AURKA", "CTNNB1", "EGFL7", "HDAC6", "IGF2", "IGF2BP1", "SMAD2", "SOCS3", "UBE2I", "VDAC1", "ZEB2", "ABCB1", "AKT2","CCNE1", "FBXO11")
choicesmutation<-c("AFP","DICER1", "GNMT", "CREB1", "ADI1", "CYP2C9", "SIRT7", "UGT2B4", "METTL3", "BMI1", "CTNNB1", "PPP1R15B", "PTEN", "TP53", "UGT2B7", "TERT", "XPO5", "AGA", "CAV1", "CEP55", "PBX3", "ADAMTS4", "CCNE1", "CDC25A", "FGF19", "ZC3H13", "CSMD3", "CTHRC1", "E2F7", "IGF2BP1", "MET", "NRAS", "PCNA", "RHEB", "RICTOR", "TG", "TSC1", "TSC2", "VEGFA")
choicesmiarn<-c("mir-122", "mir-21", "mir-221","mir-125b", "mir-34a", "mir-26a", "mir-148a", "mir-224", "mir-101", "mir-145", "mir-155", "mir-146a", "mir-375", "mir-7","mir-195", "mir-214", "mir-200a", "mir-124", "mir-16", "mir-223", "mir-9", "mir-203", "mir-18a",  "mir-29", "mir-139", "mir-181a", "mir-29a", "mir-126", "mir-141", "mir-106b", "mir-23a", "mir-199a-3p", "mir-338-3p", "mir-1", "mir-22", "mir-143", "mir-499", "mir-100", "mir-130b", "mir-638", "mir-222", "mir-503", "mir-125a", "mir-449a")
new_query<-c("(hepatocellular carcinoma[TIAB] OR HCC [all fields] OR hepatocarcinoma cells[TIAB]) AND (microRNA[TIAB] OR miARN[TIAB] OR microRNAs[TIAB]) AND (2000:2020/09[dp])")
titulo<-tags$img(src="logo.png", heigth=110, width= 110)
entrez_id<-select(org.Hs.eg.db, 
                  keys = choicesgen,
                  columns = c("ENTREZID", "SYMBOL"),
                  keytype = "SYMBOL")




ui <- dashboardPage(
  skin="purple",
  dashboardHeader(
    tags$li(
        class = "dropdown"
        ,tags$style(".main-header {max-height: 70px}")
        ,tags$style(".main-header .logo {height: 70px}")
    ),

    title = titulo

  ),
  dashboardSidebar(
    sidebarMenu(
      br(),
      menuItem("Genes", tabName = "Genes", icon = icon("dna"), startExpanded = "TRUE", 
        menuSubItem("Publicaciones en Pubmed", tabName = "pubmed1"),
        menuSubItem("GO", tabName = "GO1"),
        menuSubItem("Clinical Trials", tabName = "clinical")),
      menuItem("miARN", tabName = "miARN", icon = icon("vial"), startExpanded = "TRUE",
        menuSubItem("Publicaciones en Pubmed", tabName = "pubmed2"),
        menuSubItem("miRData", tabName = "miRData")),
      menuItem("Mutaciones", tabName = "Mutaciones", icon = icon("atom"), startExpanded = "TRUE",
        menuSubItem("Publicaciones en Pubmed", tabName = "pubmed3"),
        menuSubItem("My Cancer Genome", tabName = "cancer")),
      menuItem("Entorno HCC", tabName = "mapa", icon = icon("sitemap"), startExpanded = "TRUE")
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$style(type = "text/css",
      ".shiny-output-error{visibility: hidden; }",
      ".shiny-output-error:before{visibility:hidden; }"
    ),
    tabItems(
      tabItem(tabName = "pubmed1",
        h2("Busqueda de Genes en Pubmed"),
        htmlOutput("genes"),
        br(),
        selectInput("pubmeda", "Selecciona un Gen", choices = choicesgen),
        actionButton(inputId = "pub1", label = "Buscar", icon = icon("search"), block = FALSE),
        add_busy_bar(color = "white"),
        br(),
        hidden(actionButton("nb", label = "Nueva Busqueda", block = FALSE)),
        br(),br(),
        tags$div(style= 'cursor:pointer',DT::dataTableOutput("resultados1")),
        br(),
        wellPanel(htmlOutput("panelPublicacionTMP"))
      ),
      tabItem(tabName = "pubmed2",
        h2("Busqueda de miARN en Pubmed"),
        htmlOutput("miarn"),
        br(),
        selectInput("pubmedb", "Selecciona un miARN", choices = choicesmiarn),
        actionButton(inputId = "pub2", label = "Buscar", icon = icon("search"), block= FALSE),
        add_busy_bar(color = "white"),
        br(),
        hidden(actionButton("nb1", label = "Nueva Busqueda", block = FALSE )),
        br(),br(),
        tags$div(style= 'cursor:pointer',DT::dataTableOutput("resultados2")),
        br(),
        wellPanel(htmlOutput("panelPublicacionTMPS"))
      ),
      tabItem(tabName = "pubmed3",
        h2("Busqueda de mutaciones en Pubmed"),
        br(),
        actionButton(inputId = "pub3", label = "Buscar", icon = icon("search"), block= FALSE),
        add_busy_bar(color = "white"),
        br(),
        hidden(actionButton("nb2", label = "Nueva Busqueda", block = FALSE)),
        br(),br(),
        tags$div(style= 'cursor:pointer',DT::dataTableOutput("resultados3")),
        br(),
        wellPanel(htmlOutput("panelPublicacion2"))
      ),
      tabItem(tabName = "GO1",
        h2("Ontologia de Genes"),
        h3("Referencias de Identificadores",style = "text-align: right;"),
        fluidPage(
          fluidRow(
            tags$head(tags$style(HTML(".small-box {height: 250px}"))),
            valueBox(
              h2("Identificadores Entrez"), width = 5, color = "purple",
              div( selectInput(inputId = "radio", label = "Selecciona un gen", choices = entrez_id$ENTREZID))
            ),
            valueBox(tags$div(img(src = "referencias.png", height = "230", width = "550"), style = "text-align: center;"),color = "aqua", width = 7 ,subtitle = "Referencia")
          )
        ),
        br(),
        add_busy_bar(color = "white"),
        fluidRow(
          tabsetPanel(
            tabPanel(title="Resultados GO", width = 12,
              tags$div(style= 'cursor:pointer',DT::dataTableOutput("resultadosgo"))
            ),
            tabPanel(title= "Similitud Genes", width = 12,
              h2("Los genes deben ir con el identidicador Entrez"),
              div(textInput(inputId = "input1", label = "Escribe el primer gen"),
                div(textInput(inputId = "input2", label = "Escribe el segundo gen"),
                tags$div(style = "text-align: right;", tableOutput("simgen")))
              )
            )
          )
        )
      ),
      tabItem(tabName = "cancer",
        h2("Muestras de mutaciones"),
        fluidPage(
          fluidRow(
            tags$head(tags$style(HTML(".small-box {height: 250px}"))),
            valueBox(h2("Identificadores Entrez"), width = 6, color = "purple",
              div( selectInput("mutacion", "Selecciona un gen", choices = choicesmutation))
            )
          )
        ),
        br(),
        add_busy_bar(color = "white"),
        fluidRow(
          tabBox(title="Resultados", width = 12,
            tags$div(style= 'cursor:pointer',DT::dataTableOutput("publicacionmut"))
          )
        )
      ),
      tabItem(tabName = "mapa",
        h2("Informacion obtenida de la Enciclopedia de Genes y Genomas de Kioto (KEGG)"),
        tags$div(img(src = "hsa05225.png", height = 890, width = 1060)),
        wellPanel(uiOutput("mapakegg"))
      ),
      tabItem(tabName = "miRData",
        h2("Información correspondiente a miARN"),
        fluidPage(
          fluidRow(
            tags$head(tags$style(HTML(".small-box {height: 250px}"))),
            valueBox(h2("A partir de un gen elegido, se muestra todos los miARN que se dirigen a el"), width = 6, color = "purple",
              div( selectInput("microgen", "Selecciona un gen", choices = choicesgen))
            ),
            valueBox(h2("A partir de un miARN, se muestra todos los genes dianas a los que se dirigen"), width = 6, color = "aqua",
              div(textInput(inputId = "inputarn", label = "Escribe el miARN", value = "hsa-miR-338-3p"))
            )
          )
        ),
        br(),
        add_busy_bar(color = "white"),
        fluidRow(tabsetPanel(
          tabPanel(title="Resultados ARN", width = 12,
          tags$div(style= 'cursor:pointer',DT::dataTableOutput("publicacionesarn"))),
          tabPanel(title= "Genes Diana", width = 12,
            tags$div(style= 'cursor:pointer',DT::dataTableOutput("publicaciondiana")))
          )
        )
      )
    )
  )
)          





server <- function(input, output, session) { 
  
  #Busqueda Pubmed 
  ##Pubmed genes
  
  PubmedResult<-eventReactive(input$pub1,{
    PM1 <- batch_pubmed_download(
      pubmed_query_string = paste(c(input$pubmeda, " [MH] AND" , new_query),collapse = ""),
      format = "abstract",
      batch_size = 5000,
      dest_file_prefix = "pubmed_"
    )
    pmr<-readabs("pubmed_01.txt")
  })
  output$resultados1<-renderDataTable({
    corpus1<- PubmedResult()
    pmres1<-data.frame(corpus1@PMID, corpus1@Journal)
    colnames(pmres1)<-c("Nro. Identidicador", "Revista")
    datatable(pmres1, selection=list(mode="single", selected = c(1)
    ), options = list(language= list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),pageLength = 5))
  })
  output$panelPublicacionTMP<-renderPrint({
    s = input$resultados1_row_last_clicked
    if (length(s)) {
      corpus1<-readabs("pubmed_01.txt")
      abstract <- corpus1@Abstract[s]
      id <- corpus1@PMID[s]
      link <- a(
        "Ver Publicacion en PubMed"
        ,href= gsub(
          " "
          ,""
          ,paste(
            "https://pubmed.ncbi.nlm.nih.gov/"
            ,corpus1@PMID[s]
          )
          , fixed = TRUE)
        , target="_blank"
      )
      cat(
        abstract
      )
      cat(
        
        paste("<br>", link)
      )
    }
  })
  observeEvent(input$pub1,{
    hide("pubmeda")
    hide("pub1")
    hide("panelPublicacion")
    shinyjs::toggle("nb")
    
  })
  observeEvent(input$nb,{
    shinyjs::toggle("pubmeda")
    shinyjs::toggle("pub1")
    hide("nb") 
    show("panelPublicacion")
  })
  output$genes<-renderPrint({
    cat('<h3> Publicaciones sobre <font style="text-trasform: capitalize";>',input$pubmeda,'</font></h3>')
  })
  
  ##Pubmed miARN
  PubmedResult1<-eventReactive(input$pub2,{
    PM1 <- batch_pubmed_download(
      pubmed_query_string = paste(c(input$pubmedb, " [MH] AND" , new_query),
      collapse = ""),
      format = "abstract",
      batch_size = 5000,
      dest_file_prefix = "pubmed1_"
    )
    pmr1<-readabs("pubmed1_01.txt")
  })
  output$resultados2<-renderDataTable({
    corpus2<- PubmedResult1()
    pmres2<-data.frame(corpus2@PMID, corpus2@Journal)
    colnames(pmres2)<-c("Nro. Identidicador", "Revista")
    datatable(pmres2, selection=list(mode="single", selected = c(1)
    ), options = list(language= list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),pageLength = 5))
  })
  output$panelPublicacionTMPS<-renderPrint({
    s1 = input$resultados2_row_last_clicked
    if (length(s1)) {
      corpus2<-readabs("pubmed1_01.txt")
      abstract <- corpus2@Abstract[s1]
      id <- corpus2@PMID[s1]
      link <- a(
        "Ver publicacion en PubMed"
        ,href= gsub(
          " "
          ,""
          ,paste(
            "https://pubmed.ncbi.nlm.nih.gov/"
            ,corpus2@PMID[s1]
          )
          , fixed = TRUE)
        , target="_blank"
      )
      cat(
        abstract
      )
      cat(
        
        paste("<br>", link)
      )
    }
  })
  observeEvent(input$pub2,{
    hide("pubmedb")
    hide("pub2")
    hide("panelPublicacion1")
    shinyjs::toggle("nb1")
  })
  observeEvent(input$nb1,{
    shinyjs::toggle("pubmedb")
    shinyjs::toggle("pub2")
    hide("nb1") 
    show("panelPublicacion1")
  })
  output$miarn<-renderPrint({
    cat('<h3> Publicaciones sobre <font style="text-trasform: capitalize";>',input$pubmedb,'</font></h3>')
  })
  ##Pubmed mutaciones
  PubmedResult2<-eventReactive(input$pub3,{
    PM2 <- batch_pubmed_download(pubmed_query_string = paste(c("mutation AND" , new_query),
                                                             collapse = ""),
                                 format = "abstract",
                                 batch_size = 5000,
                                 dest_file_prefix = "pubmed2_"
    )
    pmr2<-readabs("pubmed2_01.txt")
  })
  output$resultados3<-renderDataTable({
    corpus3<- PubmedResult2()
    pmres3<-data.frame(corpus3@PMID, corpus3@Journal)
    colnames(pmres3)<-c("Nro. Identidicador", "Revista")
    datatable(pmres3, selection=list(mode="single", selected = c(1)
    ), options = list(language= list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),pageLength = 5))
  })
  
  
  output$panelPublicacion2<-renderPrint({
    
    s2 = input$resultados3_row_last_clicked
    if (length(s2)) {
      corpus3<-readabs("pubmed2_01.txt")
      abstract <- corpus3@Abstract[s2]
      id <- corpus3@PMID[s2]
      link <- a(
        "Ver publicacion en PubMed"
        ,href= gsub(
          " "
          ,""
          ,paste(
            "https://pubmed.ncbi.nlm.nih.gov/"
            ,corpus3@PMID[s2]
          )
          , fixed = TRUE)
        , target="_blank"
      )
      cat(
        abstract
      )
      cat(
        
        paste("<br>", link)
      )
      
    }
    
  })
  
  #GO
  
  output$resultadosgo<-renderDataTable({input$radio
    x<-as.data.frame(t(getGOInfo(input$radio)))
    colnames(x)<-c("ID GO", "Termino", "Definicion", "IC")
    datatable(x, selection=list(mode="single", selected = c(1)
    ), options = list(language= list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),pageLength = 5))
    
  })
  
  output$referencias<-renderTable({
    y<-select(org.Hs.eg.db, 
              keys = choicesgen,
              columns = c("ENTREZID", "SYMBOL"),
              keytype = "SYMBOL")
  })
  
  output$simgen<-renderTable({
    getGeneSim(c(input$input1,input$input2),similarity="OA",similarityTerm="Lin",avg=FALSE, verbose= FALSE)
  })
  
  #KEGG
  
  
  hiperlink <- a("Ver detalles del mapa en la pagina web de KEGG",href=
                   "https://www.genome.jp/dbget-bin/www_bget?pathway+hsa05225")
  
  
  output$mapakegg<-renderUI({
    tagList("URL link:", hiperlink)
    
  })
  
  
  #Mutaciones
  
  output$publicacionmut <- renderDataTable({
    mycgds = CGDS("http://www.cbioportal.org/")
    
    y= getMutationData(mycgds, caseList = "lihc_tcga_all",geneticProfile = "lihc_tcga_mutations", genes = input$mutacion)
    
    tabla_mut<-y[ ,c(-3:-5,-7, -9:-12,-14-15,-18:-22)]
    colnames(tabla_mut)<-c("Entrez ID", "Gen", "Tipo de mutacion", "Cambio en el aa.", "Nro Chr", "Posicion Inicio", "Posicion Final", "Referencia", "Variante")
    datatable(y, selection=list(mode="single", selected = c(1)
    ), options = list(language= list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),pageLength = 5))
    tabla_mut
  })
  #miARN
  output$publicacionesarn <- renderDataTable({
      example3 <- get_multimir(org = "hsa",
      target  = input$microgen,
      table   = "predicted",
      summary = TRUE,
      predicted.cutoff = 35,
      predicted.cutoff.type = "p",
      predicted.site = "all"
    )
    tablamir<-example3@data[ ,c(-1,-2,-5:-6)]
      colnames(tablamir)<-c("miARN involucrado", "Gen", "Score", "Tipo")
      datatable(tablamir, selection=list(mode="single", selected = c(1)
      ), options = list(language= list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),pageLength = 5))
      tablamir
  })
  output$publicaciondiana<-renderDataTable({ 
    genesDiana<- get_multimir(mirna = input$inputarn, summary = TRUE)
    tablagen<-genesDiana@data[ ,c(-2,-5,-6,-8)]
    colnames(tablagen)<-c("Base de datos", "miARN involucrado", "Gen", "Tecnica", "ID Pubmed","Tipo")
    datatable(tablagen, selection=list(mode="single", selected = c(1)
    ), options = list(language= list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),pageLength = 5))
    tablagen 
  })
}

shinyApp(ui, server)