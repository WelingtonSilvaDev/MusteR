# ----------- Interface MusteR 2022 ----------------------
#    __  __           _       _____  
#   |  \/  |         | |     |  __ \ 
#   | \  / |_   _ ___| |_ ___| |__) |
#   | |\/| | | | / __| __/ _ \  _  / 
#   | |  | | |_| \__ \ ||  __/ | \ \ 
#   |_|  |_|\__,_|___/\__\___|_|  \_\
# Created by Welington Goncalves Silva
# Last update on november 23, 2022
# My github: https://github.com/WelingtonSilvaDev

# RECOMENDA-SE que o zoom (ctrl +) do browser seja de 125% 
install.packages("pacman") # pacman package install libraries if they are not installed yet
pacman::p_load(c("flexclust", "tidyverse", "doMC", "bio3d", "Rpdb","rmarkdown", "pracma","geometry",
                 "deldir", "caret", "graphkernels", "shape", "rARPACK", "mongolite", "tnet", 
                "markdown", "DT", "shiny", "shinythemes", "shinycssloaders", "dplyr","r3dmol",
                "plotly","bslib")) #list of libraries used

source("main-ligs.R")
source("show3d/3dmol.R")

library(shiny)
library(bslib)
library(rmarkdown)
library(shinythemes)
library(DT)
library(shinycssloaders)
library(dplyr)
library(r3dmol)
library(plotly)
 
options(spinner.color = "#0275D8", spinner.color.background = "#ffffff", spinner.size = 2)

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "yeti", base_font = font_google("Montserrat")), # united, yeti,zephyr ou cerulean
  navbarPage(
    title = "MusteR",
    id = "navbarID",
    # imageOutput("home_img",   #isn't being used
    #             width = "100%",
    #             height = "60%",
    # ),
    tabPanel(
      title = "Home",
      splitLayout(
        sidebarPanel(
          width = "100%",
          tags$style(type="text/css", "
           #loadmessage {
             position: fixed;
             top: 700px;
             left: 0px;
             width: 100%;
             padding: 5px 0px 5px 0px;
             text-align: center;
             font-weight: bold;
             font-size: 100%;
             color: #000000;
             background-color: #00FA9A;
             z-index: 105;
           }
          "),
          conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                           tags$div("Loading...",id="loadmessage")
          ),
          h2(strong("Welcome to Muster!")),
          h4(strong("Fill in the table below separating by comma")),
          br(),
          #tags$blockquote("You can type more than one line, just pressing enter."), #descomentar quando quiser mutiplas entradas
          tags$hr(), br(), br(), #apagar os br() para diminuir espacamento vertical
          #textAreaInput("file1", "PDB Name,Ligand Name,Ligand ID", height = "100px", placeholder = "Exemple:\n7NF5,ALD,401\n6XQU,U5G,401"), #descomentar para multiplas entradas
          textInput("file1", "PDB Name,Ligand Name,Ligand ID", placeholder = "Exemple:\n7NF5,ALD,401"), 
          br(),br(),
          tags$style(type = "text/css", "#file2 { margin-top: -3px }"),
          actionButton("action", "Submmit", class = "btn-success"), h5(strong("After clicked on the button, wait to be redirectioned")),h6(strong("(it can take some minutes))"))
        ),
        sidebarPanel(
          width = "100%",
          h4(strong("OR"), align = "left"),
          h4(strong("Insert the csv table")), br(),
          tags$blockquote("You can upload a .csv file in the field below."),
          hr(),br(),
          fileInput("filecsv", "The right formatation can be seen in \"More\">>\"CSV Table Formatation\"",
                    #multiple = FALSE, #alterar para TRUE caso desejar multiplas entradas
                    multiple = FALSE,
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv"
                    ),
                    width = NULL,
                    buttonLabel = "Browse...",
                    placeholder = "No file selected"
          ),
          br(),
          actionButton("upcsv", "Submmit", class = "btn-success"), h5(strong("After clicked on the button, wait to be redirectioned")),h6(strong("(it can take some minutes))"))
        ) 
      ), 
    ),
    # tabPanel( #descomentar para caso desejar obter a opcao COMPLEX TABLE
    #   title = "Complex Table",
    #   value = "complextable",
    #   withSpinner(DT::dataTableOutput("to"), type = 3)
    # ),
    tabPanel(
      title = "Interactive Map",
      value = "Mapid",
      sidebarLayout(position = "left", 
          sidebarPanel(h6(strong("Molecular Visualization")), width = 2),
          mainPanel(
                fluidRow(
                   column(6,withSpinner(r3dmolOutput("graph1", width = "auto", height = "400px"), type = 3)),  
                   column(6,withSpinner(plotlyOutput("graph2", width = "auto", height = "auto"), type = 3))
                        )
                    )
      ),
      h6(strong("*Tags Map csv file was successfully saved at folder \"tag_out/tags.csv\""))
  
    ),
    navbarMenu(
      "More",
      tabPanel(
        "CSV Table Formatation",
        sidebarPanel(
          width = 20,
          h4(strong("CSV Table Formatation")),
          tags$blockquote("The input table must follow this exacly formatation:"),
          hr(),
          DT::dataTableOutput("toexample")
        ),
      ),
      tabPanel(
        title = "About",
        # Markdown sobre o MusteR
        includeMarkdown("md/about.md"),
      )
    ),
    inverse = T
    

  ),
)


server <- function(input, output, session) {
  output$toexample <- DT::renderDataTable({
    filex <- read.csv("LIGS/pdb_table_test.csv")
  })
  
  observeEvent(input$action, {
    updateTabsetPanel(session, "navbarID",
                      selected = "Mapid")
  })
  observeEvent(input$upcsv, {
    updateTabsetPanel(session, "navbarID",
                      selected = "Mapid")
  })  
  
  observeEvent(input$action, {
    file_df <- read.table(text = gsub(" ","",input$file1), sep = ",", header = FALSE, fileEncoding = "UTF-8")
    
    colnames(file_df) <- c("pdb_title", "lig1n", "lig1id")
    file_df <- data.frame(lapply(file_df, function(v) {
      if (is.character(v)) {
        return(toupper(v))
      } else {
        return(v)
      }
    }))
    write.csv(file_df, "file_df.csv", row.names = FALSE)
    backend_MusteR("file_df.csv")
    write_csv(m.ligs1, file = "tags_out/tags.csv")
  })

  observeEvent(input$upcsv, {
    file_csv <- read.table(file = gsub(" ","",input$filecsv$datapath), sep = ",", header = TRUE, fileEncoding = "UTF-8")
    colnames(file_csv) <- c("pdb_title", "lig1n", "lig1id")
    file_csv <- data.frame(lapply(file_csv, function(v) {
      if (is.character(v)) {
        return(toupper(v))
      } else {
        return(v)
      }
    }))
    write.csv(file_csv, "file_df.csv", row.names = FALSE)
    backend_MusteR("file_df.csv")
  })
  
  observeEvent(input$action, {
    output$to <- DT::renderDataTable({
      read.csv("file_df.csv")
    })
  })

  observeEvent((input$action || input$upcsv), {
    output$graph1 = renderR3dmol({
      mol_name = list.files(path = "pdb-mol/", pattern = ".pdb", all.files = T)
      mol3d(paste0(lig.table$pdb_title, ".pdb"), lig.table$lig1id)

    })
    output$graph2 = renderPlotly({
      p.ligs1
    })
  })
  

}

shinyApp(ui = ui, server = server)
