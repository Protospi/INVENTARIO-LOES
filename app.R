
# -----------------------------------------------------------------------

# Shiny App Inventário Casa da Vovó

# -----------------------------------------------------------------------

# Load Packages
library(shiny)
library(DT)
library(readxl)
library(tidyverse)
library(googledrive)
library(rdrop2)

# Function to link photo in table
createLink <- function(val1, val2) {
  temp <- rep("a", 361)
  for( i in 1:361){
    temp[i] <- paste('<a href=',
                     val1[i],
                     ' target="_blank"><img src=',
                     val2[i],' height="100"></a>')
  }
  return(temp)
}

# -----------------------------------------------------------------------

# Drop box foldes
outputDir <- "responses"

# Define function to save data to dropbox
saveData <- function(data, nome) {
  fileName <- paste0(nome, ".csv")
  filePath <- file.path(tempdir(), fileName)
  write.csv(data, filePath, row.names = FALSE, quote = TRUE)
  drop_upload(filePath, path = outputDir, dtoken = token)
}

# -----------------------------------------------------------------------

# Load token
token <- readRDS("droptoken.rds")

# -----------------------------------------------------------------------

# Function to load shiny application
shinyApp(
  
  # Declare user interface
  ui =
    
    # Function to call fluid page
    fluidPage(
      
      # Define html style
      tags$style(
        HTML(
          ".error {
                    background-color: red;
                    color: white;
                    }
                    .success {
                    background-color: green;
                    color: white;
                    }"
        ),
        "input[type=checkbox] {
                    transform: scale(1.7);
           }"),
      
      tags$link(rel = "stylesheet", href = "style.css"),
      tags$div(class="windows",tags$img(src='frente2.jpg',
                                               width = 500)),
      
      # Print title
      h2("Inventário Casa Lóes",  align = "center"),
      
      # Insere imagens capa
      HTML('<center><img src="frente2.jpg" width="500" height="250"></center>'),
      
      # Quebras de linha
      br(),br(),
      
      # Imprime instruções
      h4("Por favor marque o checkbox para os items de seu interesse na coluna Interesse."),
      
      # Quebras de linha
      br(),br(),
      
      # Mostr a tabela
      DT::dataTableOutput('checkbox_table'),
      
      # Quebras de linha
      br(),br(),
      
      column(4,
      
         # Entrada de texto para nome do beneficiário
         textInput(inputId = "username",
                   label= "Por favor insira seu primeiro nome"),
         
         # Quebras de linha
         br(),
         
         # Botão de entrada do usuário
         actionButton(inputId = "submit",
                      label= "Envie",
                      icon("paper-plane"), 
                      style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
         
         # Quebras de linha
         br(),br(),
         
         verbatimTextOutput("value"),
         
         # Menssagem final
         h4("Obrigado!")       
             
      ),
      
      column(8,
           
         # Insere imagens capa
         HTML('<center><img src="sala_branca2.jpg"  height="250"></center>')  
             
      )
      
    # Close user Fluid page
    ),
  
  # Call server function
  server = function(input, output, session) {
    
    # Load data and links
    inventario <- read_csv("inventario_final2.csv") %>%
      filter(Id < 362) %>% 
      mutate(`Preço` = rep(1000, length(Id))) %>% 
      relocate(c(1:5,8, 6, 7))
    
    imagens <- read.csv("links_arranged2.csv") %>%  pull(links)
    
    thumbnails <- paste0("https://raw.githubusercontent.com/Protospi/Inventario-Casa/main/", 1:361, ".jpg")
    
    links <- createLink(imagens, thumbnails)
    
    # Declare answer options
    answer_options <- as.character(inventario$Id)
    
    # Declare a data table with checkbox
    shinyInput <- function(FUN, ids, ...) {
      inputs <- NULL
      inputs <- sapply(ids, function(x) {
        inputs[x] <- as.character(FUN(inputId = x, label = NULL, ...))
      })
      inputs
    }
    
    # Declara dataframe com checkbox
    df <- inventario %>% 
            mutate(Interesse = shinyInput(checkboxInput, answer_options),
                   Foto = links)

    
    output$value <- renderText({ 
      
      total <- inventario %>% 
        mutate(interesse = sapply(answer_options,
                                  function(i) input[[i]],
                                  USE.NAMES = FALSE)) %>% 
        filter(interesse) %>% 
        summarize(Total = sum(`Preço`))
      
       total$Total
      
      })
    
    # Declare a data table
    output$checkbox_table <- DT::renderDataTable(
      df,
      server = FALSE,
      escape = FALSE,
      selection = 'none',
      rownames = FALSE,
      options = list(
        dom = 't', paging = FALSE, ordering = FALSE,
        columnDefs = list(list(className = 'dt-center',
                               targets = 0:6),
                          list(className = 'dt-left', targets = 7)),
        preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
        drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
      )
    )
    
    # Observe event to save user inputs
    observeEvent(input$submit, {
      
      # Condition to input name
      if(input$username == "") {
        showModal(modalDialog(
          "Por favor insira seu primeiro nome", 
          easyClose = TRUE,
          footer = NULL,
          class = "error"
        ))
        
      } else {
        
        # Declare data frame to save the answers
        responses <- data.frame(usuario = input$username,
                                item = answer_options,
                                interesse = sapply(answer_options,
                                                   function(i) input[[i]],
                                                   USE.NAMES = FALSE))
        
        # Salva dados
        saveData(responses, input$username)
        
        # Message to show the user responses success sent
        showModal(modalDialog("Enviado com Sucesso",
                              easyClose = TRUE,
                              footer = NULL,
                              class = "success")) 
        
        # Reset checkboxes
        sapply(answer_options, function(x) updateCheckboxInput(session, x, value = FALSE))
        updateTextInput(session, "username", value = "")
      }
    })
  }
)