# ------------------------------------------------------------------------------------------

# Servidor

# ------------------------------------------------------------------------------------------

# Call server function
server = function(input, output, session) {
  
  # Carrega Inventario
  inventario <- read_csv("inventario.csv") %>%
    filter(Id < 810) %>% 
    mutate(`Preço` = rep(1000, length(Id)),
           Interesse = rep(FALSE, length(Id))) %>% 
    relocate(c(1:5,8, 6, 7))
  
  # Carrega Links ordenados
  imagens <- read.csv("links_fotos.csv") %>%  pull(links)
  
  # Declara endereços thumbnail do git
  thumbnails <- paste0("https://raw.githubusercontent.com/Protospi/INVENTARIO-LOES/main/fotos_final/", 1:809, ".jpg")
  
  # Gera links com as imagens
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
  
  
  # Renderiza total de itens escolhidos
  output$value <- renderText({ 
    
    # Calcula total de itens selecionados
    total <- inventario %>% 
                mutate(interesse = sapply(answer_options,
                                          function(i) input[[i]],
                                          USE.NAMES = FALSE)) %>% 
      filter(interesse == 1) %>% 
      summarize(Total = sum(`Preço`)) 
    
    paste0("Total = R$ ", total$Total)
    
  })
  
  # Renderiza a data table
  output$checkbox_table <- DT::renderDataTable(
    df,
    server = FALSE,
    escape = FALSE,
    selection = 'none',
    rownames = FALSE,
    options = list(
      pageLength = 5, scrollY = "250px",
      dom = 't', paging = FALSE, ordering = FALSE,
      columnDefs = list(list(className = 'dt-center',
                             targets = 0:7),
                        list(className = 'dt-left', targets = 7)),
      preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
      drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
    )
  )
  
  # Observa evento de entrada do primeiro nome
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
      
      # Declara data frame para salvar as respostas
      responses <- data.frame(usuario = input$username,
                              item = answer_options,
                              interesse = sapply(answer_options,
                                                 function(i) input[[i]],
                                                 USE.NAMES = FALSE))
      
      # Salva dados com nome do cliente
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
  
  # Declara escolha do cliente
  escolha <- reactive({
    
    # Declara dataframe com checkbox
    recibo <- inventario %>% 
                mutate(interesse = sapply(answer_options,
                       function(i) input[[i]],
                       USE.NAMES = FALSE))
    
    # Filtra recibo
    recibo %>% 
      filter(interesse) %>% 
      select(1:6)
    
  })
  
  # Salva pdf com relatorio do recibo de cliente
  output$report <- downloadHandler(
    filename = paste0(format(Sys.time(), format='RECIBO_INVENTARIO_CASA_LOES_DATA_%d_%m_%Y_HORA_%H_%M'), ".pdf"),
    content = function(file) {
      imagem <- readPNG("www/images/piscina.png")
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      params <- list(table1 = escolha(), img = imagem, usuario = input$username)
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
}
