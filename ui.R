# ----------------------------------------------------------------------------------------

# Interface do Usuário 

# ----------------------------------------------------------------------------------------

ui = shiny::htmlTemplate(
  # Index Page
  "www/index.html",
  
# ----------------------------------------------------------------------------------------
  
  # Entrada de texto para nome do beneficiário
  texto = textInput(inputId = "username",
                    label= "Insira seu nome:"),

# ----------------------------------------------------------------------------------------
  
  # Botão de entrada do usuário
  botao = actionButton(inputId = "submit",
                       label= "Enviar",
                       icon("paper-plane"), 
                       style="color: #fff; border-radius: 50%; height:50px; font-size:60%; background-color: #0a7927"),

# ----------------------------------------------------------------------------------------

  total = textOutput("value"),

# ----------------------------------------------------------------------------------------

  # Mostra tabela
  tabela = DT::dataTableOutput('checkbox_table'),

# ----------------------------------------------------------------------------------------

  # Botão de relatório
  relatorio = downloadButton("report",
                             "Recibo",
                             style = "color: #fff; border-radius: 50%; height:50px; font-size:60%; background-color: #0a7927")

)
