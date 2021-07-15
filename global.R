# ---------------------------------------------------------------------------------------------

# Ambiente Global Inventário Casa Loes

# ---------------------------------------------------------------------------------------------

# Biblioteca

# ---------------------------------------------------------------------------------------------

# Pacotes
library(dplyr)
library(ISLR)
library(tree)
library(readr)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(RColorBrewer)
library(highcharter)
library(shinyWidgets)
library(shinycssloaders)
library(plotly)
library(reshape2)
library(tidyr)
library(rlist)
library(hrbrthemes)
library(caret)
library(janitor)
library(shinycssloaders)
library(sp)
library(shinymaterial)
library(e1071)
library(htmltools)
library(shinyjs)
library(DT)
library(readxl)
library(tidyverse)
library(googledrive)
library(rdrop2)
library(png)

# ---------------------------------------------------------------------------------------------

# Funções

# ---------------------------------------------------------------------------------------------

# Function to link photo in table
createLink <- function(val1, val2) {
  temp <- rep("a", 809)
  for( i in 1:809){
    temp[i] <- paste('<a href=',
                     val1[i],
                     ' target="_blank"><img src=',
                     val2[i],' height="100"></a>')
  }
  return(temp)
}

# ---------------------------------------------------------------------------------------------

# Carrega Token
token <- readRDS("droptoken.rds")

# ---------------------------------------------------------------------------------------------

# Declara pasta do drop box
outputDir <- "responses"

# Define função para salvar no dropbox
saveData <- function(data, nome) {
  fileName <- paste0(nome, ".csv")
  filePath <- file.path(tempdir(), fileName)
  write.csv(data, filePath, row.names = FALSE, quote = TRUE)
  drop_upload(filePath, path = outputDir, dtoken = token)
}

# ---------------------------------------------------------------------------------------------
