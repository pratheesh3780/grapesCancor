
######## List of required packages for appearance

library(shiny)
library(shinyWidgets)
library(rmarkdown)
library(shinycssloaders)
library(dplyr)

################################################## Ui
ui <- fluidPage(
  # Background colour
  setBackgroundColor(
    color = c("#f5ffcf", "#ffffff"),
    gradient = "radial",
    direction = c("bottom", "right")
  ),
  # Title Panel and colour
  titlePanel(tags$div(tags$b("Canonical Correlation"), style = "color:#000000")),

  # Entries in sidebar panel

  sidebarPanel(
    fileInput("file1", "CSV File (upload in csv format)",
      accept = c(
        "text/csv",
        "text/comma-separated-values,text/plain",
        ".csv"
      )
    ),
    checkboxInput("header", "Header", TRUE),
    uiOutput("var"),
    tags$br(),
    h5(
      tags$div(
        "Developed by:",
        tags$br(),
        tags$b("Dr. Pratheesh P. Gopinath"),
        tags$br(),
        tags$b("Assistant Professor,"),
        tags$br(),
        tags$b("Agricultural Statistics,"),
        tags$br(),
        tags$b("Kerala Agricultural University"),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$b("Sandra M.M."),
        tags$br(),
        tags$b("MSc Agricultural Statistics"),
        tags$br(),
        tags$b("Kerala Agricultural University"),
        tags$br(),
        tags$br(),
        h3(),
        "post your queries at: pratheesh.pg@kau.in",
        style = "color:#343aeb"
      )
    ),
    conditionalPanel(
      "$('#corrmat1').hasClass('recalculating')",
      tags$div(tags$b("Loading ...please wait while calculation is going on in the background.....please dont press submit button again "), style = "color:green")
    )
  ),
  mainPanel(
    tabsetPanel(
      type = "tab",
      tabPanel(
        "Analysis.Results",
        conditionalPanel(
          "$('#corrmat1').hasClass('recalculating')",
          tags$div(tags$b("Loading ...please wait while calculation is going on in the background.....please dont press submit button again "), style = "color:green")
        ),
        uiOutput("note1"),
        tags$br(),
        uiOutput("data_set"),
        tags$br(),
        uiOutput("note2"),
        tableOutput("corrmat1"),
        uiOutput("note4corrmat"),
        tags$br(),
        tableOutput("wilks"),
        tags$br(),
        tableOutput("cancorr"),
        tags$br(),
        tableOutput("rawU"),
        tags$br(),
        tableOutput("rawV"),
        tags$br(),
        tableOutput("load1"),
        tags$br(),
        tableOutput("load2"),
        tags$br(),
        tableOutput("load3"),
        tags$br(),
        tableOutput("load4"),
        tags$br(),
        tags$br(),
        uiOutput('var1'),
        tags$br()
      ),
      tabPanel(
        "Plots & graphs",
        tags$br(),
        h3("Generalized pairs plot1"),
        plotOutput('plot1')%>% withSpinner(color="#0dc5c1"),
        tags$br(),
        tags$br(),
        uiOutput('image_down'),#image to download
        tags$br(),
        h3("Generalized pairs plot2"),
        plotOutput('plot2')%>% withSpinner(color="#0dc5c1"),
        tags$br(),
        tags$br(),
        uiOutput('image_down1'),#image to download
      )
    )
  )
)
