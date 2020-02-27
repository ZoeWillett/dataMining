library(shiny)
library(golubEsets)

# Define UI for app
ui <- fluidPage(
  
  # App title 
  titlePanel("Hierarchisches Clustering"),
  
  sidebarLayout(
    sidebarPanel(
      p("Bitte Clustering-Methode und Distanzmass auswaehlen:"),
      fluidRow(
        column(6,
               selectInput("clust", h3("Clustering-Methode"), 
                           choices = list("complete" = "complete", "single" = "single",
                                          "average" = "average"), selected = "complete")),
        column(6,
               selectInput("dist", h3("Distanzmass"), 
                           choices = list("euclidean" = "euclidean", "manhattan" = "manhattan"),
                                          selected = "euclidean")),
      )
    ),
    mainPanel(
      fluidRow(
        column(8,
               plotOutput("clustAndDist")),
        column(4,
               helpText("Bei diesem hierarchischen Clustering kann die Clustering-Methode
                        und das Distanzmass gewaehlt werden. Als Daten wird der Golub Datensatz verwendet."))
      )
    )
  )
  
)

# Define server logic
server <- function(input, output) {
  
  # Verarbeitung Daten ---------
  # load Dataset
  library(golubEsets)
  # load specific data
  data("Golub_Train")
  # create var
  x=exprs(Golub_Train)
  # nullwerte rausstreichen
  x[is.na(x)] = 0
  x = x[names(sort(apply(x,1,var), decreasing=TRUE)[1:50]),]
  # Matrix transponieren
  tx = t(x)
  
  output$clustAndDist <- renderPlot({
    #plot(input$clust, input$dist)
    plot(hclust(dist(tx, input$dist), method=input$clust), main="Hierarchisches Clustering", xlab = "Gene")
  })
}

shinyApp(ui = ui, server = server)