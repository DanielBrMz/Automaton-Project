install.packages(c("shiny", "igraph"))
library(shiny)
library(igraph)


ui <- fluidPage(
  titlePanel("Regular Grammar to Finite Automaton Converter"),
  sidebarLayout(
    sidebarPanel(
      textAreaInput("grammar", "Enter Regular Grammar", rows = 10)
    ),
    mainPanel(
      plotOutput("automatonPlot")
    )
  )
)



server <- function(input, output) {
  # Reactive expression to process grammar input and create automaton
  reactiveAutomaton <- reactive({
    # Parse user input into rules (assuming correct format X -> Y)
    rules <- strsplit(input$grammar, "\n")[[1]]
    rules <- lapply(rules, function(x) strsplit(x, " -> ")[[1]])
    
    # Create graph object
    g <- graph.empty()
    
    # Add states and transitions based on rules
    for (rule in rules) {
      antecedent <- rule[1]
      consequent <- rule[2]
      
      # Add states if they don't exist yet
      if (!antecedent %in% V(g)$name) {
        g <- add.vertices(g, nv = 1, name = antecedent)
      }
      if (!consequent %in% V(g)$name) {
        g <- add.vertices(g, nv = 1, name = consequent)
      }
      
      # Add transition
      g <- add.edges(g, c(antecedent, consequent))
    }
    
    # Return the graph
    return(g)
  })
  
  # Render the automaton plot
  output$automatonPlot <- renderPlot({
    g <- reactiveAutomaton()
    plot(g, vertex.color = ifelse(V(g)$name == "S", "green", ifelse(V(g)$name == "Z", "red", "white")))
  })
}

shinyApp(ui = ui, server = server)
