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
      # Check if the rule is not empty and has enough characters
      if (length(rule) > 1 && nchar(rule[2]) >= 1) {
        antecedent <- rule[1]
        transition <- substr(rule[2], 1, 1)  # Extract the transition symbol from the consequent
        
        # Check if the rule is valid
        if (nchar(transition) > 0) {
          # Add states if they don't exist yet
          if (!antecedent %in% V(g)$name) {
            g <- add.vertices(g, nv = 1, name = antecedent)
          }
          
          # Determine the consequent
          if (nchar(rule[2]) > 1) {
            consequent <- substr(rule[2], 2, 2)  # Extract the state from the consequent
            if (!consequent %in% V(g)$name) {
              g <- add.vertices(g, nv = 1, name = consequent)
            }
          } else {
            consequent <- "Z"  # Set to 'Z' if no consequent state is specified
            if (!consequent %in% V(g)$name) {
              g <- add.vertices(g, nv = 1, name = consequent)
            }
          }
          
          # Add transition
          g <- add.edges(g, c(antecedent, consequent))
          E(g, path=c(antecedent, consequent))$name <- transition
        }
      }
    }
    
    # Return the graph
    return(g)
  })
  
  # Render the automaton plot
  output$automatonPlot <- renderPlot({
    g <- reactiveAutomaton()
    
    if (nchar(input$grammar) > 0 && gorder(g) > 0) {
      E(g)$label <- E(g)$name  # Label the edges with the transition symbols
    }
    
    plot(g, vertex.color = ifelse(V(g)$name == "S", "green", ifelse(V(g)$name == "Z", "red", "white")), edge.label=E(g)$label)
  })
}

shinyApp(ui = ui, server = server)