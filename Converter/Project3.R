# install.packages(c("shiny", "igraph")) install if needed
library("shiny")
library("igraph")


ui <- fluidPage(
  titlePanel("Regular Grammar to Finite Automaton Converter"),
  sidebarLayout(
    sidebarPanel(
      textAreaInput("grammar", "Enter Regular Grammar", rows = 10)
    ),
    mainPanel(
      verbatimTextOutput("validRules"), 
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
    g <- make_empty_graph(directed = TRUE)
    
    # Initialize processed grammar
    processedGrammar <- ""
    
    # Add states and transitions based on rules
    for (rule in rules) {
      # Check if the rule is not empty and has enough characters
      if (length(rule) > 1 && nchar(rule[2]) >= 1) {
        antecedent <- rule[1]
        transition <- substr(rule[2], 1, 1)
        
        # Check if the rule is valid
        if (nchar(transition) > 0) {
          # Add states if they don't exist yet
          if (!antecedent %in% V(g)$name) {
            g <- add_vertices(g, nv = 1, name = antecedent)
          }
          
          # Determine the consequent
          if (nchar(rule[2]) > 1) {
            consequent <- substr(rule[2], 2, 2)
            if (!consequent %in% V(g)$name) {
              g <- add_vertices(g, nv = 1, name = consequent)
            }
          } else {
            consequent <- "Z" 
            if (!consequent %in% V(g)$name) {
              g <- add_vertices(g, nv = 1, name = consequent)
            }
          }
          
          # Add transition
          g <- add_edges(g, c(antecedent, consequent))
          E(g, path=c(antecedent, consequent))$name <- transition
          
          # Add the valid rule to the processed grammar
          processedGrammar <- paste(processedGrammar, paste(antecedent, "->", paste(transition, consequent, sep="")), sep="\n")
        }
      }
    }
    
    return(list(g, processedGrammar))
  })
  
  # Render the automaton plot
  output$automatonPlot <- renderPlot({
    g <- reactiveAutomaton()[[1]]
    
    if (nchar(input$grammar) > 0 && gorder(g) > 0) {
      E(g)$label <- E(g)$name 
    }
    
    plot(g, vertex.color = ifelse(V(g)$name == "S", "green", ifelse(V(g)$name == "Z", "red", "white")), edge.label=E(g)$label)
  })
  
  # Render the processed grammar
  output$validRules <- renderText({
    processedGrammar <- reactiveAutomaton()[[2]]
    processedGrammar <- gsub("Z", "", processedGrammar)
    if (nchar(processedGrammar) > 0) {
      paste("Output:\n", processedGrammar)
    } else {
      "Output:"
    }
  })
}

shinyApp(ui = ui, server = server)
