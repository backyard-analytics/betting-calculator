# Load the Shiny package
library(shiny)

ui <- fluidPage(
  titlePanel("Betting Calculator"),
  sidebarLayout(
    sidebarPanel(
      numericInput("num_bets", "Number of Bets:", 1),
      radioButtons("is_parlay", "Is this a parlay?", c("Yes" = "yes", "No" = "no")),
      uiOutput("betting_fields"),
      actionButton("calculate", "Calculate Potential Winnings")
    ),
    mainPanel(
      textOutput("potential_winnings"),
      tableOutput("submitted_bets")
    )
  )
)


# Define server logic
server <- function(input, output, session) {
  
  output$betting_fields <- renderUI({
    input_fields <- lapply(1:input$num_bets, function(i) {
      tagList(
        selectInput(
          paste0("bet_type_", i),
          paste("Bet type for bet", i),
          c("Moneyline", "Over/Under", "Point Spread")
        ),
        numericInput(
          paste0("american_odds_", i),
          paste("American odds for bet", i),
          value = 100
        )
      )
    })
    
    if (input$is_parlay == "no") {
      for (i in 1:input$num_bets) {
        input_fields[[i]] <- tagAppendChild(input_fields[[i]],
                                            numericInput(
                                              paste0("bet_amount_", i),
                                              paste("Amount for bet", i),
                                              value = 10
                                            )
        )
      }
    } else {
      parlay_amount_input <- tagList(
        numericInput("parlay_bet_amount", "Amount for Parlay Bet", value = 10)
      )
      input_fields <- c(input_fields, list(parlay_amount_input))
    }
    return(input_fields)
  })
  
  # Create a reactive object to store bets
  submitted_bets <- reactiveVal(data.frame())
  
  observeEvent(input$calculate, {
    # Function to calculate potential winnings based on American odds
    calculate_winnings <- function(american_odds, bet_amount, is_parlay) {
      potential_winnings <- numeric(length(american_odds))
      
      if (is_parlay) {
        combined_odds <- 1
        for (i in seq_along(american_odds)) {
          if (american_odds[i] > 0) {
            combined_odds <- combined_odds * (american_odds[i] / 100 + 1)
          } else if (american_odds[i] < 0) {
            combined_odds <- combined_odds * (100 / abs(american_odds[i]) + 1)
          } else {
            stop("Odds cannot be zero.")
          }
        }
        potential_winnings <- bet_amount[1] * (combined_odds - 1)
      } else {
        for (i in seq_along(american_odds)) {
          if (american_odds[i] > 0) {
            potential_winnings[i] <- bet_amount[i] * (american_odds[i] / 100)
          } else if (american_odds[i] < 0) {
            potential_winnings[i] <- bet_amount[i] / (abs(american_odds[i]) / 100)
          } else {
            stop("Odds cannot be zero.")
          }
        }
      }
      
      potential_winnings <- round(potential_winnings, 2)
      return(sum(potential_winnings))
    }
    
    # Collect the data from inputs
    num_bets <- input$num_bets
    is_parlay <- input$is_parlay
    american_odds <- sapply(1:num_bets, function(i) input[[paste0("american_odds_", i)]])
    bet_amount <- if (is_parlay == "no") {
      sapply(1:num_bets, function(i) input[[paste0("bet_amount_", i)]])
    } else {
      rep(input$parlay_bet_amount, num_bets)
    }
    
    # Calculate the potential winnings
    potential_winnings <- calculate_winnings(american_odds, bet_amount, is_parlay == "yes")
    
    # Update the text output
    output$potential_winnings <- renderText({
      paste("If the bets hit, you would win a total of:", potential_winnings)
    })
    
    # Store the bets in the reactive object
    bet_data <- data.frame(
      "Bet Type" = sapply(1:num_bets, function(i) input[[paste0("bet_type_", i)]]),
      "American Odds" = american_odds,
      "Bet Amount" = bet_amount
    )
    submitted_bets(bet_data)
  })
  
  # Render the table for submitted bets
  output$submitted_bets <- renderTable({
    submitted_bets()
  })
}



# Run the application 
shinyApp(ui = ui, server = server)
