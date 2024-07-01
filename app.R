library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(ggplot2)
library(ggiraph)
library(ggpattern)


# Load the .RData file. Contains constituency geometries as long/lat coordinates and hex, plus a column of some key Conservative candidates that might be at risk.
load("constituencies.RData")

# Define party colors and names
party_colors <- c("#FF0000",    # Red (Labour)
                  "#00008B",    # Dark Blue (Conservative)
                  "#FFA500",    # Orange (Lib Dems)
                  "#ADD8E6",    # Light Blue (Reform UK)
                  "#9ACD32",    # Yellow Green (Green)
                  "#FFD700",    # Khaki 2 (SNP)
                  "#006400",    # Forest Green (Plaid Cymru)
                  "#A9A9A9"     # Dark Grey (Other)
)

party_names <- c("Labour", 
                 "Conservative", 
                 "Lib Dems", 
                 "Reform UK", 
                 "Green",
                 "SNP", 
                 "Plaid Cymru", 
                 "Other"
)

# Sort constituencies alphabetically
sorted_constituencies <- sort(unique(constituencies$Name))

# Define UI
ui <- fluidPage(
  # Search input row with selectizeInput and update button
  fluidRow(
    align = "center",  
    column(width = 12,
           align = "center",  
           tags$h2(style = "font-weight: bold;", "General Election Map, 2024")
    )
  ),
  # Row for the search options.
  fluidRow(
    align = "center",  
    column(width = 5,
           align = "center",  
           selectizeInput("search", "Search constituency", 
                          choices = sorted_constituencies, 
                          multiple = FALSE,
                          options = list(
                            placeholder = "Type to search...",
                            onInitialize = I('function() { this.setValue(""); }')
                          )
           )
    ),
    column(width = 5,
           align = "center",  
           selectizeInput("party", "Select winning party", 
                          choices = party_names, 
                          multiple = FALSE,
                          options = list(
                            placeholder = "Select a party...",
                            onInitialize = I('function() { this.setValue(""); }')
                          )
           )
    ),
    column(width = 2,
           align = "center",  
           actionButton("update", "Update", style = "width: 100%; margin-top: 25px;")  
    )
  ),
  # Row for the hexogram
  fluidRow(
    align = "center",
    column(width = 12,
           align = "center",
           girafeOutput("hexogram", width = "100%", height = "700px")
    )
  ),
  
  # Add some spacing between rows
  tags$hr(style = "margin-top: 10px; margin-bottom: 10px;"),
  
  # Row for the bar plot
  fluidRow(
    align = "center",  
    column(width = 12,
           align = "center",  
           plotOutput("barplot", width = "100%", height = "420px")
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Reactive values to store all constituency-party selections
  rv <- reactiveValues(
    selections = data.frame(constituency = character(), party = character(), stringsAsFactors = FALSE),
    party_counts = rep(0, length(party_names))
  )
  
  # Observe the update button and update the hexogram and bar plot
  observeEvent(input$update, {
    req(input$search, input$party)
    
    # Check if the constituency already has a party assigned
    idx <- which(rv$selections$constituency == input$search)
    
    if (length(idx) == 0) {
      # No party assigned yet, add new selection
      new_selection <- data.frame(constituency = input$search, party = input$party, stringsAsFactors = FALSE)
      rv$selections <- rbind(rv$selections, new_selection)
    } else {
      # Update the existing selection
      rv$selections[idx, "party"] <- input$party
      
      # Remove the entry if no party is selected (undo selection)
      if (input$party == "") {
        rv$selections <- rv$selections[-idx, ]
      }
    }
    
    # Update party counts
    rv$party_counts <- table(rv$selections$party)[party_names]
    
    # Reset the input fields
    updateSelectizeInput(session, "search", selected = "")
    updateSelectizeInput(session, "party", selected = "")
  })
  
  # Render the hexogram
  output$hexogram <- renderGirafe({
    # Add a party column to the constituencies data frame
    constituencies$party <- ifelse(constituencies$Name %in% rv$selections$constituency,
                                   rv$selections$party[match(constituencies$Name, rv$selections$constituency)],
                                   NA)
    
    # Create a new column indicating if there's a candidate
    constituencies$has_candidate <- ifelse(!is.na(constituencies$Candidate), TRUE, FALSE)
    
    # Create ggplot with tooltips
    p <- ggplot(data = constituencies) +
      geom_sf_interactive(aes(geometry = geometry, 
                              fill = ifelse(!is.na(party), party, "No Party"), 
                              tooltip = paste(Name, ifelse(!is.na(Candidate), paste("(", Candidate, ")"), ""), sep = "")), 
                          color = "black", linewidth = 0.1) +  # Base layer for all constituencies
      
      geom_sf_interactive(data = filter(constituencies, has_candidate == TRUE), 
                          aes(geometry = geometry, 
                              fill = ifelse(!is.na(party), party, "No Party"), 
                              tooltip = paste(Name, " (", Candidate, ")", sep = "")), 
                          color = "black", linewidth = 0.35) +  # Layer for constituencies with candidates
      
      scale_fill_manual(values = c(setNames(party_colors, party_names), "No Party" = "lightgrey"), na.value = "lightgrey") +
      
      theme_void() +  
      theme(plot.background = element_rect(fill = "#cbebff", color = NA),  
            legend.position = "none"
      )
    
    # Render the girafe object with interactive options
    girafe_obj <- girafe(ggobj = p)
    
    # Apply zoom options
    girafe_obj <- girafe_options(girafe_obj,
                                 interactive = TRUE,
                                 opts_zoom(min = 0.7, max = 20)
    )  
    
    girafe_obj  # Return the girafe object
  })
  
  
  
  
  # Render the bar plot
  output$barplot <- renderPlot({
    # Calculate party counts
    party_counts_df <- data.frame(party = party_names, count = as.numeric(rv$party_counts))
    
    # Ensure 'party' is a factor with correct ordering and levels
    party_counts_df$party <- factor(party_counts_df$party, levels = rev(party_names))  
    
    # Plot the bar plot
    ggplot(party_counts_df, aes(x = count, y = party, fill = party)) +
      geom_bar(stat = "identity", show.legend = FALSE) +  
      scale_fill_manual(values = setNames(party_colors, party_names)) +  
      labs(x = "Seats", y = NULL, title = NULL) +
      xlim(0, 650) +
      geom_vline(xintercept = 326, linetype = "dashed", color = "red") +
      annotate("text", x = 326, y = length(party_names) , label = paste("\u2190", " Seats required for a majority"), vjust = -0.5, hjust = -0.05) +
      theme_minimal() +
      theme(axis.text.y = element_text(color = "black", size = 12),  
            plot.title = element_text(size = 18, face = "bold", hjust = 0.5))  
  })
}

# Run the app
shinyApp(ui, server)