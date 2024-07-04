# Load the libraries
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(ggplot2)
library(ggiraph)
library(DT)
library(forcats)  # Load the forcats package for factor manipulation

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
  fluidRow(
    align = "center",  
    column(width = 12,
           align = "center",  
           tags$h2(style = "font-weight: bold;", "General Election Map, 2024")
    )
  ),
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
  fluidRow(
    align = "center",
    column(width = 6,  # Adjust column widths as needed
           align = "center",
           girafeOutput("hexogram", width = "100%", height = "100%")
    ),
    column(width = 6,  # Adjust column widths as needed
           align = "center",
           plotOutput("barplot", width = "100%", height = "1000px")  # Adjust height as needed
    )
  ),
  
  tags$hr(style = "margin-top: 10px; margin-bottom: 10px;"),
  
  fluidRow(
    align = "center",
    column(width = 12,
           align = "center",
           dataTableOutput("results_table")
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
                          color = "white", linewidth = 0.25) +  # Base layer for all constituencies
      
      geom_sf_interactive(data = filter(constituencies, has_candidate == TRUE), 
                          aes(geometry = geometry, 
                              fill = ifelse(!is.na(party), party, "No Party"), 
                              tooltip = paste(Name, " (", Candidate, ")", sep = ""),
                              color = I(border_colour)),  # Use the border_colour column
                          linewidth = 0.25) +  # Layer for constituencies with candidates
      
      scale_fill_manual(values = c(setNames(party_colors, party_names), "No Party" = "lightgrey"), na.value = "lightgrey") +
      
      theme_void() +  
      theme(plot.background = element_rect(fill = "white", color = NA),  
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
    ggplot(party_counts_df, aes(x = fct_rev(fct_infreq(party)), y = count, fill = party)) +
      geom_bar(stat = "identity") +  
      scale_fill_manual(values = setNames(party_colors, party_names)) +  
      labs(x = NULL, y = "Seats") +  
      ylim(0, 600) +  # Adjust y-axis limits
      geom_hline(yintercept = 326, linetype = "dashed", color = "red") +
      annotate("text", x = length(party_names) / 2, y = 326 + 20, 
               label = "Seats required for a majority", 
               hjust = 0, vjust = 1.5, size = 5, fontface = "bold") +  # Adjust position, size, and font
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),  # Adjust axis text size
            axis.title.y = element_text(size = 16, face = "bold"),  # Adjust y-axis title size and bold
            plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
            legend.position = "bottom",  # Move legend to bottom
            legend.title = element_blank(),  # Remove legend title
            legend.spacing.x = unit(0.2, 'cm'),  # Adjust spacing between legend items
            legend.text = element_text(size = 12),  # Adjust legend text size
            legend.box.just = "left",  # Align legend items to the left
            legend.box = "horizontal",  # Arrange legend items horizontally
            legend.key.width = unit(2, 'cm'),  # Adjust width of legend keys
            panel.grid.major.y = element_line(color = "gray", size = 0.5),  # Adjust major grid lines
            panel.grid.minor.y = element_blank(),  # Remove minor grid lines
            axis.ticks.y = element_blank(),  # Remove y-axis ticks
            axis.text.y = element_text(size = 14),  # Adjust y-axis text size
            axis.line.y = element_line(size = 0.5),  # Adjust y-axis line size
            panel.background = element_blank(),  # Remove panel background
            panel.border = element_rect(color = "black", fill = NA, size = 0.5)) +  # Adjust panel border
      guides(fill = guide_legend(reverse = TRUE))  # Reverse legend order
  })
  
}

# Run the app
shinyApp(ui, server)
