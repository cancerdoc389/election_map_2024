# General Election Map, 2024

## Description

This interactive Shiny application can be used to manually visualise constituency election result data for the 2024 General Election using geographical maps and bar plots.

## Features

- **Search Constituency:** Use a search box to find and select a specific constituency.
- **Select Winning Party:** Choose the winning party for the selected constituency from a dropdown list.
- **Interactive Map:** Display constituencies coloured according to the winning party selected.
- **Dynamic Bar Plot:** Shows the distribution of seats among different political parties, updating based on user selections.
- **Highlight Key Constituencies:** Constituencies with notable Conservative candidates are highlighted with bold borders.

## Libraries Used

- **shiny:** For creating the interactive web application.
- **leaflet:** For displaying interactive maps.
- **sf:** For handling spatial data and geometries.
- **dplyr:** For data manipulation tasks.
- **stringr:** For string manipulation functions.
- **ggplot2:** For creating static and interactive plots.
- **ggiraph:** For interactive ggplot objects.

## Installation and Setup

1. **Clone the Repository:**

   ```bash
   git clone https://github.com/your-username/general-election-map.git
   cd general-election-map

## Installation and Setup

2. **Install Dependencies:**

   Ensure you have R and RStudio installed. Install required R packages:

   ```r
   install.packages(c("shiny", "leaflet", "sf", "dplyr", "ggplot2", "ggiraph"))

3. **Run the Application:**

   Open `app.R` in RStudio and click on the "Run App" button to launch the Shiny application.

## Usage

- Use the search input to find a constituency by name.
- Select a party from the dropdown to assign it as the winning party for the selected constituency.
- Click the "Update" button to apply the selection.
- Explore the interactive hexogram map and bar plot to analyse your election results.
- Update the 'Candidate' column in order to highlight any constituencies of interest to you. By default, these are set to some of the prominent figures of the Conservative party, such as Rishi Sunak and Liz Truss.

## Contributing

Contributions are welcome! If you find any issues or have suggestions for improvements, please open an issue or submit a pull request.
