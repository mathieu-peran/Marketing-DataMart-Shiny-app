# Install and load required packages
if (!require("shiny")) install.packages("shiny")
if (!require("shinydashboard")) install.packages("shinydashboard")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("leaflet")) install.packages("leaflet")
if (!require("plotly")) install.packages("plotly")
if (!require("highcharter")) install.packages("highcharter")
library(htmltools)

# Load libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(leaflet)
library(plotly)
library(highcharter)
library(tidyr)

# Load the MarketingDataMart dataframe from the CSV file
MarketingDataMart <- read.csv("C:/Users/Source/Desktop/R Project/MarketingDataMart.csv")

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Marketing DataMart"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview"),
      menuItem("Players Profiles", tabName = "profiles"),
      menuItem("Products", tabName = "products"),
      menuItem("Poker", tabName = "poker"),  # Added "Poker" tab to the sidebar
      selectInput("continentDropdown", "Select Continent",
                  choices = c("World", unique(MarketingDataMart$Continent)), selected = "World"),
      selectInput("countryDropdown", "Select Country",
                  choices = c("All", unique(MarketingDataMart$CountryName)), multiple = TRUE),  # "All" is always an option
      sliderInput("customerLifespanSlider", "Customer Lifespan", 0, max(MarketingDataMart$CustomerLifespan), value = c(0, max(MarketingDataMart$CustomerLifespan)))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                box(
                  textOutput("summaryText"), 
                  title = "Introduction", 
                  solidHeader = TRUE, 
                  width = 12
                )
              ),
              fluidRow(
                box(
                  tags$img(src = "bwin.jpg", height = "80%", width = "80%"), 
                  solidHeader = TRUE, 
                  width = 6
                ),
                box(
                  tableOutput("dataTable"), 
                  title = "Summary Statistics", 
                  solidHeader = TRUE, 
                  width = 6
                )
              ),
              fluidRow(
                box(
                  plotOutput("topten_profit"),
                  title = "Top 10 Countries by Business Profit",
                  solidHeader = TRUE,
                  width = 6
                ),
                box(
                  plotOutput("bottomten_profit"),
                  title = "Bottom 10 Countries by Business Profit",
                  solidHeader = TRUE,
                  width = 6
                )
              )
      ),
  
      tabItem(tabName = "profiles",
              fluidRow(
                # Plot 1: Gender Distribution
                box(
                  plotOutput("genderDistribution"),
                  title = "Gender Distribution",
                  solidHeader = TRUE
                ),
                # Plot 2: Top 5 Applications by Number of Users
                box(
                  plotOutput("topApplications"),
                  title = "Top 5 Applications by Number of Users",
                  solidHeader = TRUE
                ),
                # New box for the language distribution pie chart
                box(
                  plotOutput("languageDistribution"),
                  title = "Language Distribution",
                  solidHeader = TRUE
                )
              )
      ),
      tabItem(tabName = "poker",
              fluidRow(
                selectInput("timeOfDay", "Time of Day:",
                            choices = c("All", "Morning", "Afternoon", "Night"),
                            selected = "All"),
                fluidRow(
                  column(6,
                         plotOutput("marketingPlot")
                  ),
                  column(6,
                         plotOutput("balanceHistogram")
                  )
                ),
                fluidRow(
                  column(6,
                         plotOutput("combinedScatterPlot")
                  ),
                  column(6,
                         # Add another plotOutput here if needed
                  )
                )
              )
      ),
      tabItem(tabName = "products",
              fluidRow(
                selectInput("categoryDropdown", "Select Product",
                            choices = c("Sports book fixed-odd", "Sports book live-action",
                                        "Casino BossMedia", "Supertoto", "Games VS",
                                        "Games bwin", "Casino Chartwell"), selected = "Sports book fixed-odd"),
                column(12, align = "center", plotOutput("aggregatedPlot", width = "70%"))  # Centered the plot
              )
      )
    )
  )
)

# Define server
server <- function(input, output, session) {
  observe({
    req(input$continentDropdown)
    if (input$continentDropdown == "World") {
      choices <- c("All", unique(MarketingDataMart$CountryName))
    } else {
      choices <- c("All", unique(MarketingDataMart$CountryName[MarketingDataMart$Continent == input$continentDropdown]))
    }
    updateSelectInput(session, "countryDropdown", choices = choices, selected = "All")
  })
  
  # Filter data for Poker tab based on selected countries and customerLifespanSlider
  selectedCountryDataPoker <- reactive({
    req(input$countryDropdown, input$continentDropdown, input$customerLifespanSlider)
    
    data <- MarketingDataMart
    
    if (input$continentDropdown != "World") {
      data <- data[data$Continent == input$continentDropdown, ]
    }
    
    if (!("All" %in% input$countryDropdown)) {
      data <- data[data$CountryName %in% input$countryDropdown, ]
    }
    
    # Add filter based on customerLifespanSlider
    data <- data[data$CustomerLifespan >= input$customerLifespanSlider[1] & data$CustomerLifespan <= input$customerLifespanSlider[2], ]
    
    return(data)
  })
  
  #Introduction text:
  # Render text for the Summary tab
  output$summaryText <- renderText({
    "This is an interactive application where you can change several metrics to be able to filter the data according
    to several metrics such as Continent, Country, Time of day. This applications hold 4 different interactive tabs.
    The first tab focuses on the user demographics. The secon tab focuses on the Poker game. The third tab focuses on
    the different products offered. And lastly the last tab focuses on the client segmentation and client loyality."
  })
  
  #Summary Statistics Table:
  myTableData <- read.csv("C:/Users/Source/Desktop/R Project/SummStats.csv") # Replace with your CSV file path
  
  # Render the table in the UI
  output$dataTable <- renderTable({
    myTableData
  })
  
  #adding the business plots of mathieu:
  # Profitability per Country
  profit_by_country <- MarketingDataMart %>%
    group_by(Country = CountryName) %>%
    summarize(BusinessProfit = sum(-replace(total_profit, is.na(total_profit), 0), na.rm = TRUE)) %>%
    arrange(desc(BusinessProfit)) # Arrange the countries by descending business profit
  
  # Top 10 profitable countries (for the business)
  top_countries_profit <- head(profit_by_country, 10)
  
  # Now, you can plot the top 10 profitable countries for the business
  output$topten_profit <- renderPlot({
    ggplot(top_countries_profit, aes(x = reorder(Country, BusinessProfit), y = BusinessProfit)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +  # Flip the coordinates to make it horizontal
      scale_y_continuous(labels = scales::dollar_format(prefix = "€", big.mark = ",")) +  # Format the y-axis labels
      labs(title = "Top 10 Countries by Business Profit", x = "Country", y = "Business Profit (€)") +
      theme_minimal()
  })
  
  # Top 10 less profitable countries (for the business)
  bottom_countries_profit <- tail(profit_by_country, 10)
  
  # Now, you can plot the top 10 profitable countries for the business
  output$bottomten_profit <- renderPlot({
    ggplot(bottom_countries_profit, aes(x = reorder(Country, BusinessProfit), y = BusinessProfit)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +  # Flip the coordinates to make it horizontal
      scale_y_continuous(labels = scales::dollar_format(prefix = "€", big.mark = ",")) +  # Format the y-axis labels
      labs(title = "Bottom 10 Countries by Business Profit", x = "Country", y = "Business Profit (€)") +
      theme_minimal()
  })
  # Plot 1: Gender Distribution
  output$genderDistribution <- renderPlot({
    ggplot(selectedCountryDataPoker(), aes(x = Gender, fill = Gender)) +
      geom_bar() +
      scale_fill_manual(values = c("Female" = "pink", "Male" = "blue")) +
      labs(title = "Gender Distribution", x = "Gender", y = "Count")
  })
  
  # Plot 2: Top 5 Applications by Number of Users
  output$topApplications <- renderPlot({
    # Group by ApplicationID, count the number of users, and get the top 5 applications
    top_apps <- selectedCountryDataPoker() %>%
      group_by(ApplicationDescription) %>%
      summarize(NumberOfUsers = n()) %>%
      arrange(desc(NumberOfUsers)) %>%
      top_n(5, NumberOfUsers)
    
    # Convert ApplicationID to a factor
    top_apps$ApplicationDescription <- as.factor(top_apps$ApplicationDescription)
    
    # Define pastel colors
    pastel_colors <- c("#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4", "#FED9A6", "#FFFFCC", "#E5D8BD", "#FDDAEC")
    
    # Plotting the number of users for the top 5 applications with pastel colors
    ggplot(top_apps, aes(x = reorder(ApplicationDescription, NumberOfUsers), y = NumberOfUsers, fill = ApplicationDescription)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = pastel_colors) +
      geom_text(aes(label = NumberOfUsers), vjust = -0.3, color = "black") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Top 5 Applications by Number of Users", x = "Application ID", y = "Number of Users")
  })
  
  # New output for the language distribution pie chart
  output$languageDistribution <- renderPlot({
    # Create a pie chart for Language Distribution
    ggplot(selectedCountryDataPoker(), aes(x = "", fill = LanguageDescription)) +
      geom_bar(width = 1) +  # Create bars with width 1 for a pie chart effect
      coord_polar(theta = "y") +  # Convert the bar chart into a pie chart
      labs(title = "Pie Chart of Language Distribution",
           fill = "Language Description") +
      theme_void()  # Remove unnecessary elements like axes and gridlines
  })
  
  output$marketingPlot <- renderPlot({
    data <- selectedCountryDataPoker() %>%
      pivot_longer(
        cols = c(
          NumTransactions_Morning_Buy, NumTransactions_Afternoon_Buy, NumTransactions_Night_Buy,
          NumTransactions_Morning_Sell, NumTransactions_Afternoon_Sell, NumTransactions_Night_Sell
        ),
        names_to = "TransactionType",
        values_to = "Count"
      ) %>%
      mutate(TimeOfDay = case_when(
        grepl("Morning", TransactionType) ~ "Morning",
        grepl("Afternoon", TransactionType) ~ "Afternoon",
        grepl("Night", TransactionType) ~ "Night"
      ),
      TransactionAction = if_else(grepl("Buy", TransactionType), "Buy", "Sell"))
    
    if(input$timeOfDay != "All") {
      data <- data %>% filter(TimeOfDay == input$timeOfDay)
    }
    
    ggplot(data, aes(x = TimeOfDay, y = Count, fill = TransactionAction)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      scale_fill_manual(values = c("Buy" = "blue", "Sell" = "red")) +
      labs(title = "Poker Transactions Count by Time of Day", x = "Time of Day", y = "Transactions Count") +
      theme_minimal()
  })
  
  # Render the balance histogram
  output$balanceHistogram <- renderPlot({
    filteredBalanceData <- selectedCountryDataPoker() %>%
      filter(Poker_Balance >= -500 & Poker_Balance <= 500)
    
    ggplot(filteredBalanceData, aes(x = Poker_Balance)) +
      geom_histogram(binwidth = 10, fill = "blue", color = "black") +
      labs(title = "Distribution of Poker Balances", x = "Poker Balance", y = "Count of Users") +
      theme_minimal()
  })
  
  # Render the combined scatter plot
  output$combinedScatterPlot <- renderPlot({
    combined_data <- selectedCountryDataPoker() %>%
      filter(TotalPokerAmount_Buy <= 100000, TotalPokerAmount_Sell <= 100000) %>%
      mutate(TransactionType = "Buy") %>%
      select(TransactionType, ActiveDays = ActiveDays_Buy, TotalAmount = TotalPokerAmount_Buy) %>%
      bind_rows(
        selectedCountryDataPoker() %>%
          filter(TotalPokerAmount_Buy <= 100000, TotalPokerAmount_Sell <= 100000) %>%
          mutate(TransactionType = "Sell") %>%
          select(TransactionType, ActiveDays = ActiveDays_Sell, TotalAmount = TotalPokerAmount_Sell)
      )
    
    ggplot(combined_data, aes(x = ActiveDays, y = TotalAmount, color = TransactionType)) +
      geom_point(alpha = 0.6) +
      scale_color_manual(values = c("Buy" = "red", "Sell" = "blue")) +
      labs(title = "Combined Buy and Sell Transactions: Active Days vs Total Amount (Capped at 100000)",
           x = "Active Days",
           y = "Total Transaction Amount",
           color = "Transaction Type") +
      theme_minimal()
  })
  
  # New output for the aggregated plot in "Products" tab
  output$aggregatedPlot <- renderPlot({
    # Aggregate data to get overall sums
    sums <- selectedCountryDataPoker() %>%
      summarise(
        Stakes = sum(coalesce(get(paste0("avg_daily_stakes_", gsub("[ -]", ".", input$categoryDropdown))), 0)),
        Bets = sum(coalesce(get(paste0("avg_daily_bets_", gsub("[ -]", ".", input$categoryDropdown))), 0)),
        Winnings = sum(coalesce(get(paste0("avg_daily_winnings_", gsub("[ -]", ".", input$categoryDropdown))), 0))
      )
    
    # Reshape data using pivot_longer
    sums_long <- sums %>% pivot_longer(cols = everything(), names_to = "Variables", values_to = "TotalAmount")
    
    # Plotting with adjusted parameters
    ggplot(sums_long, aes(x = Variables, y = TotalAmount, fill = Variables)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.5, color = "white") +  # Adjusted width and color
      ggtitle("Total Daily Stakes, Bets, and Winnings") +
      xlab("Variables") +
      ylab("Amount") +
      scale_fill_manual(values = c('Stakes' = '#3498db', 'Bets' = '#2ecc71', 'Winnings' = '#e74c3c')) +  # Changed colors
      theme_minimal() +
      theme(axis.text.x = element_text(size = 10),  # Increased label size
            axis.text.y = element_text(size = 10),  # Increased label size
            axis.title.x = element_text(size = 12),  # Increased title size
            axis.title.y = element_text(size = 12),  # Increased title size
            plot.title = element_text(size = 14))  # Increased plot title size
  })
}

# Run the Shiny app
shinyApp(ui, server)


