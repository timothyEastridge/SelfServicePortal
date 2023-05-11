# library(shiny)
library(shinydashboard)
library(dplyr)
library(visNetwork)
library(data.table)

# Add footer to each tab

shinyApp(
  ui = dashboardPage(
    skin = "black",
    header = dashboardHeader(title = "Self-Service Portal"),
    
    sidebar = dashboardSidebar(
      collapsed = FALSE,
      sidebarMenu(
        menuItem("One Hop Connections", tabName = "onehop", icon = icon("network-wired")),
        menuItem("Leiden Community Data", tabName = "filter", icon = icon("database")),
        menuItem("Network Explorer", tabName = "network", icon = icon("connectdevelop")),
        menuItem("Code Sharing", tabName = "wiki", icon = icon("wikipedia-w")),
        menuItem("Employee Matching", tabName = "matching", icon = icon("users"))
      )
    ),
    body = dashboardBody(
      tabItems(
        tabItem(tabName = "onehop",
                fluidRow(
                  column(width = 6,
                         textInput("ssns1", "List of SSNs", value = ""),
                         textInput("name1", "Name", value = ""),
                         textInput("state1", "States", value = ""),
                         textInput("city1", "City", value = ""),
                         textInput("zip1", "Zip", value = ""),
                         textInput("phone1", "Phone", value = ""),
                         numericInput("amount1", "Stimulus Amount", value = ""),
                         br(), 
                         # numericInput("max_people", "Max Number of Individuals", value = 20),
                         actionButton("submit_ssns1", "Submit"),
                         tags$head(
                           tags$style(HTML('#submit_ssns1{background-color:green; color:white; font-weight:bold}'))
                         ),
                         br(), br(), br(), br(), br()),
                  column(width = 6,
                         textInput("email1", "Email"),
                         textInput("SSH_key1", "SSH Key for Encrypted Authentication"),
                         actionButton("send_email1", "Send Email & Request Tax IDs for Members of Community"),
                         tags$head(
                           tags$style(HTML('#send_email1{background-color:green; color:white; font-weight:bold}'))
                         ))
                ),
                fluidRow(
                  box(status = "primary", width = 12, title = "SSN 1-Hop Data", 
                      DT::dataTableOutput("table1"))
                )
                ,
                br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
                tags$iframe(src = "https://i3llc.us/", 
                            height = "400", width = "100%", style = "border:none; margin-top:20px;")
        ),
        tabItem(tabName = "filter",
                fluidRow(
                  column(width = 6,
                         textInput("state", "State", value = "FL"),
                         textInput("city", "City", value = "Miami"),
                         numericInput("amount", "Community Stimulus Amount", value = 1000000),
                         numericInput("max_people", "Max Number of Individuals", value = 20),
                         actionButton("submit", "Submit"),
                         tags$head(
                           tags$style(HTML('#submit{background-color:green; color:white; font-weight:bold}'))
                         ),
                         br(), br(), br()),
                  column(width = 6,
                         textInput("email", "Email"),
                         textInput("SSH_key", "SSH Key for Encrypted Authentication"),
                         actionButton("send_email", "Send Email & Request Tax IDs for Members of Community"),
                         tags$head(
                           tags$style(HTML('#send_email{background-color:green; color:white; font-weight:bold}'))
                         ))
                ),
                fluidRow(
                  box(status = "primary", width = 12, title = "Filtered Data", 
                      DT::dataTableOutput("table"))
                )
                ,
                br(), br(), br(), br(), br(), br(), br(), br(), br(),
                tags$iframe(src = "https://i3llc.us/", 
                            height = "400", width = "100%", style = "border:none; margin-top:20px;")
        ),
        tabItem(tabName = "network",
                fluidRow(
                  column(4,
                         textInput("community", "Leiden Community Name"),
                         br(),
                         actionButton("submit_community", "Submit", class = "btn btn-success"),
                         br(),
                         tags$head(
                           tags$style(HTML('#submit_community{background-color:green; color: white; font-weight: bold;}'))
                         )
                  ),
                  column(8,
                         visNetworkOutput("network")
                  )
                )
        )
      )
      
      
    )
    # 
    # footer = dashboardFooter(
    #   tags$img(src = "i3_logo.png", width = 100, height = 100)
    # )
    
  
  ),
  
  
  
  server = function(input, output, session) {
    
    
    
    # # 1-hop ####
    # {
    #   output$tablessns <- renderDataTable({
    #     
    #     # Read CSV
    #     data2 <- fread("SSNs.csv", stringsAsFactors = FALSE)
    #     
    #     # Set initial values for inputs
    #     observe({
    #       updateTextInput(session, "name1", value = "Grace Jackson")
    #       # updateTextInput(session, "city", value = "Miami")
    #       # updateNumericInput(session, "amount", value = 1000000)
    #     })
    #     
    #     # Define filter function
    #     filtered_data2 <- reactive({
    #       data2 %>%
    #         filter(full_name == input$name1)
    #     })
    #     
    #     # Return the filtered data as a data frame for rendering the table
    #     SSN_table()
    #     
    #     # Render table output
    #     output$table <- DT::renderDataTable({
    #       # if (input$submit == 0) {
    #       #   return()
    #       # }
    #       filtered_data2()
    #     })
    #   })
    # }

    # 1-hop ####
    {
      # Read CSV
      data1 <- fread("SSNs.csv", stringsAsFactors = FALSE)
      
      
      # Set initial values for inputs
      observe({
        updateTextInput(session, "name1", value = "Grace Jackson")
      })
      
      # Define filter function
      filtered_data1 <- reactive({
        data1 %>%
          filter(full_name == input$name1)
      })
      
      # Render table output
      output$table1 <- DT::renderDataTable({
        # if (input$submit == 0) {
        #   return()
        # }
        filtered_data1()
      })
    }
    
    
    # Leiden ####
    {
      # Read CSV
      data <- fread("leiden_community_data.csv", stringsAsFactors = FALSE)
      
      # # Load data from GitHub URL
      # data_url <- "https://github.com/timothyEastridge/SelfServicePortal/raw/main/leiden_community_data.csv"
      # data <- read.csv(data_url, stringsAsFactors = FALSE)
      
      # Set initial values for inputs
      observe({
        updateTextInput(session, "state", value = "FL")
        updateTextInput(session, "city", value = "Miami")
        updateNumericInput(session, "amount", value = 1000000)
      })
      
      # Define filter function
      filtered_data <- reactive({
        data %>%
          filter(Primary_State == input$state,
                 Primary_City == input$city,
                 Community_Stimulus_Amount >= input$amount,
                 Community_Count_of_Individuals >= input$max_people)
      })
      
      # Render table output
      output$table <- DT::renderDataTable({
        # if (input$submit == 0) {
        #   return()
        # }
        filtered_data()
      })
    }
    
    # Create a simple network
    nodes <- data.frame(id = 1:10, label = paste0("Individual ", 1:10), Amount = sample(1:10, 10, replace = TRUE))
    edges <- data.frame(from = rep(1:10, each = 9), to = rep(2:10, each = 1))
    output$network <- renderVisNetwork({
      visNetwork(nodes, edges) %>%
        visNodes(label = "label", shape = "circle", size = "Amount", color = "orange") %>%
        visEdges(arrows = "to")
    })
    

    
    # Define function to send email
    observeEvent(input$send_email, {
      # Your email sending code here
    })
    
    # Define function to request tax IDs
    observeEvent(input$send_email, {
      # Your tax ID request code here
    })
    
  }
  
)

