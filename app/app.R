# packs <- c("shinyjs", "shiny", "tidyverse", "shinydashboard", "sodium")
library(shinyjs)
library(shiny)
library(tidyverse)
library(shinydashboard)
library(shinyMobile)
library(DT)

# packs <- c("shinyjs", "shiny", "tidyverse", "shinydashboard", "DT")
# sapply(packs, require, character.only = TRUE)

####################
### Login screen ###
####################
# placing here in case there is interest later

# loginpage <- div(id = "loginpage",
#                  style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
#                  wellPanel(
#                    tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
#                    textInput("userName", placeholder = "Username", label = tagList(icon("user"), "Username")),
#                    passwordInput("passwd", placeholder = "Password", label = tagList(icon("unlock-alt"), "Password")),
#                    br(),
#                    div(
#                      style = "text-align: center;",
#                      actionButton("login", "SIGN IN", style = "color: white; background-color:#3c8dbc;
#                                  padding: 10px 15px; width: 150px; cursor: pointer;
#                                  font-size: 18px; font-weight: 600;"),
#                      shinyjs::hidden(
#                        div(id = "nomatch",
#                            tags$p("Oops! Incorrect username or password!",
#                                   style = "color: red; font-weight: 600;
#                                             padding-top: 5px;font-size:16px;",
#                                   class = "text-center"))),
#                      br(),
#                      br(),
#                      HTML("<p style='color:red'>Username: user </br> Password: pass </br> May want to remove this hehe</p>"),
#                      br(),
#                      br(),
#                      h4("What's new?"),
#                      HTML("<ul class = 'text-left'>
#                          <li> The application is now password protected </li>
#                          </ul>")
#                    ))
# )

# login credentials that are accepted - changes credentials here
# credentials = data.frame(
#   username_id = c("user"),
#   passod   = sapply(c("pass"),password_store),
#   permission  = c("basic"),
#   stringsAsFactors = F
# )

# header on app
header <- dashboardHeader(
  title = "Gang utilities",
  titleWidth = 180
  # uiOutput("logoutbtn")
)

# sidebar for navigation
sidebar <- dashboardSidebar(
  uiOutput("sidebarpanel")
)

# meat
body <- dashboardBody(
  shinyjs::useShinyjs(),
  uiOutput("body")
)

# put it together
ui <- dashboardPage(
  header,
  sidebar,
  body,
  skin = "black"
)

server <- function(input, output, session) {
  
  ##################
  ### Login page ###
  ##################
  
  # login = FALSE
  login = TRUE
  USER <- reactiveValues(login = login)
  
  # observe({
  #   if (USER$login == FALSE) {
  #     if (!is.null(input$login)) {
  #       if (input$login > 0) {
  #         Username <- isolate(input$userName)
  #         Password <- isolate(input$passwd)
  #         if(length(which(credentials$username_id==Username))==1) {
  #           pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
  #           pasverify <- password_verify(pasmatch, Password)
  #           if(pasverify) {
  #             USER$login <- TRUE
  #           } else {
  #             shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
  #             shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
  #           }
  #         } else {
  #           shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
  #           shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
  #         }
  #       }
  #     }
  #   }
  # })
  
  ##############
  ### Header ###
  ##############
  # output$logoutbtn <- renderUI({
  #   req(USER$login)
  #   tags$li(a(icon("fa fa-sign-out"), "Logout",
  #             href="javascript:window.location.reload(true)"),
  #           class = "dropdown",
  #           style = "background-color: #eee !important; border: 0;
  #                   font-weight: bold; margin:5px; padding: 10px;")
  # })
  
  ###############
  ### Sidebar ###
  ###############
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE ){
      sidebarMenu(
        menuItem("Bill splitter", tabName = "bill", icon = icon("dollar-sign"))
      )
    }
  })
  
  ############
  ### Body ###
  ############
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      
      tabItems(
        # intro
        tabItem(
          tabName = "bill_splitter",
          class = "active",
          uiOutput("bill_splitter")
        )
      )
      
      
    } else {
      loginpage
    }
  })
  
  # bill splitter tab
  output$bill_splitter <- renderUI({
    list(
      fluidRow(
        column(
          3,
          wellPanel(
            h3("Member selection"),
            checkboxGroupInput(
              inputId = "members",
              label = "Gang members present",
              choices = sort(
                c(
                  "Christian",
                  "Ian",
                  "John", 
                  "Meaghan",
                  "Samidha"
                )
              ),
              selected = c("Christian", "Ian", "John", "Meaghan", "Samidha")
            ),
            h3("Individual bills"),
            uiOutput("numericinputs"),
            h3("Modifiers"),
            uiOutput("winnings"),
            uiOutput("tip_propUI"),
            uiOutput("tip_raw") 
          )
        ),
        column(
          9, 
          wellPanel(
            h3("Tabular summaries"),
            dataTableOutput("split_table")
          )
        )
      )
    )
  })
  
  output$numericinputs <- renderUI({
    out <- list()
    ndx = 1
    
    # individual bills
    if("Christian" %in% input$members){
      out[[ndx]] <- numericInput(
        inputId = "christian_bill",
        label = "Christian bill",
        value = 10,
        min = 0
      )
      
      ndx = ndx + 1
    }
    if("Ian" %in% input$members){
      out[[ndx]] <- numericInput(
        inputId = "ian_bill",
        label = "Ian bill",
        value = 10,
        min = 0
      )
      
      ndx = ndx + 1
    }
    if("John" %in% input$members){
      out[[ndx]] <- numericInput(
        inputId = "john_bill",
        label = "John bill",
        value = 10,
        min = 0
      )
      
      ndx = ndx + 1
    }
    if("Meaghan" %in% input$members){
      out[[ndx]] <- numericInput(
        inputId = "meaghan_bill",
        label = "Meaghan bill",
        value = 10,
        min = 0
      )
      
      ndx = ndx + 1
    }
    if("Samidha" %in% input$members){
      out[[ndx]] <- numericInput(
        inputId = "samidha_bill",
        label = "Samidha bill",
        value = 10,
        min = 0
      )
      
      ndx = ndx + 1
    }
    
    out
  })
  
  output$winnings <- renderUI({
    numericInput(
      inputId = "winnings",
      label = "Approximate winnings",
      value = 38,
      min = 0
    )
  })
  
  output$tip_propUI <- renderUI({
    list(
      numericInput(
        inputId = "tip_prop",
        label = "Tip proportion",
        # label = NULL,
        value = .2,
        min = 0
      )
    )
  })
  
  output$tip_raw <- renderUI({
    money = total_bill$total
    if("Christian" %in% input$members){
      money = money + input$christian_bill
    }
    if("Ian" %in% input$members){
      money = money + input$ian_bill
    }
    if("John" %in% input$members){
      money = money + input$john_bill
    }
    if("Meaghan" %in% input$members){
      money = money + input$meaghan_bill
    }
    if("Samidha" %in% input$members){
      money = money + input$samidha_bill
    }
    
    text <- paste0("<b> (Raw tip: $", round(input$tip_prop * money, 2), ") </b>")
    HTML(text)
  })
  
  total_bill <- reactiveValues(total = 0)
  
  output$split_table <- renderDataTable({
    req(input$tip_prop)
    out <- list()
    ndx = 1
    
    if("Christian" %in% input$members){
      out[[ndx]] <- tibble(
        Member = "Christian",
        Bill = input$christian_bill, 
        `Tip split` = input$tip_prop * input$christian_bill,
        `Awarded winnings` = input$winnings / length(input$members)
      ) %>%
        mutate(
          Total = -1 * (Bill + `Tip split` - `Awarded winnings`)
        )
      
      ndx = ndx + 1
    }
    
    if("Ian" %in% input$members){
      out[[ndx]] <- tibble(
        Member = "Ian",
        Bill = input$ian_bill, 
        `Tip split` = input$tip_prop * input$ian_bill,
        `Awarded winnings` = input$winnings / length(input$members)
      ) %>%
        mutate(
          Total = -1 * (Bill + `Tip split` - `Awarded winnings`)
        )
      
      ndx = ndx + 1
    }
    
    if("John" %in% input$members){
      out[[ndx]] <- tibble(
        Member = "John",
        Bill = input$john_bill, 
        `Tip split` = input$tip_prop * input$john_bill,
        `Awarded winnings` = input$winnings / length(input$members)
      ) %>%
        mutate(
          Total = -1 * (Bill + `Tip split` - `Awarded winnings`)
        )
      
      ndx = ndx + 1
    }
    
    if("Meaghan" %in% input$members){
      out[[ndx]] <- tibble(
        Member = "Meaghan",
        Bill = input$meaghan_bill, 
        `Tip split` = input$tip_prop * input$meaghan_bill,
        `Awarded winnings` = input$winnings / length(input$members)
      ) %>%
        mutate(
          Total = -1 * (Bill + `Tip split` - `Awarded winnings`)
        )
      
      ndx = ndx + 1
    }
    
    if("Samidha" %in% input$members){
      out[[ndx]] <- tibble(
        Member = "Samidha",
        Bill = input$samidha_bill, 
        `Tip split` = input$tip_prop * input$samidha_bill,
        `Awarded winnings` = input$winnings / length(input$members)
      ) %>%
        mutate(
          Total = -1 * (Bill + `Tip split` - `Awarded winnings`)
        )
      
      ndx = ndx + 1
    }
    
    out <- do.call("rbind", out) %>%
      mutate_if(is.numeric, ~ round(., 2)) %>%
      datatable(
        selection = 'none', rownames = '', filter = 'none',
        extensions = "FixedColumns",
        options = list(
          paging = TRUE, searching = TRUE, info = FALSE,
          sort = TRUE, scrollX = TRUE, fixedColumns = list(leftColumns = 2)
        )
      ) %>%
      formatStyle(
        columns = "Total",
        color = styleInterval(cuts = 0, values = c("red", "green")),
        fontWeight = "bold"
      )
  })
}

shinyApp(ui = ui, server = server)

