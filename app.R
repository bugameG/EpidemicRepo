library(tidyverse)
library(shiny)
library(shinydashboard)
library(deSolve)
library(bslib)
library(thematic)
library(ggthemes)
library(DT)
library(plotly)


# Importing SIR Model
sir_model <- readRDS("SIR.rds")

# Setting thematic theme for plots
thematic_on(bg = "auto", 
            fg = "auto",
            accent = "auto",
            font = "auto")


# Setting ggplot theme
theme_set(theme_clean())


# User-Interface 
ui <- fluidPage(
    # Application title
    titlePanel("Epidemic Progression - SIR Deterministic Model"),
      theme = bs_theme(bg = "#F5F5F5",
                       fg = "#000000",
                       primary = "#CC6600",
                       secondary = "#CC6600",
                       font_scale = 0.85),
    # Sidebar inputs 
    sidebarLayout(fluid = TRUE,
        sidebarPanel(width = 3,
            numericInput("N",
                        "Population Size:",
                        min = 0, step = 1,
                        value = 1000),
            br(),
            numericInput("IID",
                        "Initial number of infectious people:",
                        min = 0, step = 1,
                        value = 10),
            br(),
            numericInput("timeID",
                        "Number of time steps:",
                        min = 0, step = 1,
                        value = 100),
            br(),
            br(),br(),
            numericInput("transmissionID",
                        "Transmission rate (\u03B2)",
                        min = 0,
                        max = 1, step = 0.00001,
                        value = 0.0002),
            textOutput("transmissioninfo"),
            br(),br(),
            numericInput("recoveryID",
                        "Recovery rate (\u03B3)",
                        min = 0,
                        max = 1, step = 0.00001,
                        value = 0.01),
            textOutput("recoveryinfo"),
            br(),br(),
        ),

        # A plot showing disease progression
        mainPanel(
          fluidRow(
            column(width=9,
              plotlyOutput("epidemicPlot"),
                br(),
              DTOutput("epidemicTable")),
           column(width=3,
              infoBoxOutput("infectionPeak"),
                br(),
              infoBoxOutput("rn"),
                br(),
              textOutput("less"),
              textOutput("great"),
                br(),br(),
              textOutput("socials"),
                br(),
              downloadButton("dlplot","Download plot", icon = icon("image", lib = "font-awesome")),
                br(),
              downloadButton("dlcsv","Download data", icon = icon("table", lib = "font-awesome")),
                br(),br(),
              infoBoxOutput("github"),
              infoBoxOutput("x"),
              infoBoxOutput("email"),
              infoBoxOutput("kaggle"),
              infoBoxOutput("profile"),
              infoBoxOutput("cell")
              )
        ))
    )
)





# Define server logic 
server <- function(input, output) {
  
    reactive_sir_data <- reactive({
      # SETTING INITIAL STATES AND PARAMETER VALUES
      S_0 <- input$N - input$IID - 0
      initial_state<-c(S = S_0, I = input$IID ,R = 0)
      parameters <- c(beta = input$transmissionID, 
                      gamma = input$recoveryID, N = sum(initial_state))
      # SIR MODEL
      sir_model <- readRDS("SIR.rds")
      
      # SETTING THE TIME RANGE
      times <- seq(from = 0, to = max(input$timeID), by = 1)
      # 
      # # SOLVING THE SIR ODE
      solution <- ode(y=initial_state,times = times,
                      func = sir_model,parms = parameters) |>
        as.data.frame()
      
      sltn <- solution |>
        rename("susceptible" = S,"infectious" = I,"recovered" = R) |>
        mutate("susceptible" = (susceptible),
               "infectious" = (infectious),
               "recovered" = (recovered)); sltn 
      
      
    })
  

      output$transmissioninfo <- renderText({
      paste("Transmission rate represents the rate","\n", 
            "at which susceptible individuals","\n" ,"become infected")
    })
    
    output$recoveryinfo <- renderText({
      paste("Recovery rate is the proportion of infectious" ,"\n",
            "individuals who recover", "\n","from the disease per unit time")
    })
    
    output$epidemicPlot <- renderPlotly({
      
      # SETTING INITIAL STATES AND PARAMETER VALUES
      S_0 <- input$N - input$IID - 0
      initial_state<-c(S = S_0, I = input$IID ,R = 0)
      parameters <- c(beta = input$transmissionID, 
                      gamma = input$recoveryID, N = sum(initial_state))

      # SETTING THE TIME RANGE
      times <- seq(from = 0, to = max(input$timeID), by = 1)
      # 
      # # SOLVING THE SIR ODE
      solution <- ode(y=initial_state,times = times,
                      func = sir_model,parms = parameters) |>
        as.data.frame()
      
      sltn <- solution |>
        rename("susceptible" = S,"infectious" = I,"recovered" = R) |>
        mutate("susceptible" = (susceptible),
               "infectious" = (infectious),
               "recovered" = (recovered))
      
    
      # SIR MODEL LONG FORMAT
      sltn.long = sltn |> 
        pivot_longer(cols=susceptible:recovered,
                     names_to = "condition",
                     values_to = "number") 
      
      # CONDITION TO FACTOR
      sltn.long$condition <- as.factor(sltn.long$condition)
      
      # PLOTTING RESULTS
      ep.plot <- ggplot(data = sltn.long, aes(x = time, y = ceiling(number), color = condition))+
        geom_line()+
        labs(y = "number",
             title = paste("Disease Progression; ","Population size:",input$N,
                           "\n","Initial Infected:",input$IID,
                           ",Transmission rate:",input$transmissionID,",Recovery rate:",input$recoveryID))+
        scale_color_discrete(name="Condition", labels=c("Susceptible","Infectious","Recovered"))+
        theme_clean() 
      ggplotly(ep.plot) |> 
        config(displayModeBar = FALSE) 
      
    })

       output$epidemicTable <- renderDT({
         
        # SETTING INITIAL STATES AND PARAMETER VALUES
        S_0 <- input$N - input$IID - 0 
        initial_state <- c(S = S_0, I = input$IID ,R = 0)
        parameters <- c(beta = input$transmissionID,gamma = input$recoveryID, 
                        N = sum(initial_state))
        
        # SETTING THE TIME RANGE
        times <- seq(from = 0, to = max(input$timeID), by = 1)
        
        # # SOLVING THE SIR ODE
        solution <- ode(y=initial_state,times = times,
                        func = sir_model,parms = parameters) |>
          as.data.frame()
        
        sltn <- solution |>
          rename("susceptible"=S,"infectious"=I,"recovered"=R) |>
          mutate("susceptible" = ceiling(susceptible),
                 "infectious" = ceiling(infectious),
                 "recovered" = ceiling(recovered)) 
        

        # TABLING RESULTS
          sltn |>
            select(time, susceptible, infectious, recovered) |>
            datatable(class = "display",style = "bootstrap4")
        
      }
)
       
       output$infectionPeak <- renderInfoBox({
         
         # SETTING INITIAL STATES AND PARAMETER VALUES
         S_0 <- input$N - input$IID - 0 
         initial_state <- c(S = input$N, I = input$IID ,R = 0)
         parameters <- c(beta = input$transmissionID,gamma = input$recoveryID, 
                         N = sum(initial_state))
         
         # SETTING THE TIME RANGE
         times <- seq(from = 0, to = max(input$timeID), by = 1)
         # 
         # # SOLVING THE SIR ODE
         solution <- ode(y=initial_state,times = times,
                         func = sir_model,parms = parameters) |>
           as.data.frame()
         
         sltn <- solution |>
           rename("susceptible"=S,"infectious"=I,"recovered"=R) |>
           mutate("susceptible" = ceiling(susceptible),
                  "infectious" = ceiling(infectious),
                  "recovered" = ceiling(recovered)) 
         
         peak <- max(sltn$infectious) # DISEASE PEAK
         peak_time <- sltn[sltn$infectious==max(sltn$infectious),"time"][1] # TIME OF DISEASE PEAK
         
         
         shinydashboard::infoBox(peak,
           subtitle  = paste("Infections peak at time",peak_time),
           color = "aqua",fill=TRUE
         ) 
       })
       
       output$rn <- renderInfoBox({
         
         shinydashboard::infoBox(round(input$transmissionID/input$recoveryID,2),
                  subtitle = "Reproduction number, R",color = "aqua",fill = TRUE) 
         
       })
       
       output$less <- renderText({
         paste("If R < 1 , the disease dies out")
       })
       
       output$great <- renderText({
         paste("If R > 1 , the disease may be epidemic")
       })
       
       output$socials <- renderText({
         paste("**********")
       })

       output$github <- renderInfoBox({
         infoBox(title= "",subtitle="bugameG",
                 icon = icon("github",lib = "font-awesome"),
                 href = "https://github.com/bugameG")
       })
       
       output$x <- renderInfoBox({
         infoBox(title= "",subtitle="@bugameG",
                 icon = icon("twitter",lib = "font-awesome"),
                 href = "https://x.com/bugameG")
       })
       
       output$email <- renderInfoBox({
         infoBox(title= "",subtitle="giftbugame@yahoo.com",
                 icon = icon("envelope",lib = "font-awesome"),
                 href = "giftbugame@yahoo.com")
       })
       
       output$kaggle <- renderInfoBox({
         infoBox(title= "",subtitle="kaggle",
                 icon = icon("globe",lib = "font-awesome"),
                 href = "https://www.kaggle.com/giftpamba")
       })
       
       output$profile <- renderInfoBox({
         infoBox(title= "",subtitle="Gift Pamba",
                 icon = icon("briefcase",lib = "font-awesome"),
                 href = "https://datascienceportfol.io/giftpamba")
       })
       
       output$cell <- renderInfoBox({
         infoBox(title= "",subtitle="+254757597966",
                 icon = icon("phone",lib = "font-awesome"),
                 href = "+254757597966")
       })
       
        # DOWNLOADS
        ## GGPLOT
        output$dlplot <- downloadHandler(
          filename = function(){
            paste("SIR_Plot",Sys.Date(),".jpg",sep="")
          },
          content = function(file){
            epidemic_dl_table <- reactive_sir_data()
            epidemic_dl_long_table <- pivot_longer(epidemic_dl_table,
                  cols = susceptible:recovered, names_to = "condition",
                  values_to = "number")
            
            epidemic_dl_long_table$condition <- as.factor(epidemic_dl_long_table$condition)
            
            sir_plot <- ggplot(data = epidemic_dl_long_table, aes(x = time, y = ceiling(number), color = condition))+
              geom_line()+
              labs(y = "number",
                   title = paste("Disease Progression"
                                 ,"\n","N:",input$N,",Initial Infected:",input$IID,
                                 ",Transmission rate:",input$transmissionID,",Recovery rate:",input$recoveryID))+
              scale_color_discrete(name="Condition",
                                   limits=c("infectious","recovered","susceptible"),
                                   labels=c("Infectious","Recovered","Susceptible"))+
              theme_clean()
            
            ggsave(file, plot = sir_plot, width = 8, height = 5)
          }
        )

        ## DATATABLE
        output$dlcsv <- downloadHandler(
          filename = function(){
            paste("SIR_Table.csv")
          },
          content = function(file){
            write_csv(reactive_sir_data(), file)
          }
        )

}


# Run the application 
shinyApp(ui = ui, server = server)
