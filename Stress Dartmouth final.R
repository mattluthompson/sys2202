#Benjamin Weisel bmw7rx
library(shiny)
ui <- fluidPage(
  titlePanel("Stress Predictions"),
  
  sidebarPanel(
    numericInput(inputId="wknd_walking_mean",
                 label="Around how many minutes do you spend walking on an average weekend?",
                 value=1),
    br(),
    
    numericInput(inputId="wknd_running_mean",
                 label="Around how many minutes do you spend running on an average weekend?",
                 value=1),
    br(),
    
    numericInput(inputId="wkdy_running_mean",
                 label="Around how many minutes do you spend running on an average weekday?",
                 value=1),
    br(),
    
    numericInput(inputId="wknd_convo_length",
                 label="Around how many minutes a weekend do you spend in conversation with others?",
                 value=1),
    br(),
    
    numericInput(inputId="deadlines",
                 label="Around how many deadlines do you have per week for school?",
                 value=1),
    br(),
    
    numericInput(inputId="contributions",
                 label="Around how many times do you contribute to a class discusion in an average week (i.e. Piazza, discussion pages, etc.)?",
                 value=1),
    br(),
    
    selectInput(inputId="sleep_night", 
                label="On average, how many hours of sleep do you recieve in a night?",
                choices = list("<3" = 1, "3.5" = 2, "4" = 3, "4.5" = 4, "5" = 5, "5.5" = 6, "6" = 7, "6.5" = 8, "7" = 9,
                               "7.5" = 10, "8" = 11, "8.5" = 12, "9" = 13, "9.5" = 14, "10" = 15, "10.5" = 16, "11" = 17, "11.5" = 18, "12" = 19)),
    
    br(),
    
    sliderInput(inputId="sleep_quality",
                label="On average, how would you rate the quality of your sleep (4 being the best)?",
                value=2,min=1,max=4),
    br(),
    
    sliderInput(inputId="work_alone",
                label="How much do you enjoy working alone (5 being the most enjoyment)?",
                value=3,min=1,max=5),
    br(),
    
    sliderInput(inputId="relax_others",
                label="How much do you enjoy relaxing with others (5 being the most enjoyment)?",
                value=3,min=1,max=5),
    br(),
    
    sliderInput(inputId="work_others",
                label="How much do you enjoy working with others (5 being the most enjoyment)?",
                value=3,min=1,max=5),
    br(),br(),
    
    sliderInput(inputId="anxious",
                label="Do you think of yourself as anxious/easily upset? (1 = Not at all, 5 = extremely)",
                value=3,min=1,max=5),
    br(),
    
    sliderInput(inputId="warm",
                label="Do you think of yourself as sympathetic/warm? (1 = Not at all, 5 = extremely)",
                value=3,min=1,max=5),
    br(),
    
    sliderInput(inputId="careless",
                label="Do you think of yourself as careless/disorganized? (1 = Not at all, 5 = extremely)?",
                value=3,min=1,max=5),
    br(),
    
    sliderInput(inputId="calm",
                label="Do you think of yourself as calm/emotionally stable? (1 = Not at all, 5 = extremely)?",
                value=3,min=1,max=5),
    br(),
    
    sliderInput(inputId="dependable",
                label="Do you think of yourself as dependable/self-disciplined? (1 = Not at all, 5 = extremely)?",
                value=3,min=1,max=5),
    br(),
    
    selectInput(inputId="walk", 
                label="How much time do you spend walking on an average day?",
                choices = list("None" = 0, "<30 minutes" = 1,
                               "30-60 minutes" = 2, "60-90 minutes" = 3, ">90 minutes" = 4)),
    
    br(),br(),
    actionButton("submit", label = "Submit"),
    br(),br()
  ),
  
  mainPanel(
    strong("The mental health score is a number, with higher values indicating a higher correlation with common
           stressors of students."),
    br(),br(),
    textOutput("answer"),
    br(),
    
    
    plotOutput("plot1", click = "plot_click")
  )
)



server <- function(input,output){
  
  answer <- eventReactive(input$submit,{
    (3.392+(-21.42*(input$wknd_walking_mean/2880))+(-20.99*as.numeric(input$wknd_running_mean/2880))+(56.33*as.numeric(input$wkdy_running_mean/7200))+(.001835*as.numeric(input$wknd_convo_length))
     +(.4843*as.numeric(input$deadlines))+(.1910*as.numeric(input$contributions))+(.1445*as.numeric(input$sleep_night))
     +(.4021*as.numeric(input$sleep_quality))+(.3099*as.numeric(input$work_alone))+(.2637*as.numeric(input$relax_others))+(-.6628*as.numeric(input$work_others))+(-.6138*as.numeric(input$anxious))
     +(.4416*as.numeric(input$warm))+(-.4160*as.numeric(input$careless))+(-.625*as.numeric(input$calm))+(-.644*as.numeric(input$dependable))+(.1968*as.numeric(input$walk)))
  })
  
  output$answer <- renderText({
      paste("Your mental health score is ", (answer()), ".")
  })
  
  output$plot1 <- renderPlot({
    x <- seq(0, 5, by = .1)
    y <- dnorm(x, mean = 2.220, sd = 0.5)
    plot(x, y)
  })
  
}

shinyApp(ui = ui, server = server)