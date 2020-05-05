#Benjamin Weisel bmw7rx
library(shiny)
ui <- fluidPage(
  titlePanel("UVA Stress Predictions"),
  
  sidebarPanel(
    br(),

  numericInput(inputId="age",
                label="Enter your age.",
                value=1),
  br(),
    
  sliderInput(inputId="DaysWellRested",
              label="On average, how many days in a week do you feel well rested?",
              value=4,min=0,max=7),
  br(),
  
  radioButtons(inputId="Diff_to_Handle_Academics",
               label="In the last 12 months, have your academics been extremely difficult to handle?",
               choices = list("No"=0, "Yes"=1)),
  br(),
  
  radioButtons(inputId="Diff_to_Handle_Death",
               label="In the last 12 months, has the death of a family member or friend been extremely difficult to handle?",
               choices = list("No"=0, "Yes"=1)),
  br(),
  
  radioButtons(inputId="Diff_to_Handle_Family",
               label="In the last 12 months, have family problems been extremely difficult to handle?",
               choices = list("No"=0, "Yes"=1)),
  br(),
  
  radioButtons(inputId="Diff_to_Handle_Relationship",
               label="In the last 12 months, have intimate relationship problems been extremely difficult to handle?",
               choices = list("No"=0, "Yes"=1)),
  br(),
  
  radioButtons(inputId="Diff_to_Handle_Sleep",
               label="In the last 12 months, have sleep problems been extremely difficult to handle?",
               choices = list("No"=0, "Yes"=1)),
  br(),
  
  sliderInput(inputId="Safe_Community_Night",
              label="On a scale of 1 to 4, how safe do you feel in your community at night? (4 being the most safe)",
              value=2,min=1,max=4),
  br(),
  
  radioButtons(inputId="Mobility_Disability",
               label="Do you have any kind of mobility disability?",
               choices = list("No"=0, "Yes"=1)),
  br(),
  
  radioButtons(inputId="Hopeless1",
               label="Have you ever felt like things were hopeless?",
               choices = list("No"=0, "Yes"=1)),
  br(),
  
  radioButtons(inputId="Exhausted1",
               label="Have you ever felt overly exhausted (not from physical activity)?",
               choices = list("No"=0, "Yes"=1)),
  br(),
  
  radioButtons(inputId="Anxiety1",
               label="Have you ever felt overwhelming anxiety?",
               choices = list("No"=0, "Yes"=1)),
  br(),

  selectInput(inputId="gpa", 
              label="Select your cumulative GPA range.",
              choices = list("A" = 0, "B" = 1,
                             "C" = 2)),
  br(),br(),
  actionButton("submit", label = "Submit"),
  br(),br()
  
  ),
  
  mainPanel(
    strong("The mental health score is a value between 1 and 4, with higher values indicating a higher correlation with common
           stressors of UVA students."),
    br(),br(),
    textOutput("answer"),
    br(),
    textOutput("suggestions")
  )

  
)



server <- function(input,output){
  
  #VARIOUS POSSIBLE OUTPUTS
  answer <- eventReactive(input$submit,{
    (2.02924+(0.02196*input$age)+(-.06089*as.numeric(input$DaysWellRested))+(0.29616*as.numeric(input$Diff_to_Handle_Academics))+(0.25976*as.numeric(input$Diff_to_Handle_Death))
     +(0.21881*as.numeric(input$Diff_to_Handle_Family))+(0.20090*as.numeric(input$Diff_to_Handle_Relationship))+(0.14290*as.numeric(input$Diff_to_Handle_Sleep))
     -(0.08216*as.numeric(input$Safe_Community_Night))+(0.24470*as.numeric(input$Hopeless1))+(0.19117*as.numeric(input$Exhausted1))+(0.21447*as.numeric(input$Anxiety1)))
  })
  
  #CHOOSING OUTPUT 
  output$answer <- renderText({
    #GPA of B, all four year options
    if (input$gpa==1){
      answer <- answer()-.13497
      paste("Your mental health score is ", (answer()))
    }
    
    #GPA of A or C, all four year options
    else if (input$gpa==0 || input$gpa==2){
      paste("Your mental health score is ", (answer()), ".")
    }
  })
#SUGGESTIONS
  output$suggestions <- renderText({
    if (answer() <1.5){
    paste("You may have very low stress.")
    }
    else if (answer() <3){
      paste("You may have moderate stress. Suggestions to reduce stress include:")
    if (input$DaysWellRested < 5){
      paste("get more sleep bitch")
    }
    }
    else if (answer() >3){
      paste("You may have very high stress. Suggestions to reduce stress include:")
      if (input$DaysWellRested < 5){
        paste("get more sleep bitch")}
    }
    })
}

shinyApp(ui = ui, server = server)
