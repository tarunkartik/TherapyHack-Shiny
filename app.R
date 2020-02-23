#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinysense)
library(opencv)
library(tidyverse)
library(shinydashboard)
library(shinyTime)
library(leaflet)
library("googlesheets")
library("DT")
data("quakes")

# #get your token to access google drive
# shiny_token <- gs_auth() 
# saveRDS(shiny_token, "shiny_app_token.rds")

fieldsMandatory <- c("name", "ID","location","number")
labelMandatory <- function(label) {
    tagList(
        label,
        span("*", class = "mandatory_star")
    )
}

fieldsAll <- c("name", "ID", "Gender", "age", "location","number","time5","time6","ContactType")
responsesDir <- file.path("responses")
epochTime <- function() {
    as.integer(Sys.time())
}

appCSS <-
    ".mandatory_star { color: red; }"

shinyApp(
    ui = navbarPage("TherapyHack",
                    theme = shinythemes::shinytheme("darkly"),  # <--- Specify theme here
                    tabPanel("Form",
                             div(
                                 shinyjs::useShinyjs(),
                                 shinyjs::inlineCSS(appCSS),
                                 id = "form",
                                 textInput("name", "Name", ""),
                                 textInput("ID", "Email ID"),
                                 radioButtons("Gender", label = h4("Gender"),
                                              choices = list("Male" = 1, "Female" = 2, "Other" = 3), 
                                              selected = 1),
                                 sliderInput("age", "Please Enter your Age", 0, 85, 1, ticks = FALSE),
                                 textInput("location", "Address", ""),
                                 textInput("number", "Contact", "513-"),
                                 # timeInput("time3", "Time:", value = strptime("12:34:56", "%T")),
                                 fluidRow(
                                     column(width = 3,
                                         timeInput("time5", "Availability:", value = strptime("12:34:56", "%T"))
                                     ),
                                     column(width = 3,
                                         timeInput("time6", "", value = strptime("12:34:56", "%T"))
                                     )
                                 ),
                                 # timeInput("time5", "Time:", minute.steps = 5),
                                 selectInput("ContactType", "Preferred method of contact",
                                             c("",  "Call", "Text Message", "Email")),
                                 actionButton("submit", "Submit", class = "btn-primary"),
                                 shinyjs::hidden(
                                     span(id = "submit_msg", "Submitting..."),
                                     div(id = "error",
                                         div(br(), tags$b("Error: "), span(id = "error_msg"))
                                     )
                                 )
                                 
                             ),
                             shinyjs::hidden(
                                 div(
                                     id = "thankyou_msg",
                                     h3("Thanks, your response was submitted successfully! Please take the Survey."),
                                     actionLink("submit_another", "Submit another response")
                                 )
                             )
                             ),
                    tabPanel("Survey",
                             textAreaInput("question1", "Are you suicidal", "Enter your response here", width = "1000px"),
                             textAreaInput("question2", "Do you have troubles geting up", "Enter your response here", width = "1000px"),
                             textAreaInput("question3", "Do you like this app", "Enter your response here", width = "1000px"),
                             fileInput('datafile', 'Choose video file',
                                                 accept = c(".mp4")),
                             actionButton("submitSurvey", "Submit")
                             # ocv_video(ocv_face)
                             ),
                    tabPanel("Location", "Showing Doctors closest to you...",
                             br(),
                             br(),
                             # md_cities <- data.frame(
                             #     lat = c(12.980353,13.011625,12.932195,12.971729,12.977751),
                             #     lng = c(77.640894,77.552153,77.685219,77.612790,77.697934)),
                             # md_cities %>%
                             leaflet()%>%
                             addTiles()%>%
                             #     addMarkers(popup= c("Motherhood Hospital", "Narayan Hrudalaya Hospital", "Sakra Hospital", "Manipal Hospital", "Rainbow Hospital"))
                             addMarkers(lng=-84.513782, lat=39.140703))
                    
    ),
    server = function(input, output) {
        observe({
            # check if all mandatory fields have a value
            mandatoryFilled <-
                vapply(fieldsMandatory,
                       function(x) {
                           !is.null(input[[x]]) && input[[x]] != ""
                       },
                       logical(1))
            mandatoryFilled <- all(mandatoryFilled)
            
            # enable/disable the submit button
            shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
        })
        
        formData <- reactive({
            data <- sapply(fieldsAll, function(x) input[[x]])
            data <- c(data, timestamp = epochTime())
            data <- t(data)
            data
        })
        
        saveData <- function(data) {
            fileName <- sprintf("%s_%s.csv",
                                humanTime(),
                                digest::digest(data))
            f <- function(input, output) write.csv(input, row.names = FALSE, file = output)
            
            gcs_upload(data, 
                       object_function = f,
                       type = "text/csv")
            
        }
        humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")
        # output$value <- renderText({ input$question1 })
        observeEvent(input$submitSurvey, {
            df <- data.frame(x=input$question1, y= input$question2, z = input$question3)
            gcs_upload(df,name = "survey")
            # gcs_upload(input$datafile, name = "Video.mp4",
            #            type ="mp4")
        })
        # action to take when submit button is pressed
        observeEvent(input$submit, {
            
            # User-experience stuff
            shinyjs::disable("submit")
            shinyjs::show("submit_msg")
            shinyjs::hide("error")
            
            # Save the data (show an error message in case of error)
            tryCatch({
                saveData(formData())
                shinyjs::reset("form")
                shinyjs::hide("form")
                shinyjs::show("thankyou_msg")
            },
            error = function(err) {
                shinyjs::html("error_msg", err$message)
                shinyjs::show(id = "error", anim = TRUE, animType = "fade")
            },
            finally = {
                shinyjs::enable("submit")
                shinyjs::hide("submit_msg")
            })
        })
        
        # submit another response
        observeEvent(input$submit_another, {
            shinyjs::show("form")
            shinyjs::hide("thankyou_msg")
        })
        
    }
)

