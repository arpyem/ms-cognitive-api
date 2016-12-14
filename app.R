# Microsoft Cognitive Services APIs in R

# LIBRARIES ====

library(shiny)
library(shinyjs)
library(httr)
library(jsonlite)
library(jpeg)

"http://i.dailymail.co.uk/i/pix/2015/09/17/06/2C68F1F800000578-3237858-image-a-68_1442468175398.jpg"


# UI ====

ui <- fluidPage(
        theme = "theme.css",
        verbatimTextOutput("test"),
        
        selectInput(inputId = "choose_api", label = "Which API do you want to call?", choices = c("Face", "Emotion")),
        uiOutput(outputId = "msg_key"),
        passwordInput(inputId = "key", label = NULL),
        radioButtons(inputId = "choose_method", label = NULL, choices = list("Link a JPEG" = 1, "Upload a JPEG" = 2)),
        textInput(inputId = "jpg_url", label = "Link to .jpg image"),
        fileInput(inputId = "jpg_file", label = "Upload a .jpg image"),
        checkboxInput(inputId = "face_id", label = "id"),
        checkboxInput(inputId = "face_landmarks", label = "landmarks"),
        checkboxGroupInput(inputId = "face_attributes", label = "Face attributes", choices = c("age", "gender", "headPose", "smile", "facialHair", "glasses")),
        actionButton(inputId = "post", label = "Call API")
)


# SERVER ====

server <- function(input, output, session) {
        
        output$test <- renderPrint({
                result()
        })
        
        output$msg_key <- renderUI({
                list(
                        "Enter your API key ",
                        "(click ", 
                        tags$a(href = "https://www.microsoft.com/cognitive-services/en-us/sign-up", "here"),
                        " to sign up for free if you don't have one!)"
                )
        })
        
        request_url <- reactive({
                
        })
        
        request <- reactive({
                url <- if (input$choose_api == "Face") {
                        if (input$face_id) {id <- "true"} else {id <- "false"}
                        if (input$face_landmarks) {landmarks <- "true"} else {landmarks <- "false"}
                        link <- paste0("https://api.projectoxford.ai/face/v1.0/detect?returnFaceId=", id, "&returnFaceLandmarks=", landmarks)
                        
                        if (length(input$face_attributes) > 0) {
                                paste0(link, "&returnFaceAttributes=", paste0(input$face_attributes, collapse = ","))
                        } else {
                                link
                        }
                } else {
                        "https://api.projectoxford.ai/emotion/v1.0/recognize"
                }
                
                if (input$choose_method == 1) {
                        type <- "application/json"
                        body <- list(url = input$jpg_url)
                } else {
                        type <- "application/octet-stream"
                        body <- writeJPEG(readJPEG(input$jpg_file$datapath), raw())
                }
                
                list(
                        url = url,
                        type = type,
                        body = body
                )
        })
        
        
        result <- eventReactive(input$post, {
                result <- POST(
                        url = request()$url,
                        content_type(request()$type),
                        add_headers(.headers = c("Ocp-Apim-Subscription-Key" = input$key)),
                        body = request()$body,
                        encode = "json"
                )
                
                content(result)
        })
        
        
        
        
}


# RUN ====

shinyApp(ui = ui, server = server)
