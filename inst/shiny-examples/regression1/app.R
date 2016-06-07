pts <- rbind(c(1,1), c(2,2), c(3,3))
sel_pt <- 0
DIST_THRES <- 0.1

shinyApp(
  ui = fluidPage(
    # Some custom CSS
    tags$head(
      tags$style(
        HTML(
          "
           .option-group {
           border: 1px solid #ccc;
           border-radius: 6px;
           padding: 0px 5px;
           margin: 5px -10px;
           background-color: #f5f5f5;
           }

           .option-header {
           color: #79d;
           text-transform: uppercase;
           margin-bottom: 5px;
           }
          "
        )
      )
    ),
    fluidRow(
      column(width = 3,
             div(class = "option-group",
                 div(class = "option-header", "Generation"),
                 numericInput("par_n", "N: ", 20),
                 selectInput("par_d", "True degree: ", choices = 0:3,
                            selected = 1),
                 numericInput("par_seed", "seed: ", 0)
             )),
      column(width = 3,
             div(class = "option-group",
                 div(class = "option-header", "Fit"),
                 selectInput("fit_d", "Degree: ", choices = 0:3,
                             selected = 1),
                 selectInput("fit_d", "Objective: ",
                             choices = c("Absolute", "Squared"),
                             selected = "Squared"),
                 radioButtons("fit_hide", "Hide SSE?:",
                              choices = c("Hide", "Show"),
                              selected = "Hide")
             ))
    ),
    fluidRow(
      column(width = 4,
             uiOutput("plotUI1"))
    )
  ),
  
  server = function(input, output, session) {
    curData <- reactive({
      set.seed(input$par_seed)
    })
    curPoints <- reactive({
      npts <- as.numeric(input$fit_d) + 1
      if (!is.null(input$plot1_click)) {
        p1c <- c(input$plot1_click$x, input$plot1_click$y)
        dd <- colSums((t(pts) - p1c)^2)
        if (sel_pt != 0) {
          pts[sel_pt, ] <<- p1c
          sel_pt <<- 0
        }
        if (sel_pt == 0 && min(dd) < DIST_THRES) {
          sel_pt <<- which.min(dd)
        }
      }
      pts[1:npts, ]
    })
    output$plotUI1 <- renderUI({
      plotOutput("plot1", height = 300,
                 click = "plot1_click")
    })
    output$plot1 <- renderPlot({
      plot(curPoints(), xlab = "x", ylab = "y", xlim = c(0, 4), ylim = c(0, 4))
      if (sel_pt != 0) {
        points(curPoints()[sel_pt, , drop=FALSE], cex = 3, col = "red")
      }
    })
  }
)