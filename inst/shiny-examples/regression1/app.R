pts <- rbind(c(1,1), c(2,2), c(3,3))
sel_pt <- 0
DIST_THRES <- 0.1
MIN_X <- -2
MAX_X <- 2

shinyApp(
  ui = fluidPage(
    sidebarLayout(
      sidebarPanel(
        tabsetPanel(
          id = "controls",
          tabPanel("Generation",
                   numericInput("par_n", "N: ", 10),
                   selectInput("par_d", "True degree: ", choices = 0:3,
                               selected = 1),
                   selectInput("par_distro", "Noise type: ", choices = c("Normal", "Laplacian", "Cauchy"),
                               selected = "Normal"),
                   numericInput("par_sigma", "noise:", 1),
                   numericInput("par_seed", "seed: ", 0)
                   ),
          tabPanel("Fit",
                   selectInput("fit_d", "Degree: ", choices = 0:3,
                               selected = 1),
                   selectInput("fit_d", "Objective: ",
                               choices = c("Absolute", "Squared"),
                               selected = "Squared"),
                   checkboxInput("fit_sse", "Show objective", FALSE),
                   checkboxInput("fit_opt", "Show optimal fit", FALSE)
                   ))
      ),
      mainPanel(
        uiOutput("plotUI1")
      )
    )
  ),

  server = function(input, output, session) {
    curData <- reactive({
      d <- as.numeric(input$par_d)
      set.seed(input$par_seed)
      bt <- rnorm(1 + d)
      n <- input$par_n
      x1 <- runif(input$par_n) * (MAX_X - MIN_X) + MIN_X
      xs <- do.call(cbind, lapply(0:d, function(i) x1^i))
      noises <- input$par_sigma * switch (input$par_distro,
        "Normal" = rnorm(n),
        "Laplacian" = rexp(n) * (2 * rbinom(n) - 1),
        "Cauchy" = rcauchy(n)
      )
      ys <- xs %*% bt + noises
      data.frame(x = x1, y = ys)
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
      plotOutput("plot1", height = 500,
                 click = "plot1_click")
    })
    output$plot1 <- renderPlot({
      plot(curData(), xlab = "x", ylab = "y", xlim = c(MIN_X, MAX_X))
      # if (sel_pt != 0) {
      #   points(curPoints()[sel_pt, , drop=FALSE], cex = 3, col = "red")
      # }
    })
  }
)
