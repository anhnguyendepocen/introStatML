sel_pt <- 0
DIST_THRES <- 0.1
MIN_X <- -2
MAX_X <- 2
RESO <- 10
RESO2 <- 100
pts <- cbind(c(1/6, 5/6, 3/6, 2/6, 4/6) * (MAX_X - MIN_X) + MIN_X, 0)

xterms <- c("", " * x", " * x^2", " * x^3", " * x^4", " * x^4")
eqstr <- function(bt) {
  bt <- floor(bt * RESO2)/RESO2
  paste("y = ", paste(paste(bt, xterms[1:length(bt)], sep = ""), collapse = " + "))
}

bth <- function(pts, dfit) {
  px <- do.call(cbind, lapply(0:dfit, function(i) pts[, 1]^i))
  pbt <- MASS::ginv(px) %*% pts[, 2]
  pbt
}

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
                   numericInput("par_sigma", "noise:", 0.1),
                   numericInput("par_seed", "seed: ", 0)
                   ),
          tabPanel("Fit",
                   selectInput("fit_d", "Degree: ", choices = 0:3,
                               selected = 1),
                   selectInput("fit_choice", "Objective: ",
                               choices = c("Absolute", "Squared"),
                               selected = "Squared"),
                   checkboxInput("fit_pts", "Manually select points", FALSE),
                   checkboxInput("fit_sse", "Show objective", FALSE),
                   checkboxInput("fit_opt", "Show optimal fit", FALSE),
                   DT::dataTableOutput('mytable')
                   )),
        width = 6
      ),
      mainPanel(
        uiOutput("plotUI1"),
        width = 4
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
        "Laplacian" = rexp(n) * (2 * rbinom(n, 1, 0.5) - 1),
        "Cauchy" = rcauchy(n)
      )
      ys <- xs %*% bt + noises
      x1 <- floor(x1 * RESO)/RESO
      ys <- ys - 0.7 * mean(ys)
      ys <- floor(ys * RESO)/RESO
      o <- order(x1)
      data.frame(x = x1[o], y = ys[o])
    })
    curTable <- reactive({
      tab <- curData()
      dfit <- as.numeric(input$fit_d)
      if (input$fit_pts) {
        ptz <- curPoints()
        bth <- bth(ptz, dfit)
        xs <- do.call(cbind, lapply(0:dfit, function(i) tab$x^i))
        tab$fit.y <- floor(as.numeric(xs %*% bth) * RESO2)/RESO2
      }
      if (input$fit_sse && "fit.y" %in% names(tab)) {
        tab$diff <- tab$y - tab$fit.y
        if (input$fit_choice == "Squared") {
          tab$squared <- tab$diff^2
        }
        if (input$fit_choice == "Absolute") {
          tab$abs <- abs(tab$diff)
        }
        tcost <- sum(tab[, 5])
        tab[, 5] <- floor(tab[, 5] * RESO2)/RESO2
        tot <- tab[1, ]
        tot[[1]] <- NA
        tot[[2]] <- NA
        tot[[3]] <- NA
        tot[[4]] <- NA
        tot[[5]] <- tcost
        tab <- rbind(total = tot, tab)
        rownames(tab) <- c("total", paste(1:(nrow(tab) - 1)))
      }
      tab
    })
    curPoints <- reactive({
      dfit <- as.numeric(input$fit_d)
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
      pts[1:npts, , drop = FALSE]
    })
    output$plotUI1 <- renderUI({
      plotOutput("plot1", height = 400,
                 click = "plot1_click", dblclick = "plot1_dbl")
    })
    output$plot1 <- renderPlot({
      dat <- curData()
      plot(dat, xlab = "x", ylab = "y", xlim = c(MIN_X, MAX_X))
      d <- as.numeric(input$par_d)
      dfit <- as.numeric(input$fit_d)
      if (input$fit_pts) {
        ptz <- curPoints()
        for (i in 1:(dfit + 1)) {
          cc <- "blue"; cx <- 1
          if (i == sel_pt) {
            cc <- "red"
            cx <- 1.5
          }
          points(ptz[i, , drop = FALSE], pch = 9, col = cc, cex = cx)
        }
      }
      if (input$fit_pts) {
        ptz <- curPoints()
        sx <- seq(MIN_X, MAX_X, length.out = 100)
        sX <- do.call(cbind, lapply(0:dfit, function(i) sx^i))
        sy <- as.numeric(sX %*% bth(ptz, dfit))
        lines(sx, sy, col = "blue")
        title(eqstr(bth(ptz, dfit)))
      }
    })
    output$mytable <- DT::renderDataTable({
      DT::datatable(curTable())
    })
  }
)
