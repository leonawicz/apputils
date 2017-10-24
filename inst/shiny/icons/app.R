library(shiny)
library(shinydashboard)
library(apputils)
library(purrr)

title1 <- h3("Common statistics and change/trend icons")
title2 <- h3("Simple linear regression (SLR) model themed icons")
desc1 <- h4("The first three rows to the right show statistics pertaining to x, the sample shown in the histogram.")
desc2 <- h4("The next three rows to the right show examples of delta change icons using the values listed below.")

ui <- dashboardPage(
  dashboardHeader(title="Stats icons"),
  dashboardSidebar(
    use_apputils(),
    shiny::tags$style(shiny::HTML(".small-box {height:90px !important;}")),
    sidebarMenu(id = "tab",
      menuItem("Light icons", tabName = "light"),
      menuItem("Dark icons", tabName = "dark")
    ),
    conditionalPanel("input.tab === 'light'",
                     fluidRow(div(valueBoxOutput("distLight", width=12)), style="padding:10px;")),
    conditionalPanel("input.tab === 'dark'",
                     fluidRow(div(valueBoxOutput("distDark", width=12)), style="padding:10px;"))
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "light",
              title1,
              fluidRow(
                box(plotOutput("hist1"),
                    br(), desc1, desc2,
                    verbatimTextOutput("vals1"), status="primary", width=6),
                box(uiOutput("vBoxesLight"), status="primary", width=6)
              ),
              title2,
              fluidRow(
                box(verbatimTextOutput("lm1"), status="primary", width=6),
                box(uiOutput("lmBoxesLight"), status="primary", width=6)
              )
      ),
      tabItem(tabName = "dark",
              title1,
              fluidRow(
                box(plotOutput("hist2"),
                    br(), desc1, desc2,
                    verbatimTextOutput("vals2"), status="primary", width=6),
                box(uiOutput("vBoxesDark"), status="primary", width=6)
              ),
              title2,
              fluidRow(
                box(verbatimTextOutput("lm2"), status="primary", width=6),
                box(uiOutput("lmBoxesDark"), status="primary", width=6)
              )
      )
    )
  ),
  title="Stats icons"
)

server <- function(input, output) {
  clrs <- c("yellow", "orange", "purple", "red", "blue", "navy",
            "light-blue", "teal", "olive", "green", "fuchsia", "maroon",
            rep(c("blue", "light-blue"), times = 5))

  vbox <- function(vb){ # taglist around first 12 value boxes
    tagList(
      fluidRow(
        column(6, vb[[1]], vb[[5]], vb[[3]]),
        column(6, vb[[2]], vb[[6]], vb[[4]])
      ),
      fluidRow(
        column(6, vb[[7]], vb[[8]], vb[[9]]),
        column(6, vb[[10]], vb[[11]], vb[[12]])
      )
    )
  }

  lmbox <- function(vb){ # last 5 value boxes
    fluidRow(
      column(6, vb[[1]], vb[[2]], vb[[3]]),
      column(6, vb[[4]], vb[[5]])
    )
  }

  # image files
  stats1 <- c("mean", "sd", "min", "max", "median", "iqr") # common stats
  stats2 <- c("dec", "inc", "bardec", "pctdec", "pctinc", "barinc") # changes/trends
  stats3 <- c("b0", "b1", "r2", "pvalue", "pvalue")
  s <- c(stats1, stats2, stats3)
  labs1 <- c("Mean", "Std Dev", "Min", "Max", "Median", "IQR")
  labs2 <- c("Total change", "Total change", "Max loss", "% change", "% change", "Max gain")
  labs3 <- c("Intercept", "Slope", "R^2", "p-value", "p-value")
  lb <- c(labs1, labs2, labs3)
  files_white <- map_chr(s, statIcon)
  files_black <- map_chr(s, ~statIcon(.x, "black"))

  # data
  set.seed(1)
  x <- rnorm(1000, 100, 10)
  x1 <- 1:20
  y <- 0.5*x1 + 3 + rnorm(20, sd = 3.5)
  lm1 <- lm(y ~ x1)

  lmvals <- as.numeric(unlist(
    map2(c(summary(lm1)$coefficients[1:2, 1], summary(lm1)$r.squared, summary(lm1)$coefficients[1:2, 4]),
         c(2, 2, 3, 3, 3), ~round(.x, .y))
  ))
  del <- c(-154, 47, -81, "-12%", "114%", 60) # values for delta change example icons
  val <- round(c(mean(x), sd(x), min(x), max(x), median(x)))
  val <- c(val, paste(round(quantile(x, probs = c(0.25, 0.75))), collapse=" - "), del, lmvals)
  val <- map2(val, rep(90, 17), ~pTextSize(.x, .y))
  text <- map(lb, ~pTextSize(.x, 135))

  output$vBoxesLight <- renderUI({
    vb <- map(1:12, ~apputils::valueBox(
      val[[.x]], text[[.x]],
      icon=apputils::icon(list(src=files_white[.x], width="80px"), lib="local"),
      color=clrs[.x], width=NULL)
    )
    vbox(vb)
  })

  output$vBoxesDark <- renderUI({
    vb <- map(1:12, ~apputils::valueBox(
      val[[.x]], text[[.x]],
      icon=apputils::icon(list(src=files_black[.x], width="80px"), lib="local"),
      color=clrs[.x], width=NULL)
    )
    vbox(vb)
  })

  output$distLight <- renderValueBox({
    x <- statIcon("normal")
    apputils::valueBox("Data", "light style icons",
             icon=apputils::icon(list(src=x, width="80px"), lib="local"),
             color="orange", width=NULL)
  })

  output$distDark <- renderValueBox({
    x <- statIcon("normal", "black")
    apputils::valueBox("Data", "dark style icons",
             icon=apputils::icon(list(src=x, width="80px"), lib="local"),
             color="orange", width=NULL)
  })

  output$lmBoxesLight <- renderUI({
    vb <- map(13:17, ~apputils::valueBox(
      val[[.x]], text[[.x]],
      icon=apputils::icon(list(src=files_white[.x], width="80px"), lib="local"),
      color=clrs[.x], width=NULL)
    )
    lmbox(vb)
  })

  output$lmBoxesDark <- renderUI({
    vb <- map(13:17, ~apputils::valueBox(
      val[[.x]], text[[.x]],
      icon=apputils::icon(list(src=files_black[.x], width="80px"), lib="local"),
      color=clrs[.x], width=NULL)
    )
    lmbox(vb)
  })

  output$hist1 <- renderPlot({ hist(x) })
  output$hist2 <- renderPlot({ hist(x) })
  output$vals1 <- renderText({ del })
  output$vals2 <- renderText({ del })
  output$lm1 <- renderPrint({ summary(lm1) })
  output$lm2 <- renderPrint({ summary(lm1) })
}

# Run the application
shinyApp(ui = ui, server = server)

