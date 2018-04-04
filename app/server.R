library(shiny)
library(readxl)
library(ggplot2)
library(DT)
library(future)
library(promises)


# DEFINE SERVER LOGIC
shinyServer(function(input, output) {

  
  # REFRESH
  observeEvent(input$resetBtn, {
    js$reload();
  })
  
  
  # GET DATA
  DATA <- reactiveVal()
  CLASSES <- reactiveVal()
  
  # user data
  observeEvent(input$userData, {
    FILE <- input$userData
    if (is.null(FILE)) { return(NULL) }

    future(
      switch(
        input$sourceFormat,
        "csv" = try(read.csv(FILE$datapath)),
        "excel" = try(read_excel(FILE$datapath, na = c("", "NA")))
      )      
    ) %...>%
      DATA()

    CLASSES(
      sapply(DATA(), class)
    )
  })
  
  # sample data
  observeEvent(input$sampleData, {
    DATA(
      switch(
        input$sampleData,
        "iris" = datasets::iris,
        "mtcars" = datasets::mtcars,
        "USArrests" = datasets::USArrests
      )
    )
    
    CLASSES(
      sapply(DATA(), class)
    )
  })


  # GET VIEWS
  output$TABLE <- renderDT({
    DATA()
  })

  output$SUMMARY <- renderPrint({
    summary(DATA())
  })


  # GET PLOT
  observeEvent(input$plotType, {
    
    # show plot vars
    if(input$plotType == "bar") {
      
      # bar chart
      output$selectBarX <- renderUI({
        selectInput(
          inputId = "varXBar",
          label = "X variable",
          choices = c(None = 0, names(DATA())[CLASSES() %in% c("factor", "character")])
        )
      })
      
      output$selectBarY <- renderUI({
        selectInput(
          inputId = "varYBar",
          label = "Y variable",
          choices = c(None = 0, names(DATA())[CLASSES() %in% c("numeric", "integer")])
        )
      })
      
      output$selectBarColor <- renderUI({
        selectInput(
          inputId = "varColorBar",
          label = "Color variable",
          choices = c(None = 0, names(DATA())[CLASSES() %in% c("factor", "character")])
        )
      })
      
    } else if (input$plotType == "hist") {
      
      # histogram
      output$selectHistX <- renderUI({
        selectInput(
          inputId = "varXHist",
          label = "X variable",
          choices = c(None = 0, names(DATA())[CLASSES() %in% c("numeric", "integer")])
        )
      })
    } else if (input$plotType == "line") {
      
      # line chart
      output$selectLineX <- renderUI({
        selectInput(
          inputId = "varXLine",
          label = "X variable",
          choices = c(None = 0, names(DATA())[CLASSES() %in% c("numeric", "integer")])
        )
      })
      
      output$selectLineY <- renderUI({
        selectInput(
          inputId = "varYLine",
          label = "Y variable",
          choices = c(None = 0, names(DATA())[CLASSES() %in% c("numeric", "integer")])
        )
      })
      
      output$selectLineColor <- renderUI({
        selectInput(
          inputId = "varColorLine",
          label = "Color variable",
          choices = c(None = 0, names(DATA())[CLASSES() %in% c("factor", "character")])
        )
      })
    } else if (input$plotType == "point") {
      
      # scatterplot
      output$selectPointX <- renderUI({
        selectInput(
          inputId = "varXPoint",
          label = "X variable",
          choices = c(None = 0, names(DATA())[CLASSES() %in% c("numeric", "integer")])
        )
      })
      
      output$selectPointY <- renderUI({
        selectInput(
          inputId = "varYPoint",
          label = "Y variable",
          choices = c(None = 0, names(DATA())[CLASSES() %in% c("numeric", "integer")])
        )
      })
      
      output$selectPointColor <- renderUI({
        selectInput(
          inputId = "varColorPoint",
          label = "Color variable",
          choices = c(None = 0, names(DATA()))
        )
      })
      
      output$selectPointSize <- renderUI({
        selectInput(
          inputId = "varSizePoint",
          label = "Size variable",
          choices = c(None = 0, names(DATA())[CLASSES() %in% c("numeric", "integer")])
        )
      })
    }
  })
  
  plotType <- eventReactive(input$plotType, {
    isolate(input$plotType)
  })
  
  
  # REDNER PLOT
  observeEvent(input$plotBtn, {
    PLOT <- reactiveVal()
    
    if (plotType() == "bar") {
      
      # bar chart
      if (input$varColorBar == 0) {
        bar_data <- data.frame(
          x = DATA()[, input$varXBar],
          y = DATA()[, input$varYBar]
        )
        PLOT(ggplot(data = bar_data, aes(x, y)))
      } else {
        bar_data <- data.frame(
          x = DATA()[, input$varXBar],
          y = DATA()[, input$varYBar],
          color = DATA()[, input$varColorBar]
        )
        PLOT(ggplot(data = bar_data, aes(x, y, fill = color)))
      }
      
      PLOT(
        PLOT() +
          geom_col() +
          xlab(input$varXBar) +
          ylab(input$varYBar) +
          ggtitle(paste("Bar chart of", input$varXBar, "and", input$varYBar))
      )
      
    } else if (plotType() == "hist") {
      
      # histogram
      hist_data <- data.frame(
        x = DATA()[, input$varXHist]
      )
      
      PLOT(
        ggplot(data = hist_data, aes(x)) +
          geom_histogram(bins = input$bins, color = "white") +
          xlab(input$varXHist) +
          ggtitle(paste("Histogram of", input$varXHist))
      )
      
    } else if (plotType() == "line") {
      
      # line chart
      if(input$varColorLine == 0) {
        line_data <- data.frame(
          x = DATA()[, input$varXLine],
          y = DATA()[, input$varYLine]
        )
        PLOT(ggplot(data = line_data, aes(x, y)))
      } else {
        line_data <- data.frame(
          x = DATA()[, input$varXLine],
          y = DATA()[, input$varYLine],
          color = DATA()[, input$varColorLine]
        )
        PLOT(ggplot(data = line_data, aes(x, y, color = color)))
      }
      
      PLOT(
        PLOT() +
          geom_line(size = input$linewidth) +
          xlab(input$varXLine) +
          ylab(input$varYLine) +
          ggtitle(paste("Line chart of", input$varXLine, "and", input$varYLine))
      )
      
    } else if (plotType() == "point") {
      
      # scatterplot
      if (input$varColorPoint == 0 & input$varSizePoint == 0) {
        point_data <- data.frame(
          x = DATA()[, input$varXPoint],
          y = DATA()[, input$varYPoint]
        )
        
        PLOT(ggplot(data = point_data, aes(x, y)))
      } else if (input$varColorPoint != 0 & input$varSizePoint == 0) {
        point_data <- data.frame(
          x = DATA()[, input$varXPoint],
          y = DATA()[, input$varYPoint],
          color = DATA()[, input$varColorPoint]
        )
        
        PLOT(ggplot(data = point_data, aes(x, y, color = color)))
      } else if (input$varColorPoint == 0 & input$varSizePoint != 0) {
        point_data <- data.frame(
          x = DATA()[, input$varXPoint],
          y = DATA()[, input$varYPoint],
          size = DATA()[, input$varSizePoint]
        )
        
        PLOT(ggplot(data = point_data, aes(x, y, size = size)))
      } else {
        point_data <- data.frame(
          x = DATA()[, input$varXPoint],
          y = DATA()[, input$varYPoint],
          color = DATA()[, input$varColorPoint],
          size = DATA()[, input$varSizePoint]
        )
        
        PLOT(ggplot(data = point_data, aes(x, y, color = color, size = size)))
      }
      
      PLOT(
        PLOT() +
          geom_point(alpha = input$alpha) +
          xlab(input$varXPoint) +
          ylab(input$varYPoint) +
          ggtitle(paste("Scatterplot of", input$varXPoint, "and", input$varYPoint))
      )
    }
    
    # apply theme
    if (input$plotTheme == "theme_bw") {
      PLOT(PLOT() + theme_bw(base_size = 12))
    } else if (input$plotTheme == "theme_classic") {
      PLOT(PLOT() + theme_classic(base_size = 12))
    } else if (input$plotTheme == "theme_minimal") {
      PLOT(PLOT() + theme_minimal(base_size = 12))
    } else {
      PLOT(PLOT() + theme_gray(base_size = 12))
    }
    
    p <- isolate(PLOT())
    output$PLOT <- renderPlot(p)
    
  })

  
  # ENABLE INPUTS
  observe({
    # view type / plot type input
    if (!is.null(DATA())) {
      shinyjs::enable("viewType")
      shinyjs::enable("plotType")
    }
    
    # generate plot button
    observeEvent(input$plotType, {
      if (input$plotType == "bar") {
        observeEvent(c(input$varXBar, input$varYBar), {
          if(input$varXBar != 0 && input$varYBar != 0) {
            shinyjs::enable("plotBtn")
          } else {
            shinyjs::disable("plotBtn")
          }
        })
      } else if (input$plotType == "hist") {
        observeEvent(input$varXHist, {
          if(input$varXHist != 0) {
            shinyjs::enable("plotBtn")
          } else {
            shinyjs::disable("plotBtn")
          }
        })
      } else if (input$plotType == "line") {
        observeEvent(c(input$varXLine, input$varYLine), {
          if(input$varXLine != 0 && input$varYLine != 0) {
            shinyjs::enable("plotBtn")
          } else {
            shinyjs::disable("plotBtn")
          }
        })
      } else if (input$plotType == "point") {
        observeEvent(c(input$varXPoint, input$varYPoint), {
          if(input$varXPoint != 0 && input$varYPoint != 0) {
            shinyjs::enable("plotBtn")
          } else {
            shinyjs::disable("plotBtn")
          }
        })
      } else {
        shinyjs::disable("plotBtn")
      }
    })
  })
  
  
  # DOWNLAOD PLOT
  output$downloadBtn <- downloadHandler(
    filename = function() {
      paste0("plot", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(filename = file, dpi = 600)
    }
  )
})
