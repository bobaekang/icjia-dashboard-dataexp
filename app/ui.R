library(shiny)
library(shinycssloaders)
library(shinyjs)
library(DT)


# JAVASCRIPT
jscode <- "shinyjs.reload = function() { location.reload(); }"


# DEFINE UI
#-------------------------------------------------------------------------------
shinyUI(fluidPage(
  
  # HEAD
  tags$head(
    includeCSS(path = "css/style.css"),
    tags$title("ICJIA Data Explorer")
  ),

  # SHINYJS
  useShinyjs(),
  extendShinyjs(text = jscode, functions = "reload"),

  # TITLE
  titlePanel(
    tags$div(
      img(id="logo", src="icjia.png"),
      span(id="text-identity", "ICJIA data explorer")
    )
  ),

  # SIDEBAR
  #-----------------------------------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      # About sidebar
      conditionalPanel(
        condition="input.tabselected==1",
        radioButtons(
          "sourceType",
          "Select a data source:",
          choices = c(
            "Upload file" = "upload",
            "Use sample" = "sample"
          ),
          selected = ""
        ),
        conditionalPanel(
          condition="input.sourceType=='upload'",
          radioButtons(
            "sourceFormat",
            "Select a file format:",
            choices = c(
              "Comma-separated (.csv)" = "csv",
              "Excel (.xls/.xlsx)" = "excel"
            )
          ),
          fileInput(inputId = "userData", label = "Select a file:", multiple = F)
        ),
        conditionalPanel(
          condition="input.sourceType=='sample'",
          selectInput(
            "sampleData",
            "Select sample data:",
            choices = c(
              "",
              "Edgar Anderson's Iris Data" = "iris",
              "Moter Trend Car Road Tests" = "mtcars",
              "Violent Crime Rates by US State" = "USArrests"
            ),
            selected = ""
          )
        ),
        hr(),
        actionButton("resetBtn", "Restart")
      ),
      
      # Data sidebar
      conditionalPanel(
        condition="input.tabselected==2",
        disabled(
          selectInput(
            "viewType",
            "Select view:",
            choices = c(
              "Table" = "tbl",
              "Summary" = "sum"
            ),
            selected = "tbl"          
          )
        ),
        h4("View types"),
        tags$b("Table:"),
        "View your data using an interactive table.",
        br(),
        tags$b("Summary:"),
        "View a statistical summary of each variable."
      ),
      
      # Plot sidebar
      conditionalPanel(
        condition="input.tabselected==3",
        disabled(
          selectInput(
            "plotType",
            "Select plot type:",
            c("None" = "none",
              "Bar chart" = "bar",
              "Histogram" = "hist",
              "Line plot" = "line",
              "Scatterplot" ="point"
            )
          )
        ),
        conditionalPanel(
          h4("Plot types"),
          condition = "input.plotType == 'none'",
          tags$b("Bar Chart:"),
          "categorical x and numerical y",
          br(),
          tags$b("Histogram:"),
          "the distribution of numerical x",
          br(),
          tags$b("Line chart:"),
          "numerical x and y, where x represents time",
          br(),
          tags$b("Scatterplot:"),
          "numerical x and y"
        ),

        # bar chart
        conditionalPanel(
          condition = "input.plotType == 'bar'",
          helpText("Must select X and Y inputs to generate a plot"),
          uiOutput("selectBarX"),
          uiOutput("selectBarY"),
          uiOutput("selectBarColor")
        ),
        
        # histogram
        conditionalPanel(
          condition = "input.plotType == 'hist'",
          helpText("Must select X input to generate a plot"),
          uiOutput("selectHistX"),
          sliderInput(
            "bins",
            "Number of bins:",
            min = 1,
            max = 50,
            value = 30
          )
        ),
        
        # line chart
        conditionalPanel(
          condition = "input.plotType == 'line'",
          helpText("Must select X and Y inputs to generate a plot"),
          uiOutput("selectLineX"),
          uiOutput("selectLineY"),
          uiOutput("selectLineColor"),
          sliderInput(
            "linewidth",
            "Line width:",
            min = 0.5,
            max = 2,
            step = 0.5,
            value = 0.5
          )
        ),
        
        # scattorplot
        conditionalPanel(
          condition = "input.plotType == 'point'",
          helpText("Must select X and Y inputs to generate a plot"),
          uiOutput("selectPointX"),
          uiOutput("selectPointY"),
          uiOutput("selectPointColor"),
          uiOutput("selectPointSize"),
          sliderInput(
            "alpha",
            "Transparency:",
            min = 0.3,
            max = 1,
            step = 0.1,
            value = 1
          )
        ),
        conditionalPanel(
          condition = "input.plotType != 'none'",
          selectInput(
            "plotTheme",
            "Plot theme",
            c(
              "Default" = "theme_gray",
              "Dark-on-light" = "theme_bw",
              "Classic" = "theme_classic",
              "Minimal" = "theme_minimal"
            )
          ),
          hr(),
          disabled(actionButton("plotBtn", "Generate plot")),
          downloadButton("downloadBtn", "Download plot")
        )
      )
    ),
    
    # MAIN
    #---------------------------------------------------------------------------
    mainPanel(
      tabsetPanel(
        # Home main
        tabPanel(
          span("home", class="tab-name"),
          value = 1,
          h1("Welcome to ICJIA Data Explorer"),
          "This application provides you with a simple interface to exploring your data.",
          hr(),
          h4("Using your own data"),
          "You can use this Data Explorer to explore your own data.",
          "To do so, select \"Upload file\" on the left, select the data foramt, and upload data from your computer.",
          "The Data Explorer suppots comma-seperated values (.csv) and Microsoft Excel spreadsheets (.xls or .xlsx).",
          "If you are using Excel data, make sure that your data is in a \"clean\" format,",
          "that is, the first row (1) must contain column names and the data must start with the first column (A).",
          "Also, the data must be placed in the first sheet of the file.",
          h4("Using sample data"),
          "Alternatively, you can use sample datasets to gain an understanding of how this application works.",
          "To do so, select \"Use sample\" on the left, which will bring out a dropdown menu.",
          "The Data Explorer provides three sample datasets for you to choose from:",
          "\"Edgar Anderson's Iris Data\", \"Moter Trend Car Road Tests\", and \"Violent Crime Rates by US State\".",
          h4("Restart application"),
          "To restart the Data Explorer, click \"Restart\" button on the left.",
          "You can also simply refresh your broswer.",
          h3("Data tab"),
          "Once you select your data to use, you can go to \"Data\" tab to take a look at it.",
          "There are two \"view\" options for you to choose from: \"Table\" and \"Summary\".",
          tags$ul(
            tags$li(
              tags$b("Table:"),
              "View your data using an interactive table."
            ),
            tags$li(
              tags$b("Summary:"),
              "View a statistical summary of each variable (column)."
            )
          ),
          h3("Plot tab"),
          "Once you select your data to use, you can go to \"Plot\" tab to visualize it.",
          "Here you can generate 4 types of plot using the selected data.",
          "You can also download your plot as an image (.png) file.",
          tags$ul(
            tags$li(
              tags$b("Bar chart:"),
              "Best for plotting one categorical variable, x, and one numerical variable, y."
            ),
            tags$li(
              tags$b("Histogram:"),
              "Best for plotting the distribution of a numerical variable, x."
            ),
            tags$li(
              tags$b("Line chart:"),
              "Best for plotting two numerical variables, x and y, where x represents time."
            ),
            tags$li(
              tags$b("Scatterplot:"),
              "Best for plotting two numerical variables, x and y."
            )
          )
        ),
        
        # Data main
        tabPanel(
          span("data", class="tab-name"),
          value = 2,
          br(),
          conditionalPanel(
            condition = "input.viewType == 'tbl'",
            withSpinner(DTOutput("TABLE"), type = 4)
          ),
          conditionalPanel(
            condition = "input.viewType == 'sum'",
            withSpinner(verbatimTextOutput("SUMMARY"), type = 4)
          )
        ),
        
        # Plot main
        tabPanel(
          span("plot", class="tab-name"),
          value = 3,
          br(),
          conditionalPanel(
            condition = "input.plotType != 'none'",
            withSpinner(plotOutput("PLOT"), type = 4) 
          )
        ),
        id = "tabselected"
      )
    )
  ),
  tags$div(
    class="footer",
    fluidRow(
      column(12, style = "font-size: 10px; margin-top:10px; text-align:center;",
             a(icon("facebook-square", "fa-3x"),
               href = "http://www.facebook.com/ICJIA",
               id = "social"),
             "    ",
             a(icon("twitter-square", "fa-3x"),
               href = "http://www.twitter.com/ICJIA_Illinois",
               id = "social"),
             "    ",
             a(icon("youtube-square", "fa-3x"),
               href = "https://www.youtube.com/channel/UCtZMzk8D3P4OixYTwsfPeKA",
               id = "social")
      ),
      column(12, style = "font-size: 12px; margin-top:10px; text-align:center;",
             p("(312) 793-8550 - Fax: (312) 793-8422-",
               a("cja.irc@illinois.gov",
                 href = "mailto:cja.irc@illinois.gov",
                 id = "social")
             )
      ),
      column(12, style = "font-size: 13px; text-align:center;",
             p(HTML("&copy;"), "2018",
               a("Illinois Criminal Justice Information Authority",
                 href = "http://www.icjia.state.il.us/",
                 id = "social")
             )
      )
    )
  )
))
