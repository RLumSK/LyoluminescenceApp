shinyUI(
  navbarPage(
    title = HTML(paste0("Lyoluminescence PMT App")),
    windowTitle = "Lyoluminescence PMT App",
    footer = HTML(
      "<hr>
            <div align = 'center'><small>This software comes WITHOUT ANY WARRANTY.</small>
            </div>"),
    # PANEL - SETTINGS --------------------------------------------------------
    tabPanel("Measurement",
      sidebarLayout(
        sidebarPanel(
          div(style="vertical-align:bottom;",
            fluidRow(
              column(width = 10,
                selectInput(
                    inputId = "port", 
                    label = HTML("Select Port"), 
                    choices = serial::listPorts())), 
                column(width = 2,
                  actionButton(
                    "port_refresh", 
                    label =  "", 
                    icon = icon("refresh"), 
                    style = 'margin-top:25px'
                    ))
              )),
          fluidRow(
            column(
              align = "center",
              width = 12, 
              br(),
              actionButton(
                inputId = "init_start", 
                label = HTML("Initialse")),
              actionButton(
                inputId = "init_stop", 
                label = HTML("Disconnect"))
            )),
          hr(),
         fluidRow(
           column(
             width = 12
             ,
             align = "center",
             numericInput(
               inputId =  "PMT_P",
               label = paste0("Channel time [x 10 ms]"),
               value = 10,
               min = 10,
               max = 99,
               step = 1,
               width = "200px"
             ), 
             textOutput("meas_interval"),
             br()
           )
         ),
         fluidRow(
           column(width = 6, align = "center",
            radioButtons(
            "x_log",
            label = "x-axis",
            choiceNames = c("log", "lin"),
            choiceValues = c("x", ""),
            selected = "", 
            inline = TRUE)
           ),
           column(
             width = 6, align = "center",
             radioButtons(
             "y_log",
             label = "y-axis",
             selected = "", 
             choiceNames = c("log", "lin"),
             choiceValues = c("y", ""),
             inline = TRUE))
         ),
         fluidRow(
           hr(),
           column(
             width = 12,
             align = "center", 
             actionButton(
               inputId = "start", 
               class = "btn-success",
               label = HTML("Start reading")),
             actionButton(
               inputId = "stop", 
               class = "btn-warning",
               label = HTML("Stop reading")))
          ),
         hr(),
         fluidRow(
          column(
            width = 12,
            verbatimTextOutput("con_status")
          )
         ),
         fluidRow(
           column(
             width = 12,
             align = "center",
            h4(textOutput("overexposure"), style = 'color:red')  
         )
        ),
        ),#end sidebarPanel
        mainPanel(
             fluidRow(
              div(
                column(width = 9, align = "right",
                 checkboxInput("rm_NA", "Remove NA", value = FALSE)),
                column(width = 3, align = "left",
                 downloadButton("download_data", "Download CSV"), HTML('&nbsp;')),
                align = "right")),
             fluidRow(
               plotOutput("plot")),
            icon = icon("cog", lib = "glyphicon")))
        ), #end sidePanel
    # PANEL - News -------------------------------------------------------------
    tabPanel("News",
             fluidRow(
               column(10, offset = 1,
                      uiOutput('news')
               )
             ),icon = icon("list-alt", lib = "glyphicon")
    ),# end news
    # PANEL - About ------------------------------------------------------------------------------
    tabPanel("About",
             fluidRow(
               column(10, offset = 1,
                      uiOutput('about')
               )
             ),icon = icon("info-sign", lib = "glyphicon")
    )# end about
  )##navbarPage
)##EOF
