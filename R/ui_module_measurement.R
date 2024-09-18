#' @title Measurement UI
#'
#' @param id [character] (**required**) vector to be used for the namespace.
#'
#' @family modules
#'
#' @keywords internal
#' @md
#' @export
ui_module_measurement <- function(id) {
  # Create a namespace function using the provided id
  ns <- shiny::NS(id)

    ## RENDER PANEL GUI
    shiny::tabPanel(
      title = "Measurement",
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::tabsetPanel(
            # PANEL - SETTINGS -------------------------------------------------
            shiny::tabPanel("Measurement settings",
               shiny::div(style = "vertical-align:bottom;",
                shiny::br(),
                shiny::h5("Connection settings"),
                shiny::fluidRow(
                 shiny::column(width = 10,
                   shiny::selectInput(
                     inputId = ns("port"),
                     label = shiny::HTML("Select Port"),
                     choices = list_ports(),
                     selected =
                       if (any(grepl("USB", list_ports())))
                         list_ports()[grepl("USB", list_ports())]
                       else
                         NULL
                      )
                   ),
                  shiny::column(
                    width = 2,
                    shiny::actionButton(
                     inputId = ns("port_refresh"),
                     label =  "",
                     icon = shiny::icon("refresh"),
                     style = 'margin-top:25px')))),
               shiny::fluidRow(
                 shiny::column(
                  align = "center",
                  width = 12,
                  shiny::br(),
                  shiny::actionButton(
                   inputId = ns("init_start"),
                   label = shiny::HTML("Initialse")),
                  shiny::actionButton(
                   inputId = ns("init_stop"),
                   label = shiny::HTML("Disconnect")))),
                shiny::hr(),
              # PANEL - PARAMETERS----------------------------------------------
              shiny::h5("Measurement parameters"),
              shiny::fluidRow(
                shiny::br(),
                shiny::column(
                  width = 6 ,
                  align = "center",
                 shiny::numericInput(
                   inputId =  ns("PMT_P"),
                   label = paste0("Channel time [x 10 ms]"),
                   value = 10,
                   min = 10,
                   max = 99,
                   step = 1,
                   width = "180px"),
                shiny::textOutput(outputId = ns("meas_interval")),
                shiny::br()),
              shiny::column(
               width = 6 ,
               align = "center",
               shiny::numericInput(
                inputId =  ns("PMT_nCH"),
                label = paste0("Number of channels"),
                value = 0,
                min = 0,
                max = 9999,
                step = 1,
                width = "180px"),
              shiny::textOutput(outputId = ns("meas_duration"))),
               style = 'border: 1px dashed grey;'),
             # PANEL - START/STOP
             shiny::fluidRow(
              shiny::br(),
              shiny::column(
                width = 12,
                align = "center",
                shiny::radioButtons(
                  inputId = ns("meas_mode"),
                  label = "Measurement mode",
                  choices = c("Measurement", "Simulation"),
                  selected = "Measurement",
                  inline = TRUE)),
              shiny::br(),
              shiny::column(
               width = 12,
               align = "center",
               shiny::actionButton(
                inputId = ns("start"),
                class = "btn-success",
                label = shiny::HTML("Start reading")),
               shiny::actionButton(
                inputId = ns("stop"),
                class = "btn-warning",
                label = shiny::HTML("Stop reading")))),
              shiny::hr(),
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shiny::verbatimTextOutput(outputId = ns("con_status")))),
         fluid = TRUE),
         # PANEL -- PLOT SETTINGS----------------------------------------------
          shiny::tabPanel("Plot settings",
           shiny::br(),
           shiny::br(),
           shiny::fluidRow(
             shiny::column(
               width = 12,
               shiny::textInput(
                 inputId = ns("plot_title"),
                 label = "Plot title",
                 value = ""))),
           shiny::br(),
           shiny::fluidRow(
            shiny::column(
              width = 6,
              align = "center",
                shiny::radioButtons(
                inputId = ns("x_log"),
                label = "x-axis",
                choiceNames = c("log", "lin"),
                choiceValues = c(TRUE, FALSE),
                selected = FALSE,
                inline = TRUE)),
            shiny::column(
              width = 6,
              align = "center",
               shiny::radioButtons(
                 inputId = ns("y_log"),
                 label = "y-axis",
                 selected = FALSE,
                 choiceNames = c("log", "lin"),
                 choiceValues = c(TRUE, FALSE),
                 inline = TRUE)),
          style = 'border: 1px dashed grey;'),
          shiny::br(),
          shiny::fluidRow(
            shiny::column(
              width = 12,
              align = "center",
              shiny::radioButtons(
                inputId = ns("plot_marker_style"),
                label = "Marker style",
                choiceNames = c("line", "points", "both"),
                choiceValues = c("lines", "markers", "lines+markers"),
                selected = c("lines+markers"),
                inline = TRUE))),
          shiny::br(),
          shiny::fluidRow(
            shiny::column(
              width = 12,
              align = "center",
              shiny::actionButton(
                inputId = ns("plot_update"),
                label = "Update plot",
                icon = shiny::icon("refresh")))),
         fluid = TRUE),
         # PANEL -- DATA EXPORT ----------------------------------------------
         shiny::tabPanel(
           title = "Data export",
           shiny::br(),
           shiny::br(),
           shiny::fluidRow(
            shiny::column(
              width = 12,
              align = "left",
               shiny::checkboxInput(
                 inputId = ns("rm_NA"),
                 label = "Remove NA",
                 value = FALSE))),
            shiny::fluidRow(
             shiny::column(
              width = 12,
              align = "center",
              shiny::downloadButton(
               outputId = ns("download_data"),
               label = "Download CSV"), shiny::HTML('&nbsp;'))))
         )#tab end
        ),#end sidebarPanel
      # MAIN PANEL -------------------------------------------------------------
      shiny::mainPanel(
             shiny::fluidRow(
               shiny::column(
                 width = 10,
                 plotly::plotlyOutput(ns("linePlot")))),
             shiny::fluidRow(
               shiny::column(
                 width = 12,
                 align = "center",
                 shiny::h4(
                   shiny::textOutput(
                     outputId = ns("overexposure")),
                   style = 'color:red'))),
            icon = shiny::icon("cog", lib = "glyphicon")))
        ) #end sidePanel

}
