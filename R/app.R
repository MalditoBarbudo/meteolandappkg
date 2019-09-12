#' function to launch the catdrought app
#'
#' @importFrom magrittr %>%
#'
#' @export
meteoland_app <- function(

) {

  ### Language input ###########################################################
  shiny::addResourcePath(
    'images', system.file('resources', 'images', package = 'meteolandappkg')
  )
  lang_choices <- c('cat', 'spa', 'eng')
  lang_flags <- c(
    glue::glue("<img class='flag-image' src='images/cat.png' width=20px><div class='flag-lang'>%s</div></img>"),
    glue::glue("<img class='flag-image' src='images/spa.png' width=20px><div class='flag-lang'>%s</div></img>"),
    glue::glue("<img class='flag-image' src='images/eng.png' width=20px><div class='flag-lang'>%s</div></img>")
  )

  ## UI ####
  ui <- shiny::tagList(
    # shinyjs
    shinyjs::useShinyjs(),

    # css
    shiny::tags$head(
      # custom css
      shiny::includeCSS(
        system.file('resources', 'meteoland.css', package = 'meteolandappkg')
      ),
      # corporative image css
      shiny::includeCSS(
        system.file('resources', 'corp_image.css', package = 'meteolandappkg')
      )
    ),

    navbarPageWithInputs(
      # opts
      title = 'Meteoland App',
      id = 'nav', collapsible = TRUE,

      # navbar with inputs (helpers.R) accepts an input argument, we use it for the lang
      # selector
      inputs = shinyWidgets::pickerInput(
        'lang', NULL,
        choices = lang_choices,
        selected = 'cat',
        width = '100px',
        choicesOpt = list(
          content = c(
            sprintf(lang_flags[1], lang_choices[1]),
            sprintf(lang_flags[2], lang_choices[2]),
            sprintf(lang_flags[3], lang_choices[3])
          )
        )
      ),

      # navbarPage contents
      shiny::tabPanel(
        title = shiny::uiOutput("actual_tab"),
        # we need to create the ui in the server to catch the language input
        # and redraw all the inputs and texts in the selected lang
        shiny::uiOutput('current_ui')
      ) # end of current tab
    ) # end of navbar
  ) # end of UI

  ## SERVER ####
  server <- function(input, output, session) {
    ## debug #####
    # output$debug1 <- shiny::renderPrint({
    #   input$map_daily_marker_click
    # })
    # output$debug2 <- shiny::renderPrint({
    #   map_reactives$map_click
    # })
    # output$debug3 <- shiny::renderPrint({
    #   map_reactives$map_shape_click
    # })

    ## lang reactive ####
    lang <- shiny::reactive({
      input$lang
    })

    output$actual_tab <- shiny::renderText({
      translate_app('actual_tab_title', lang())
    })

    ## proper UI ####
    output$current_ui <- shiny::renderUI({

      lang_declared <- lang()

      shiny::tagList(
        # a little space
        shiny::br(),

        shiny::sidebarLayout(
          shiny::sidebarPanel(
            # sidebar width
            width = 3,

            # panel for fixed inputs (mode and point/grid)
            shiny::wellPanel(
              # Mode selector
              shiny::radioButtons(
                inputId = 'mode_sel',
                label = 'Please select the desired mode:',
                choices = c('Historical', 'Current', 'Projection'),
                inline = TRUE, selected = 'Historical'
              ),

              # point/grid selector
              shiny::radioButtons(
                inputId = 'point_grid_sel',
                label = 'Points (up to 10) or Grid?',
                choices = c('Points', 'Grid'),
                inline = TRUE, selected = 'Points'
              )
            ),

            # Dinamic ui to show inputs and buttons depending on the mode selected
            shiny:: wellPanel(
              shiny::uiOutput(
                outputId = 'dinamic_inputs'
              )
            ),

            # latitude and longitude selector. To be able to show both in the same
            # line we must to rely in some html/css magic ;)
            shiny:: div(style = "display: inline-block;vertical-align:top; width: 145px;",
                        shiny::numericInput(
                  'latitude',
                  label = 'Latitude',
                  value = NA)),

            shiny::div(style = "display: inline-block;vertical-align:top; width: 145px;",
                       shiny::numericInput(
                  'longitude',
                  label = 'Longitude',
                  value = NA)),

            # conditional panel to show in case of grid. In this case we need
            # two different sets of coordinates, the upper left and the bottom
            # right coordinates of the boundary box desired by the user
            shiny::conditionalPanel(
              condition = "input.point_grid_sel == 'Grid'",

              shiny::div(style = "display: inline-block;vertical-align:top; width: 145px;",
                         shiny::numericInput(
                    'latitude_bottom',
                    label = 'Latitude bottom right',
                    value = NA)),

              shiny::div(style = "display: inline-block;vertical-align:top; width: 145px;",
                         shiny::numericInput(
                    'longitude_bottom',
                    label = 'Longitude bottom right',
                    value = NA)),

              shiny::p("Grid mode selected."),
              shiny::p("Please provide the upper left coordinates and the bottom right coordinates of the desired grid.")
            ),

            shiny::p('Coordinates input must be in latitude/logitude Mercator ',
              'projection, in decimal format'),

            # selected coordinates output, we need a fluid row to put inline
            # the selected coordinates and the clear button. All of this is in
            # a conditional panel to show only if points are selected

            shiny::conditionalPanel(
              condition = "input.point_grid_sel == 'Points'",

              # Append coordinates button
              shiny::actionButton(
                inputId = 'append_coord_button',
                label = 'Append coords',
                icon = shiny::icon('bullseye')
              ),

              # a little space and a header
              shiny::br(), shiny::br(),
              shiny::h5('Selected points:'),

              shiny::fluidRow(

                # coord column
                shiny::column(
                  width = 6,
                  shiny::br(),
                  shiny::tableOutput('user_coords')
                ),

                # reset button column
                shiny::column(
                  width = 6,
                  shiny::br(), shiny::br(),
                  shiny::actionButton(
                    inputId = 'reset_coord_button',
                    label = 'Reset coords',
                    icon = shiny::icon('eraser')
                  )
                )
              )

              # debug
              # textOutput('clicked'),
              # textOutput('lat_debug'),
              # textOutput('long_debug')
              # textOutput('dates_debug')
              # ,textOutput('interpolated_df_debug')
            ),

            # a little space
            shiny::br(), shiny::br(),

            # Action button to activate the process
            shiny::actionButton(
              inputId = 'process_button',
              label = 'Go!',
              icon = shiny::icon('play')
            )
          ),

          shiny::mainPanel(
            # main panel width
            width = 9,

            # map output
            leaflet::leafletOutput('map', height = 600)
          )
        )
      )
    }) ## end of proper UI
  } # end of server function

  # Run the application
  meteolandapp <- shiny::shinyApp(
    ui = ui, server = server#,
    # onStart = function() {
    #
    #   ## on stop routine to cloose the db pool
    #   shiny::onStop(function() {
    #     pool::poolClose(catdrought_db)
    #   })
    # }
  )

  # shiny::runApp(nfi_app)
  return(meteolandapp)
}