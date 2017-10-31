# info_htmlTable <- function(data, ...) {
#   nice_col_headings <-
#     plyr::mapvalues(
#       colnames(data),
#       from = shipwrecks_table_labels$data.name,
#       to = shipwrecks_table_labels$display.name,
#       warn_missing = FALSE
#     ) %>%
#     tools::toTitleCase()
#   
#   data[is.na(data)] <- "Unknown"
#   
#   htmlTable(
#     data,
#     rnames = FALSE,
#     css.table = "width:100%",
#     col.columns = c("none", "#F7F7F7"),
#     header = nice_col_headings,
#     # css.cell = "padding-left: .5em; padding-right: .2em;
#     # border: 1px solid black;",
#     css.cell = "padding-left: .5em; padding-right: .2em;",
#     ...
#   )
# }

info_renderTable <- function(data, ...) {
  nice_col_headings <-
    plyr::mapvalues(
      colnames(data),
      from = shipwrecks_table_labels$data.name,
      to = shipwrecks_table_labels$display.name,
      warn_missing = FALSE
    ) %>%
    tools::toTitleCase()
  
  the_na_labels <- shipwrecks_table_labels %>%
    filter(data.name %in% colnames(data)) %>%
    select(label.for.null.fields) %>%
    .[[1]] %>%
    as.list()
  
  the_na_labels <- setNames(the_na_labels, colnames(data))
  
  data <- data %>%
    replace_na(the_na_labels)
  
  colnames(data) <- nice_col_headings
  
  data
}


# 
# output$modal_summary_tab <- renderUI({
#   modal_row_data <- modal_row_data()
#   
#   summary_table <- modal_row_data %>%
#     select(wreckName, longitude, latitude)
#   
#   location_table <- modal_row_data %>%
#     select(wreckSite, seaArea, country, region, parkerNumber)
#   
#   date_depth_table <- modal_row_data %>%
#     select(notBeforeWreckDate,
#            notAfterWreckDate,
#            period,
#            minimumDepth,
#            maximumDepth)
#   
#   details_table <- modal_row_data %>%
#     select(notBeforeWreckDate, notAfterWreckDate, latitude)
#   
#   modal_row_data %>%
#     select(minimumDepth, maximumDepth) %>%
#     print()
#   
#   fluidPage(
#     fluidRow(
#       column(
#         br(),
#         modal_row_data %>%
#           select(wreckName, longitude, latitude, parkerNumber) %>%
#           info_htmlTable(),
#         br(),
#         p("Possible wreck time period:"),
#         plotOutput("summary_timeline", height = "50px"),
#         modal_row_data %>%
#           select(notBeforeWreckDate, notAfterWreckDate, period) %>%
#           info_htmlTable(),
#         br(),
#         p("Wreck location info"),
#         modal_row_data %>%
#           select(wreckSite, seaArea, country, region) %>%
#           info_htmlTable(),
#         br(),
#         p("Depth info"),
#         modal_row_data %>%
#           select(minimumDepth, maximumDepth) %>%
#           info_htmlTable(),
#         width = 6
#       ),
#       column(leafletOutput("summary_leaflet_map"),
#              width = 6)
#     ),
#     p(),
#     modal_row_data %>%
#       select(references, notes) %>%
#       info_htmlTable(., align = paste(rep('l',ncol(.)),collapse=''))
#   )
# })


output$renderTable_summary <- renderTable({
  modal_row_data <- modal_row_data()
  
  modal_row_data %>%
    select(wreckName, longitude, latitude) %>%
    info_renderTable()
},
width = '100%', align = 'l',
na = 'missing')

output$renderTable_timeperiod <- renderTable({
  modal_row_data <- modal_row_data()
  
  modal_row_data %>%
    select(notBeforeWreckDate, notAfterWreckDate, period) %>%
    info_renderTable()
},
width = '100%', align = 'l',
na = 'missing')

output$renderTable_locInfo <- renderTable({
  modal_row_data <- modal_row_data()
  
  modal_row_data %>%
    select(wreckSite, seaArea, country, region) %>%
    info_renderTable()
},
width = '100%', align = 'l',
na = 'missing')

output$renderTable_depthInfo <- renderTable({
  modal_row_data <- modal_row_data()
  
  modal_row_data %>%
    select(minimumDepth, maximumDepth) %>%
    info_renderTable()
},
width = '100%', align = 'l',
na = 'missing')

output$renderTable_references <- renderTable({
  modal_row_data <- modal_row_data()
  
  modal_row_data %>%
    select(references, notes) %>%
    info_renderTable()
},
width = '100%', align = 'l',
na = 'missing')

output$datatable_references <- DT::renderDataTable({
  modal_row_data <- modal_row_data()
  
  modal_row_data %>%
    select(references, notes) %>%
    info_renderTable()
},
options = list(
  autoWidth = TRUE,
  columnDefs = list(list(width = '50%', targets = "1")),
  dom = "t"
),
rownames = FALSE)

output$modal_summary_tab <- renderUI({
  
  fluidPage(
    
    fluidRow(
      column(
        br(),
        tableOutput("renderTable_summary"),
        p("Possible wreck time period:"),
        plotOutput("summary_timeline", height = "50px"),
        tableOutput("renderTable_timeperiod"),
        p("Wreck location info"),
        tableOutput("renderTable_locInfo"),
        p("Depth info"),
        tableOutput("renderTable_depthInfo"),
        width = 6
      ),
      column(leafletOutput("summary_leaflet_map"),
             width = 6)
    ),
    p(),
    DT::dataTableOutput("datatable_references")
    
  )
  
})


output$summary_timeline <- renderPlot({
  modal_row_data <- modal_row_data()
  
  gg_timeline_plot(
    start = modal_row_data$notBeforeWreckDate,
    end = modal_row_data$notAfterWreckDate,
    minyear = modal_row_data$notBeforeWreckDate - 1000,
    maxyear = modal_row_data$notAfterWreckDate + 1000,
    breaks = 300
  )
  
})



output$summary_leaflet_map <- renderLeaflet({
  modal_row_data <- modal_row_data()
  
  centre_latitude <- modal_row_data %>%
    select(latitude) %>%
    .[[1]]
  
  centre_longitude <- modal_row_data %>%
    select(longitude) %>%
    .[[1]]
  
  modal_row_data %>%
    collect() %>%
    leaflet() %>%
    addTiles() %>%
    addCircleMarkers(
      label = ~ wreckSite,
      popup = ~ paste("Wreck Site:", wreckSite,
                      "<br/>",
                      "Wreck Name:", wreckName)
    ) %>%
    setView(zoom = 4,
            lat = centre_latitude,
            lng = centre_longitude)
  
})