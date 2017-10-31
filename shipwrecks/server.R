library("tidyverse")
library("shiny")
library("DT")
library("pool")
library("shinyBS")
library("leaflet")
library("knitr")
library("htmlTable")
library("dbplyr")
library("RMySQL")
library("stringr")
library("shinyjs")
library("sf")
library("readxl")
library("openxlsx")


source("data-processing.R", local = TRUE)
source("gg_timeline_plot.R", local = TRUE)


lapply(dbListConnections(MySQL()), dbDisconnect)

function(input, output, session) {
  
  source("modal_summary-tab.R", local = TRUE)$value
  
  source("modal_ship-info-equipment_tab.R", local = TRUE)$value
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("OxRep-Shipwrecks", ".xlsx", sep = "")
    },
    content = function(file) {
      selected_materials_in_shipwrecks <- input$materials_in_shipwrecks
      
      if (!is.null(selected_materials_in_shipwrecks)) {
        display_main_data <- display_main_data %>%
          filter_at(.,
                    vars(selected_materials_in_shipwrecks),
                    all_vars(. == TRUE))
      }
      
      display_main_data <- display_main_data %>%
        filter(
          notBeforeWreckDate >= input$timeperiod_main_DT[1] &
            notAfterWreckDate <= input$timeperiod_main_DT[2]
        )
      
      nice_col_headings <-
        plyr::mapvalues(
          colnames(display_main_data),
          from = shipwrecks_table_labels$data.name,
          to = shipwrecks_table_labels$display.name,
          warn_missing = FALSE
        ) %>%
        tools::toTitleCase() %>%
        trimws()
      
      write.xlsx(display_main_data %>%
                   setNames(nice_col_headings), file)
    }
  )
  
  output$main_DT_summary_text <- renderUI({
    selected_materials_in_shipwrecks <- input$materials_in_shipwrecks
    
    if (!is.null(selected_materials_in_shipwrecks)) {
      display_main_data <- display_main_data %>%
        filter_at(.,
                  vars(selected_materials_in_shipwrecks),
                  all_vars(. == TRUE))
    }
    
    if (is.null(input$timeperiod_main_DT)) {
      return()
    }
    
    display_main_data <- display_main_data %>%
      filter(
        notBeforeWreckDate >= input$timeperiod_main_DT[1] &
          notAfterWreckDate <= input$timeperiod_main_DT[2]
      )
    
    paste0(
      "With your current filters, there are ",
      nrow(display_main_data),
      " observations from a total of ",
      total_observations_dt_main_data,
      " shipwrecks in the database."
    )
    
  })
  
  output$timeperiod_main_DT_UI <- renderUI({
    sliderInput(
      "timeperiod_main_DT",
      "Show shipwrecks that occured within this time period:",
      min = min(display_main_data$notBeforeWreckDate, na.rm = TRUE),
      max = max(display_main_data$notAfterWreckDate, na.rm = TRUE),
      value = c(
        min(display_main_data$notBeforeWreckDate, na.rm = TRUE),
        max(display_main_data$notBeforeWreckDate, na.rm = TRUE)
      ),
      width = "100%"
    )
  })
  
  output$overview_map <- renderLeaflet({
    map_point_labeller <-
      function(wreckSite = NA,
               wreckName = NA,
               country = NA,
               notBeforeWreckDate = NA,
               notAfterWreckDate = NA) {
        paste0(
          # "<p>", Name, "</p>",
          "<p>Wreck Site: ",
          wreckSite,
          "</p>",
          "<p>Wreck Name: ",
          wreckName,
          "</p>",
          "<p>Country: ",
          country,
          "</p>",
          "<p>Date Range: ",
          "from ",
          notBeforeWreckDate,
          " to ",
          notAfterWreckDate,
          "</p>"
        )
      }
    
    if (is.null(input$timeperiod_main_DT)) {
      return()
    }
    
    selected_materials_in_shipwrecks <-
      input$materials_in_shipwrecks
    
    
    if (!is.null(selected_materials_in_shipwrecks)) {
      display_main_data <- display_main_data %>%
        filter_at(.,
                  vars(selected_materials_in_shipwrecks),
                  all_vars(. == TRUE))
    }
    
    display_main_data <- display_main_data %>%
      filter(
        notBeforeWreckDate >= input$timeperiod_main_DT[1] &
          notAfterWreckDate <= input$timeperiod_main_DT[2]
      )
    
    my_fitBounds <- function(map, bbox) {
      fitBounds(map, bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax)
    }
    
    locs_sf <-
      st_as_sf(display_main_data, coords = c("longitude", "latitude"))
    
    locs_sf %>%
      leaflet() %>%
      addProviderTiles(providers$Esri.WorldShadedRelief) %>%
      addMarkers(
        popup = ~ map_point_labeller(
          wreckSite,
          wreckName,
          country,
          notBeforeWreckDate,
          notAfterWreckDate
        ),
        icon = makeIcon(
          "shipwreck.png",
          iconWidth = 18,
          iconHeight = 18
        )
      ) %>%
      my_fitBounds(locs_sf %>%
                     st_bbox() %>%
                     as.list())
    
    
  })
  
  output$materials_in_shipwreck_UI <- renderUI({
    selectInput(
      "materials_in_shipwrecks",
      "Only show shipwrecks where all of the following materials were found",
      choices = c(
        "Amphorae" = "foundAmphorae",
        "Marble" = "foundMarble",
        "Columns" = "foundColumns",
        "Sarcophagi" = "foundSarcophagi",
        "Blocks" = "foundBlocks"
      ),
      multiple = TRUE,
      width = "100%"
    )
  })
  
  output$main_DT <- DT::renderDataTable({
    if (is.null(input$timeperiod_main_DT)) {
      return()
    }
    
    shinyjs::show(id = "loading-main-table",
                  anim = TRUE,
                  animType = "fade")
    
    selected_materials_in_shipwrecks <-
      input$materials_in_shipwrecks
    
    if (!is.null(selected_materials_in_shipwrecks)) {
      display_main_data <- display_main_data %>%
        filter_at(.,
                  vars(selected_materials_in_shipwrecks),
                  all_vars(. == TRUE))
    }
    
    shinyjs::hide(id = "loading-main-table",
                  anim = TRUE,
                  animType = "fade")
    
    display_main_data <- display_main_data %>%
      filter(
        notBeforeWreckDate >= input$timeperiod_main_DT[1] &
          notAfterWreckDate <= input$timeperiod_main_DT[2]
      )
    
    display_tbl <- display_main_data %>%
      select(display_main_label_df$data.name) %>%
      mutate(seaArea = gsub(" ", "<br>", seaArea))
    
    the_na_labels <- shipwrecks_table_labels %>%
      filter(
        data.name %in% c(
          "wreckSite",
          "wreckName",
          "notBeforeWreckDate",
          "notAfterWreckDate",
          "seaArea",
          "country"
        )
      ) %>%
      select(label.for.null.fields) %>%
      .[[1]] %>%
      as.list()
    
    the_na_labels <- setNames(
      the_na_labels,
      c(
        "wreckSite",
        "wreckName",
        "notBeforeWreckDate",
        "notAfterWreckDate",
        "seaArea",
        "country"
      )
    )
    
    display_tbl <- display_tbl %>%
      replace_na(the_na_labels)
    
    datatable(
      display_tbl,
      rownames = FALSE,
      colnames = display_main_label_df$display.name,
      # extensions = c('Scroller'),
      selection = 'none',
      escape = FALSE,
      options = list(
        columnDefs = list(list(
          width = "80px", targets = list(0, 1, 5)
        )),
        # columnDefs = list(list(width = "80px", targets = "_all")),
        server = FALSE,
        autoWidth = TRUE,
        scrollX = TRUE,
        # scrollY = 500,
        # scroller = TRUE,
        pageLength = 50,
        dom = 'Bfrtip',
        buttons = c('csv', 'excel'),
        rowCallback = htmlwidgets::JS(
          "function(row, data, rowi) {
          data.forEach(function(d,i) {
          if(typeof(d) === 'boolean') {
          $($('td', row)[i]).html(
          [
          '<center><i class=\\'',
          d ? 'fa fa-circle' : 'fa fa-circle-o',
          '\\'>',
          '</i></center>'
          ].join('')
          )
          }
          })
  }"
)
        )
        ) %>%
      formatStyle(1:2, color = "#6d9dc8", cursor = "default")
    
},
server = FALSE)
  
  observeEvent(input$main_DT_cell_clicked,
               {
                 info <- input$main_DT_cell_clicked
                 if (is.null(info$value) || info$col == 0) {
                   return()
                 } else {
                   selected_row <- dt_main_data[info$row,]
                   
                   toggleModal(session, "selected_row_modal", toggle = "toggle")
                   
                 }
                 
               })
  
  
  modal_row_data <- eventReactive(input$main_DT_cell_clicked,
                                  {
                                    info <- input$main_DT_cell_clicked
                                    
                                    display_main_data <-
                                      display_main_data %>%
                                      filter(
                                        notBeforeWreckDate >= input$timeperiod_main_DT[1] &
                                          notAfterWreckDate <= input$timeperiod_main_DT[2]
                                      )
                                    
                                    
                                    selected_row <-
                                      display_main_data[info$row,]
                                    
                                    selected_row
                                  })
  
  output$modal_body <- renderUI({
    tabsetPanel(tabPanel("Map & Summary",
                         uiOutput("modal_summary_tab")),
                tabPanel("Ship Info & Equipment",
                         uiOutput("modal_shipinfo_tab")))
    
    
    
    
  })
  
  output$the_modal_call <- renderUI({
    modal_row_data <- modal_row_data()
    
    
    
    bsModal(
      "selected_row_modal",
      modal_row_data$wreckName,
      trigger = "main_DT_cell_clicked",
      size = "large",
      uiOutput("modal_body")
    )
  })
  
  
  }