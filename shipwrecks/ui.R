library("DT")
library("shinyBS")
library("leaflet")
library("shinyjs")

appCSS <- "
#loading-content {
position: absolute;
background: #FFFFFF;
opacity: 0.9;
z-index: 100;
left: 0;
right: 0;
height: 100%;
text-align: center;
color: #000000;
}
"

shinyUI(
  fluidPage(
    theme = "animate.min.css",
    useShinyjs(),
    includeScript('www/fontawesome.js'),
    inlineCSS(appCSS),
    tags$head(tags$style(HTML(
      '.modal-lg {width: 95%;}'
    ))),
    # uiOutput("table_selection_UI"),
    uiOutput("timeperiod_main_DT_UI"),
    uiOutput("materials_in_shipwreck_UI"),
    uiOutput("main_DT_summary_text"),
    div(id = "loading-main-table",
        fluidPage(
          h2(class = "animated infinite pulse", "Loading shipwrecks database...")
          # HTML("<img src=images/cruk-logo.png width='50%'></img>")
        )),
    downloadButton("downloadData", "Download Table"),
    p(),
    fluidRow(
      column(width = 1),
      column(leafletOutput("overview_map"), width = 10),
      column(width = 1)
    ),
    p(),
    fluidPage(
      # div(DT::dataTableOutput("main_DT", width = "100%"), style = "font-size: 90%"),
      DT::dataTableOutput("main_DT", width = "100%"),
      # bsModal(
      #   "selected_row_modal",
      #   "Shipwreck Info",
      #   trigger = "main_DT_cell_clicked",
      #   size = "large",
      #   uiOutput("modal_body")
      # )
      uiOutput("the_modal_call")
    ),
    p(),
    p()
  )
)