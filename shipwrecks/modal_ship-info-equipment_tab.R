info_transposed_table <- function(data, ...) {
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
  
  data %>%
    replace_na(the_na_labels) %>%
    setNames(nice_col_headings) %>%
    gather('Shipwreck Property', 'Observation') %>%
    mutate(`Shipwreck Property` = paste0(`Shipwreck Property`, ":"))
  
}

# output$modal_shipinfo_table <- renderTable({
#   modal_row_data <- modal_row_data()
#   modal_row_data %>%
#     select(hullRemains, shipEquipment, estimatedTonnage, estimatedLength) %>%
#     info_transposed_table()
# },
# colnames = FALSE,
# width = '100%', align = 'l')

output$modal_shipinfo_DT <- renderDataTable({
  modal_row_data <- modal_row_data()
  modal_row_data %>%
    select(hullRemains, shipEquipment, estimatedTonnage, estimatedLength) %>%
    info_transposed_table() %>%
    datatable(
      rownames = FALSE,
      colnames = "",
      options = list(dom = 't',
                     autoWidth = TRUE,
                     columnDefs = list(list(width = "200px", targets = list(0))),
                     style = "bootstrap")
    ) %>%
    formatStyle(1, fontWeight = "bold")
}
)

output$modal_shipinfo_tab <- renderUI({

  fluidPage(
    # tableOutput("modal_shipinfo_table"),
    dataTableOutput("modal_shipinfo_DT")
  )
})