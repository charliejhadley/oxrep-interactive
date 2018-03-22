library("tidyverse")
library("shiny")
library("DT")
library("pool")
library("datamodelr")

oxrep_db <- dbPool(
  drv = RMariaDB::MariaDB(),
  dbname = "oxrep",
  port = 3306,
  host = "163.1.169.203",
  user = "ouit0409"
)

oxrep_tables <- dbGetQuery(oxrep_db, "SHOW TABLES")[[1]]
oxrep_table_variables <- paste0(oxrep_tables, "_tbl")

map(oxrep_tables, function(x) {
  assign(paste0(x, "_tbl"),
         oxrep_db %>%
           tbl(x) %>%
           collect(),
         envir = .GlobalEnv)
})

## Where are the ID columns?

map(oxrep_table_variables, function(x) {
  get(x) %>%
    select(contains("ID")) %>%
    colnames() %>%
    list(tbl = x, ids = .)
})


oxrep_dmfr <-
  do.call("dm_from_data_frames",
          map(oxrep_table_variables, function(x) {
            as.name(x)
          }))

oxrep_graph <-
  dm_create_graph(oxrep_dmfr,
                  rankdir = "BT",
                  col_attr = c("column", "type"))
dm_render_graph(oxrep_graph)

oxrep_dmfr <- dm_add_references(
  oxrep_dmfr,
  MineFeatures_tbl$mineID == Mines_tbl$mineID,
  Mines_tbl$mineID == MineFinds_tbl$mineID,
  Shipwrecks_tbl$shipwreckID == ShipwreckFinds_tbl$shipwreckID,
  WaterTechDocs_tbl$waterTechDocID == WaterTechWords_tbl$documentID,
  PressLoc_tbl$ID == PressStructures_tbl$pressID,
  References_tbl$minesRef == Mines_tbl$mineID,
  References_tbl$publicationRef == Publications_tbl$pubID,
  References_tbl$mineStructuresRef == MineFeatures_tbl$mineFeatureID,
  References_tbl$pressesSiteRes == PressLoc_tbl$ID,
  References_tbl$pressesStructuresRef == PressStructures_tbl$productionSiteRef,
  References_tbl$wreckRef == Shipwrecks_tbl$shipwreckID,
  References_tbl$quarryRef == Quarries_tbl$quarryID
)

oxrep_graph <-
  dm_create_graph(oxrep_dmfr,
                  rankdir = "BT",
                  col_attr = c("column", "type"))
dm_render_graph(oxrep_graph, width = "1500px",
                height = "1500px")
