
### =========== Get labels ======================
### =============================================

shipwrecks_table_labels <- read_csv("data/shipwrecks-table-labels.csv")
colnames(shipwrecks_table_labels) <- tolower(make.names(colnames(shipwrecks_table_labels)))

shipwrecks_table_labels <- shipwrecks_table_labels %>%
  mutate(display.name = ifelse(is.na(display.name), data.name , display.name)) %>%
  mutate(
    display.name = gsub('([[:upper:]])', ' \\1', display.name),
    display.name = gsub(
      "(^|[[:space:]])([[:alpha:]])",
      "\\1\\U\\2",
      display.name,
      perl = TRUE
    ))

### =========== Get secure database details =====
### =============================================

secure_database_details <- read_csv("data/secure-database-details.csv")


print("CHECK THE USERNAME!!!!! Shiny server needs to be shiney")

## ======= LOCAL
oxrep_db <- dbPool(
  drv = RMySQL::MySQL(),
  dbname = "oxrep",
  port = 3306,
  host = secure_database_details %>%
    filter(property == "host") %>%
    select(value) %>%
    .[[1]],
  user = secure_database_details %>%
    filter(property == "local.username") %>%
    select(value) %>%
    .[[1]]
)

# ## ======= REMOTE
# oxrep_db <- dbPool(
#   drv = RMySQL::MySQL(),
#   dbname = "oxrep",
#   port = 3306,
#   host = secure_database_details %>%
#     filter(property == "host") %>%
#     select(value) %>%
#     .[[1]],
#   user = secure_database_details %>%
#     filter(property == "server.username") %>%
#     select(value) %>%
#     .[[1]]
# )


### =========== Connect to database =============
### =============================================


time_start_app <- Sys.time()
oxrep_db <- dbPool(
  drv = RMySQL::MySQL(),
  dbname = "oxrep",
  port = 3306,
  host = "163.1.169.203",
  user = "ouit0409"
  # user = "shiney"
)

### =========== Get main table =============
### =============================================

dt_main_data <- {
  shipwrecks <- oxrep_db %>%
    tbl("Shipwrecks") %>%
    collect()
}

total_observations_dt_main_data <- nrow(dt_main_data)


dt_main_data <- dt_main_data %>%
  mutate(maximumDepth = as.numeric(maximumDepth),
         minimumDepth = as.numeric(minimumDepth))


display_main_label_df <- shipwrecks_table_labels %>%
  filter(!is.na(main.table)) %>%
  select(data.name, display.name) %>%
  mutate(display.name = trimws(display.name),
         display.name = gsub("\\s+", " ", display.name))

# dt_main_data <- dt_main_data %>%
#   mutate_if(is.character, funs(stringr::str_conv(., "UTF-8")))


### =========== Artifacts

shipwreck_finds <- oxrep_db %>%
  tbl("ShipwreckFinds") %>%
  collect()



### =========== Display version of tables

## Display version omits IDs
display_main_data <- dt_main_data %>%
  select(-contains("ID")) %>%
  # select(display_main_label_df$data.name) %>%
  collect()

### =========== Replace ticks with glyphicons
tick_columns <- shipwrecks_table_labels %>%
  filter(!is.na(tick.label)) %>%
  select(data.name) %>%
  .[[1]]

display_main_data <- display_main_data %>%
  mutate_at(vars(one_of(tick_columns)), funs(plyr::mapvalues(
    .,
    from = c("0", "1", ""),
    to = c(FALSE, TRUE, FALSE)
  ))) %>%
  mutate_at(vars(one_of(tick_columns)), funs(as.logical))

colnumber_filter_disabled <-
  which(as.character(vapply(
    as.data.frame(head(display_main_data)), typeof, character(1)
  )) == "logical")

### =========== Replace empty strings with NA

display_main_data[display_main_data == ""] <- NA

