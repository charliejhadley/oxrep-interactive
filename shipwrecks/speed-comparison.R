start_src_mysql <- Sys.time()

oxrep_db <- src_mysql(
  dbname = "oxrep",
  port = 3306,
  from = "foo2",
  host = "163.1.169.203",
  user = "ouit0409"
  # user = "shiney"
)

dt_main_src_mysql <- {
  shipwreck_finds <- oxrep_db %>%
    tbl("ShipwreckFinds-JC-2017-06-08")
  
  shipwrecks <- oxrep_db %>%
    tbl("Shipwrecks-JC-2017-06-08")
  
  shipwreck_details <- shipwrecks %>%
    left_join(shipwreck_finds)
  
  shipwreck_details %>%
    # select(-contains("ID")) %>%
    collect()
}


end_src_mysql <- Sys.time()

start_pool <- Sys.time()

oxrep_pool <- dbPool(
  drv = RMySQL::MySQL(),
  dbname = "oxrep",
  port = 3306,
  from = "foo2",
  host = "163.1.169.203",
  user = "ouit0409"
  # user = "shiney"
)


dt_main_pool <- {
  shipwreck_finds <- oxrep_pool %>%
    tbl("ShipwreckFinds-JC-2017-06-08")
  
  shipwrecks <- oxrep_pool %>%
    tbl("Shipwrecks-JC-2017-06-08")
  
  shipwreck_details <- shipwrecks %>%
    left_join(shipwreck_finds)
  
  shipwreck_details %>%
    # select(-contains("ID")) %>%
    collect()
}

end_pool <- Sys.time()

end_src_mysql - start_src_mysql

end_pool - start_pool

dt_main_pool
