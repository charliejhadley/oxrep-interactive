library("tidyverse")

backup_mines <- read_csv("data-rescue/backup-mines.csv")

backup_mines %>%
  write_csv("data-rescue/Mines.csv", na = "")
