library(dplyr)
library(magrittr)
library(stringr)

# source program files
source("Code/MixtureLineupsTurk18.R")

# load data files
filelist <- data.frame(filename = list.files(path = "./Images/Data/", pattern = "^set(.)*", full.names = TRUE), tmp = 1, stringsAsFactors = F)

extract.data.pars <- function(filename){
  data.frame(
    # set = str_extract(filename, "set-\\d{1,2}") %>% str_replace("set-", "") %>% as.numeric(),
    k = str_extract(filename, "k-\\d") %>% str_replace("k-", "") %>% as.numeric(),
    sdline = str_extract(filename, "sdline-0\\.\\d{2}") %>% str_replace("sdline-", "") %>% as.numeric(),
    sdgroup = str_extract(filename, "sdgroup-0\\.\\d{2}") %>% str_replace("sdgroup-", "") %>% as.numeric()
  )
}

data <- filelist %>%
  rowwise() %>%
  do({
    pars <- extract.data.pars(.$filename)
    data.frame(pars, read.csv(file = .$filename, stringsAsFactors=FALSE), stringsAsFactors=FALSE)
  }) %>% ungroup() %>%
  group_by(set) %>%
  do({
    tmp <- .$group
    df <- pick.clusters(., unique(.$k), nrow(.))
    df$oldgroup <- tmp
    df
  })

save(data, file = "Images/DataWithNewGroups.RData")
