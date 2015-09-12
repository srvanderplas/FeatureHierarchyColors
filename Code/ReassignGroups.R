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
    data.frame(pars, read.csv(file = .$filename, stringsAsFactors = FALSE), stringsAsFactors = FALSE)
  }) %>% ungroup() %>%
  group_by(set) %>%
  do({
    tmp <- .$group
    df <- pick.clusters(., unique(.$k), nrow(.))
    df$oldgroup <- tmp
    df
  })

save(data, file = "Images/DataWithNewGroups.RData")

source("Code/Turk16Palette.R")
library(RColorBrewer)
library(dichromat)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

paired.to.categ <- c(7, 8, 5, 6, 11, 12, 1, 2, 9, 10, 3, 4)

data.frame(
  type = c(
    rep("ggplot", 8),
    rep("turk16", 8),
    rep("tableau", 8),
    rep("colorbrewer", 8),
    rep("dichromat", 8)
  ),
  N = rep(
    c(rep(3, 3), rep(5, 5))
    times = 6
  ),
  color = c(
    gg_color_hue(3), gg_color_hue(5),
    best.combo(3, colors, colortm), best.combo(5, colors, colortm),
    colors[1:3], colors[1:5],
    brewer.pal(3, "Paired"), brewer.pal(5, "Paired")
  )
)
