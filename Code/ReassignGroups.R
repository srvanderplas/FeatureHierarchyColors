library(dplyr)
library(magrittr)
library(stringr)
library(ggplot2)
library(digest)
library(Cairo)

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

set.seed(32309813)
data <- filelist %>%
  rowwise() %>%
  do({
    pars <- extract.data.pars(.$filename)
    data.frame(pars, read.csv(file = .$filename, stringsAsFactors = FALSE), stringsAsFactors = FALSE)
  }) %>% ungroup() %>%
  group_by(set, .sample) %>%
  do({
    tmp <- .$group
    df <- pick.clusters(., unique(.$k), nrow(.))
    df$oldgroup <- tmp
    df
  })

# data %>% group_by(set, .sample) %>%
#   summarize(minpts = min(table(group)), maxpts = max(table(group))) %>%
#   subset(minpts<4)



save(data, file = "Images/DataWithNewGroups.RData")

source("Code/Turk16Palette.R")
library(RColorBrewer)
library(dichromat)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

paired.to.categ <- c(7, 8, 5, 6, 11, 12, 1, 2, 9, 10, 3, 4)

color.matrix <- data.frame(
  type = c(
    rep("ggplot", 8),
    rep("turk16", 8),
    rep("tableau", 8),
    rep("colorbrewer", 8),
    rep("dichromat", 8)
  ),
  x = rep(c(1:3, 1:5), times = 5),
  N = rep(
    c(rep(3, 3), rep(5, 5)),
    times = 5
  ),
  color = c(
    gg_color_hue(3), gg_color_hue(5),
    best.combo(3, colors, colortm), best.combo(5, colors, colortm),
    colors[1:3], colors[1:5],
    brewer.pal(3, "Paired"), brewer.pal(5, "Paired"),
    best.combo(3, colorschemes$Categorical.12, rgb.dist(colorschemes$Categorical.12)),
    best.combo(5, colorschemes$Categorical.12, rgb.dist(colorschemes$Categorical.12))
  ),
  stringsAsFactors = F
)

# qplot(x = x, y = type, color = color, data = color.matrix, geom = "point", size = I(3)) + facet_wrap(~N, scales = "free_x") + scale_color_identity() + theme_bw()

source("Code/MixtureLineupsTurk18.R")

source("./Code/theme_lineup.R")

plot.names <- c("color", "colorEllipse", "colorTrend", "colorEllipseTrendError")

plot.opts <- data.frame(expand.grid(i=unique(data$set), j=plot.names, k = unique(color.matrix$type)))

data.stats <- data[,c(1:4, 9:10)] %>% unique() %>%
  mutate(K = k, sd.trend = sdline, sd.cluster = sdgroup) %>% select(-k, -sdline, -sdgroup)

plot.parms <- expand.grid(
  color = c(0,1),
  shape = c(0,1),
  reg = c(0,1),
  err = c(0,1),
  ell = c(0,1)
)[c(
  # 1, # control
  2, # color
  # 3, # shape
  # 4, # color + shape
  18, # color + ellipse
  # 20, # color + shape + ellipse
  # 5, # trend
  # 13, # trend + error
  6, # color + trend
  30 # color + ellipse + trend + error
),]

picture.details <- plot.opts %>% rowwise() %>% do({
  i <- .$i
  j <- .$j
  k <- .$k
  save.pics(subset(data, set == i), datastats = data.stats[i,],
            plotparms = plot.parms[j,], plotname = j, palname = k,
            colorp = subset(color.matrix, N == data.stats$K[i] & type == k)$color)
})

write.csv(picture.details[,-c(1:2)], "./Images/Lineups/picture-details.csv", row.names=FALSE)

files <- paste0("Images/Lineups/svgs/", list.files("./Images/Lineups/svgs"))
del.files <- !(files%in%picture.details$pic_name)
file.remove(gsub("svg", "pdf", files)[del.files])
file.remove(files[del.files])

