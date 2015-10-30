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

# set.seed(32309813)
# data <- filelist %>%
#   rowwise() %>%
#   do({
#     pars <- extract.data.pars(.$filename)
#     data.frame(pars, read.csv(file = .$filename, stringsAsFactors = FALSE), stringsAsFactors = FALSE)
#   }) %>% ungroup() %>%
#   group_by(set, .sample) %>%
#   do({
#     # Only reassign groups to samples which are not the cluster target sample
#     if (unique(.$.sample) == unique(.$target2)) {
#       df <- .
#       df$oldgroup <- df$group
#     } else {
#       tmp <- .$group
#       df <- pick.clusters(., unique(.$k), nrow(.))
#       df$oldgroup <- tmp
#     }
#     df
#   })
#
# save(data, file = "Images/DataWithNewGroups.RData")
load("Images/DataWithNewGroups.RData")
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

plot.opts <- data.frame(
  expand.grid(
    i = unique(data$set),
    j = plot.names,
    k = unique(color.matrix$type)
  )
)

plot.opts$idx <- 1:nrow(plot.opts)

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

# Merge to determine which stuff to keep...
plot.opts <- merge(plot.opts, data.stats, by.x = c("i"), by.y = c("set")) %>% arrange(idx)

# subset plot.opts according to criteria for experiment design...
plot.opts <- subset(
  plot.opts,
  (str_detect(k, "turk16")) |
    (K == 3 & sd.trend %in% c(0.35, 0.45) & sd.cluster %in% c(0.30, 0.35)) |
    (K == 5 & sd.trend %in% c(0.35, 0.45) & sd.cluster %in% c(0.25, 0.30))
)


# init_cluster(cores=14, quiet = F)
#
# picture.details <- plot.opts %>% rowwise() %>% do({
#   i <- .$i
#   j <- .$j
#   k <- .$k
#   save.pics(subset(data, set == i), datastats = data.stats[i,],
#             plotparms = plot.parms[j,], plotname = j, palname = k,
#             colorp = subset(color.matrix, N == data.stats$K[i] & type == k)$color)
# })

library(multicore)

make.files <- function(z){
  . <- plot.opts[z,]
  i <- .$i
  j <- .$j
  k <- .$k
  tmp <- save.pics(subset(data, set == i), datastats = data.stats[i,],
                   plotparms = plot.parms[j,], plotname = j, palname = k,
                   colorp = subset(color.matrix, N == data.stats$K[i] & type == k)$color)
  message(paste0(tmp$filename, " completed"))
  gc(verbose = F, reset = F)
  return(tmp)
}


res <- mclapply(1:nrow(plot.opts), make.files, mc.preschedule = F, mc.cores = 14, mc.cleanup = TRUE)

picture.details <- bind_rows(res)
picture.details$test_param <- paste0(
  str_replace(picture.details$test_param, "turk16", "turk18"),
  "-", picture.details$palname
)

picture.details2 <- select(picture.details, -palname)
# picture.details2 <- subset(
#   picture.details,
#   (str_detect(test_param, "turk16")) |
#     (str_detect(param_value, "k-3") &
#        str_detect(param_value, "sdline-0\\.[34]5") &
#        str_detect(param_value, "sdgroup-0\\.3[05]")) |
#     (str_detect(param_value, "k-5") &
#        str_detect(param_value, "sdline-0\\.[34]5") &
#        str_detect(param_value, "sdgroup-(0\\.25|0\\.30)"))
#   )

picture.details2$i <- str_extract(picture.details2$data_name, "set-\\d{1,2}") %>% str_replace("set-", "") %>% as.numeric
picture.details2$j <- (str_replace(picture.details2$test_param, "turk18-(\\w*)-(.*)", "\\1") %>% factor() %>% as.numeric()) - 1
picture.details2$k <- (str_replace(picture.details2$test_param, "turk18-(.*)-(\\w*)", "\\2") %>% factor() %>% as.numeric()) - 1
picture.details2$sample_size <- str_replace(picture.details2$param_value, "k-(\\d)-(.*)", "\\1") %>% as.numeric
picture.details2$pic_id <- with(picture.details2, sprintf("%d%d%d", i, j, k)) %>% as.numeric()
picture.details2$p_value <- ""
picture.details2$difficulty <- picture.details2$i

picture.details3 <- select(picture.details2, pic_id, sample_size, test_param, param_value, p_value, obs_plot_location, pic_name, experiment, difficulty, data_name)

write.csv(picture.details, "./Images/Lineups/picture-details-incomplete.csv", row.names = FALSE)
write.csv(picture.details3, "./Images/Lineups/picture-details.csv", row.names=FALSE)

picture.details <- read.csv("./Images/Lineups/picture-details.csv", stringsAsFactors = F)

files <- paste0("Images/Lineups/svgs/", list.files("./Images/Lineups/svgs"))
del.files <- !(files%in%picture.details$pic_name)
file.remove(gsub("svg", "pdf", files)[del.files])
file.remove(files[del.files])

