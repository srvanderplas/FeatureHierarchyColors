best.combo <- function(ngroups=3, palette, dist.matrix){
  # check distance matrix
  if(nrow(dist.matrix)!=length(palette) | ncol(dist.matrix)!=length(palette)){
    stop(paste0("The distance matrix does not match the size of the palette. ",
                "It should be ", length(palette), "x", length(palette), "."))
  }
  if(sum(dist.matrix<0)>0){
    stop("Distance matrix cannot have negative entries.")
  }

  require(combinat)
  clist <- t(combn(1:length(palette), ngroups))
  pairwise.combos <- t(combn(1:ngroups, 2))
  res <- rowSums(apply(pairwise.combos, 1, function(i){diag(as.matrix(dist.matrix[clist[,i[1]], clist[,i[2]]]))}))

  return(palette[clist[which.max(res),]])
}


# Define colors and shapes
colors <-  c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
             "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")

shapes <- c(1,0,3,4,8,5,2,6,-0x25C1, -0x25B7)

colortm <- read.csv("./Data/color-perceptual-kernel.csv")
# colortm[3,4] <- 0
# colortm[4,3] <- 0
colortm[8,] <- 0
colortm[,8] <- 0

shapetm <- read.csv("./Data/shape-perceptual-kernel.csv")
# shapetm[9:10,] <- 0
# shapetm[, 9:10] <- 0
shapetm[9,] <- 0
shapetm[,9] <- 0
shapetm[10,] <- 0
shapetm[,10] <- 0

rgb.dist <- function(cl){
  stopifnot(sum(nchar(cl) != 7) == 0)

  require(stringr)

  red <- str_sub(cl, 2, 3) %>% as.hexmode() %>% as.numeric() %>% dist()
  green <- str_sub(cl, 4, 5) %>% as.hexmode() %>% as.numeric() %>% dist()
  blue <- str_sub(cl, 6, 7) %>% as.hexmode() %>% as.numeric() %>% dist()

  sqrt(red ^ 2 + green ^ 2 + blue ^ 2 + (256 - (red + green + blue)/3) ^ 2) %>% as.matrix
}
