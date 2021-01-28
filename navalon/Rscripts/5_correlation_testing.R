# test for correlations between principal components
# of shape space and dorsal curve space

library(ape)
library(geomorph)
library(here)
library(tidyverse)


# -----------

# colours
cbPalette <- c('#999999', '#E69F00', '#56B4E9', '#009E73', '#F0E442', '#0072B2', '#D55E00', '#CC79A7')

# preview colours
plot(1:8, rep(5,8), pch=21, col = cbPalette, bg = cbPalette, cex=4)

# -----------------
# divergence times for plotting

# read tree
bird_tree <- ape::read.nexus(here("MCC_phylogeny.nex"))

# create vector of species from Psittaciformes (Polytelis to Strigops)
# 45 spp.
parrots <- bird_tree$tip.label[164:208]

# prune tree
parrot_tree <- keep.tip(phy = bird_tree, parrots)

# quick function for finding tips (n=1:45) from phylogeny$edge
fun1 <- function(x) which(parrot_tree$edge[,2] == x)

# find rows containing tips (species) 1:45
rownums <- sapply(1:45, fun1)

# get branch lengths
branches <- 
  enframe(parrot_tree$edge.length) %>% 
  slice(rownums)


  

# --------------

# read shape data
shape_pca <- readRDS(here("rds_files/parrot_pca.rds"))
k <- readRDS(here("rds_files/curvature_tbl_dorsal.rds"))

# are pc1 of  and adjusted curvature correlated?
# model variables
shapePC1 <-  shape_pca$x[,1]
dors_curv <- k$value

identical(k$name, names(shapePC1)) #TRUE




#--------------
# plotting

mydata <- tibble(dors_curv, shapePC1, branches)

# geom_smooth formula calculated from pgls()
ggplot(data = mydata) +
  geom_point(aes(x = dors_curv, y = shapePC1, colour = branches$value ), size=5) +
  # geom_abline(slope = mod1$model$coef[2], intercept = mod1$model$coef[1]) +
  labs(
    y = "PC1 of Shape Variation (35.3%)",
    x = "total curvature") +
  scale_colour_gradientn(colours = c(cbPalette[6], cbPalette[7]), name = "branch length")+
  theme_classic() + 
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title =  element_text(size=14)
)




# ------------
# stats

library(caper)

phydata <- 
  comparative.data(
    phy = parrot_tree, 
    data = data.frame(shapePC1, dors_curv, parrots),
    names.col = parrots,
    vcv = TRUE)


mod1 <- 
  pgls(
    formula = shapePC1 ~ dors_curv, 
    data = phydata,
    lambda = 'ML')
summary(mod1)











# --------------

# K vs dorsal curve 
dorsal_pca <- readRDS(here("rds_files/dorsal_curve_pca.rds"))
k <- readRDS(here("rds_files/curvature_tbl_dorsal.rds"))


# are pc1 of  and adjusted curvature correlated?
# model variables
dorsalPC1 <- dorsal_pca$x[,1]
dors_curv <- k$total_curvature

identical(k$name, names(dorsalPC1)) #TRUE


# create group colours for plotting 
colour_ids <-
  c(replicate(10, "#E69F00"), 
    replicate(5, "#56B4E9"),
    replicate(51, ))


# plot
plot(
  dors_curv, 
  dorsalPC1, 
  col = "#CC79A7",
  pch=21,
  bg = "#CC79A7")

title("PC1 of dorsal curve space vs curvature")

lm2<-lm(dorsalPC1 ~ dors_curv)
anova(lm2)




