# motivation:
# reproduce phylomorphospace analysis from Navolon et al (2020).

library(ape)
library(geomorph)
library(here)
library(tidyverse)

# ----------------

# read tree
bird_tree <- ape::read.nexus(here("MCC_phylogeny.nex"))

# import shape data
# 12 landmarks, 24 columns (Figure 3a, Navalon et al 2020)
bill_shapes <- read.csv(here("procrustes_coords_beak_block.csv"))

# rename column 1 
colnames(bill_shapes)[1] <- "taxon"


# --------------
# geometric morphometrics with entire dataset

# convert to npk matrix
# omit column one (species names)
bs_array <- arrayspecs(bill_shapes[,-1], 12, 2)

#replace numbered list with taxon list
dimnames(bs_array)[[3]] <- bill_shapes$taxon


# --------------
# phylomorphospace

# pca on shape coordinates with phylogeny
bill_pca <- gm.prcomp(bs_array, phy = bird_tree)

# quick load
bill_pca <- readRDS(here("rds_files/bill_pca.rds"))

# get some colours from colours from
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
pca_plot <-
  #or geomorph::GMPhyloMorphoSpace? 
  geomorph::plot.gm.prcomp(
    bill_pca,
    pch=21,
    bg=1:nrow(bill_pca$x),
    phylo = TRUE,
    phylo.par = list(edge.color="grey", node.cex=0));
    title(main="phylomorphospace")

# saveRDS(bill_pca, file=here("rds_files/bill_pca.rds")

# reference (mean) shape
ref <- mshape(bs_array)

# deformation grids
plotRefToTarget(ref, bill_pca$shapes$shapes.PC1$min)
plotRefToTarget(ref, bill_pca$shapes$shapes.PC1$max)
plotRefToTarget(ref, bill_pca$shapes$shapes.PC2$min)
plotRefToTarget(ref, bill_pca$shapes$shapes.PC2$max)

