# motivation:
# subset entire Navalon (2020) dataset to
# Psittaciformes (tips 164-208) and Passeriformes (tips 209-434)


library(ape)
library(geomorph)
library(here)
library(tidyverse)


# ----------------
# phylogeny

# read tree
# tips 164-208 Psittaciformes
# tips 209-434 Passeriformes
bird_tree <- ape::read.nexus(here("navalon/data/MCC_phylogeny.nex"))


# create vector of species from Psittaciformes and Passeriformes
# 45 parrots, 226 passerines
pp <- bird_tree$tip.label[164:434]

# prune tree
pp_tree <- keep.tip(phy = bird_tree, pp)


# ----------------
# shape data

# import original shape data
# 12 landmarks, 24 columns (Figure 3a, Navalon et al 2020)
bill_shapes <- read.csv(here("navalon/data/procrustes_coords_beak_block.csv"))

#rename column 1
colnames(bill_shapes)[1] <- "taxon"

# subset shape data to parrots and passerines (pp)
bs_subset <- filter(bill_shapes, taxon %in% pp)

# convert to npk matrix
# omit column one (species names)
pp_arr <- arrayspecs(bs_subset[,-1], 12, 2)


# procrustes alignment
pp_gpa <- two.d.array(gpagen(pp_arr)$coords)


# change taxon names to character strings
dimnames(pp_arr)[[3]] <- bs_subset$taxon


# ----------------
# phylomorphospace

#names are not alphabetical (as in the shape data)
bill_pca <- gm.prcomp(pp_arr, phy = pp_tree)

# save bc PCA takes a while to run..
saveRDS(bill_pca, file= here("navalon/rds_files/pp_pca.rds"))


pca_plot <-
  plot(
    bill_pca,
    pch=21,
    cex=1.5,
    col="#CC79A7",
    bg="#CC79A7",
    phylo = TRUE,
    phylo.par = list(edge.color="grey", node.cex=0, tip.labels=FALSE, node.labels=FALSE))

  title(main="phylomorphospace")


# ------------------------
# deformation grids

# reference (mean) shape
ref <- mshape(pp_arr)

# p x k matrix

pkmat <- gpagen(pp_arr)$coords


# grid parameters
gp = gridPar(
       tar.pt.size=3, grid.lwd = 0.5, grid.col='black')

# deformation grids
plotRefToTarget(ref, bill_pca$shapes$shapes.comp1$min, gridPars = gp)
plotRefToTarget(ref, bill_pca$shapes$shapes.comp1$max, gridPars = gp)
plotRefToTarget(ref, bill_pca$shapes$shapes.comp2$min, gridPars = gp)
plotRefToTarget(ref, bill_pca$shapes$shapes.comp2$max, gridPars = gp)


# also could compare
# Bucerotiformes (rows 22-31) and Ramphastidae (rows 76-80)
