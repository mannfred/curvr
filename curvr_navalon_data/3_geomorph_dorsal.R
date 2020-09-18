# motivation:
# plot phylomorphospace of dorsal landmarks
#
# p.271, Navalon et al (2020), Nature E&E
# Parrots (Psittaciformes) are characterized by a single ancestral
# shift towards very high rates of skull evolution, resulting in a
# characteristic cranial anatomy with short, curved beaks and expanded
# braincases.


# Psittaciformes (51 spp.)
# rows 158-208

library(ape)
library(curvr)
library(geomorph)
library(here)
library(tidyverse)

# ----------------
# phylogeny

# read tree
bird_tree <- ape::read.nexus(here("MCC_phylogeny.nex"))


# create vector of species from Psittaciformes (Polytelis to Strigops)
# 45 spp.
parrots <- bird_tree$tip.label[164:208]


# prune tree
parrot_tree <- keep.tip(phy = bird_tree, parrots)


# ----------------
# shape data

# import original shape data
# 12 landmarks, 24 columns (Figure 3a, Navalon et al 2020)
bill_shapes <- read.csv(here("procrustes_coords_beak_block.csv"))

# rename column 1
colnames(bill_shapes)[1] <- "taxon"

# subset shape data to parrots
bs_subset <- filter(bill_shapes, taxon %in% parrots)



# --------------
# create dorsal landmark data

# subset landmarks to dorsal side of bill
#lms 1,2 semi-lms 7,8,9
dorsal_lms <- bs_subset[,c(1:5, 14:19)]

# convert to npk matrix
# omit column one (species names)
dorsal_arr <- arrayspecs(dorsal_lms[,-1], 5, 2)

#replace numbered list with taxon list
dimnames(dorsal_arr)[[3]] <- bs_subset$taxon

# save
saveRDS(dorsal_arr, file=here("rds_files/dorsal_landmarks.rds"))

# ----------------
# phylomorphospace

# pca on dorsal coordinates with phylogeny
bill_pca2 <- gm.prcomp(dorsal_arr, phy = parrot_tree)

saveRDS(bill_pca2, file=here("rds_files/dorsal_curve_pca.rds"))


pca_plot <-
  plot(
    bill_pca2,
    pch=21,
    cex=1.5,
    col="#CC79A7",
    bg="#CC79A7",
    phylo = TRUE,
    phylo.par = list(edge.color="grey", node.cex=0))

title(main="dorsal curvature morphospace")

# reference (mean) shape
ref2 <- mshape(dorsal_arr)

# deformation grids
plotRefToTarget(ref2, bill_pca2$shapes$shapes.PC1$min)
plotRefToTarget(ref2, bill_pca2$shapes$shapes.PC1$max)
plotRefToTarget(ref2, bill_pca2$shapes$shapes.PC2$min)
plotRefToTarget(ref2, bill_pca2$shapes$shapes.PC2$max)


