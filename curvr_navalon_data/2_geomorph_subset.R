# motivation:
# subset entire Navalon (2020) dataset to
# just Psittaciformes (rows 158-208)
# Bucerotiformes (rows 22-31) and Ramphastidae (rows 76-80)
# can be subsetted as well..


library(ape)
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

#rename column 1
colnames(bill_shapes)[1] <- "taxon"

# subset shape data to parrots
bs_subset <- filter(bill_shapes, taxon %in% parrots)

# convert to npk matrix
# omit column one (species names)
parrot_arr <- arrayspecs(bs_subset[,-1], 12, 2)


# write tps file for making outlines
writeland.tps(parrot_arr*450+250, file="parrot_lms.tps", specID=TRUE)


# import after curves created in tpsDig2
parrot_tps <-
  readland.tps(file="parrot_lms.tps", specID="ID", readcurves=TRUE)

# gpa
parrot_gpa <- two.d.array(gpagen(parrot_tps)$coords)

# array-ize
parrot_arr2 <- arrayspecs(parrot_gpa, 23, 2)

# change taxon names to character strings
dimnames(parrot_arr2)[[3]] <- bs_subset$taxon


# ----------------
# phylomorphospace

#names are not alphabetical (as in the shape data)
bill_pca3 <- gm.prcomp(parrot_arr2, phy = parrot_tree)

saveRDS(bill_pca3, file= here("rds_files/parrot_pca.rds"))



pca_plot <-
  plot(
    bill_pca3,
    pch=21,
    cex=1.5,
    col="#CC79A7",
    bg="#CC79A7",
    phylo = TRUE,
    phylo.par = list(edge.color="grey", node.cex=0))

  title(main="phylomorphospace")



# reference (mean) shape
ref3 <- mshape(parrot_arr2)

# p x k matrix

pkmat <- gpagen(parrot_tps)$coords

pkmat2 <- matrix(pkmat, dim(pkmat)[1], dim(pkmat)[2])
ref_out <- warpRefOutline(file = here("parrot_lms.tps"), pkmat2, ref3)

# grid parameters
gp = gridPar(
       tar.pt.size=3, grid.lwd = 0.5, grid.col='black')

# deformation grids
plotRefToTarget(ref3, bill_pca3$shapes$shapes.PC1$min, gridPars = gp)
plotRefToTarget(ref3, bill_pca3$shapes$shapes.PC1$max, gridPars = gp)
plotRefToTarget(ref3, bill_pca3$shapes$shapes.PC2$min)
plotRefToTarget(ref3, bill_pca3$shapes$shapes.PC2$max)

# outlines
ref_out <- warpRefOutline(file=here("parrot_lms.tps"), two.d.array(parrot_arr), ref3)
