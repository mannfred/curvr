# motivation: fit polynomials to dorsal bird bills
# then, compute curvature

library(curvr)
library(geomorph)
library(here)
library(Momocs)
library(tidyverse)



# ---------------
# import landmark data

# import dorsal landmarks
dorsal_arr <- readRDS(here("rds_files/dorsal_landmarks.rds"))

# Momocs::Ldk() converts to Coo object
dorsal <-
  dorsal_arr[1:5,,] %>%
  Ldk()

# rearrange second row to be last landmark
dorsal_r <- Momocs::rearrange_ldk(dorsal, c(1,3,4,5,2))

# ---------------
# alignment

# align the longest axis of each shape along the x-axis
dorsal_x <- coo_alignxax(dorsal_r)


# fit polynomials to landmarks
poly_list <-
  map(dorsal_x$coo, Momocs::npoly, 3)


# plot landmarks
coo_plot(dorsal_x$coo[25])

# inspect polynomial fit
# inspect 9, 13, 14, 21, 22, 23..
poly_list[[25]] %>%
  npoly_i() %>% #calculates shape from polynomial model
  coo_draw(border='red')


# extract all R2 fits
r2 <- list()

for (i in 1:length(poly_list)) {
  r2[[i]] <- poly_list[[i]]$r2
}

# visualize R2 distribution
# lowest R2 is 0.97 for degrees = 4
plot(1:45,
     as.numeric(r2),
     text(x=1:45,
          y=as.numeric(r2),
          labels=dimnames(dorsal_arr)[[3]]))



# ----------------
# calculate curvature

# extract the lower and upper bounds from b[5] and b[4], respectively
baselines_list <-
  poly_list %>%
  lapply(., function(b) c(unlist(b[5])[1], unlist(b[4])[1]))

# calculate total curvature
# units are in degrees per unit length
# taxa are ordered alphabetically (unlike shape data)
curvature_tbl <-
  curvr::total_curvature %>%
  mapply(., poly_list, baselines_list, 5) 

# keep totalK only
totalK <- curvature_tbl[2,]


# --------------------
# reorder names from alphabetical to 
# match phylogenetic order from PCA data

# reassign taxonomic names
names(totalK) <- dimnames(dorsal_arr)[[3]]
totalK <- enframe(unlist(totalK))


# import some shape data to get correct taxon order
parrot_pca <- readRDS(file= here("rds_files/parrot_pca.rds"))
taxon <- rownames(parrot_pca$x)

# reorder curvature table by row order in parrot_pca
totalK_reorder <-
  totalK[match(taxon, totalK$name),]

# save
saveRDS(totalK_reorder, file = here("rds_files/curvature_tbl_dorsal.rds"))



# ------------
# very curved:
# 2 - 94.30
# 8 - 100.35
# 9 - 97.34

# not curved:
# 5 - 66.03
# 6 - 60.96
# 10 - 71.17



# calculate arc length by first parameterizing polynomials by t
param_list <-
  lapply(poly_list, curvr::parameterize)  # a list of 31 parameterized polynomial functions




