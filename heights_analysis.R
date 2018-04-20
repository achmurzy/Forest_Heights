library(ggplot2)
library(BIEN)

#This analysis must be done at the plot level in order to compare tree size distributions within a stand 
#Which plots can we use? Which plots are 'forests'?

#Load FIA data - 
#ref_unit <- read.csv("REF_UNIT.csv")
#ref_spec <- read.csv("REF_PLANT_DICTIONARY.csv")
fia_trees <- read.csv("TREE.csv")

#Get stem data and height data for trees from BIEN - no way to connect trait obs to one individual in time? By plot?
traits <- c("whole plant height", "diameter at breast height (1.3 m)")
height_dbm <- BIEN_trait_trait(traits)

plot_metadata <- BIEN_plot_metadata()
candidates <- which(plot_metadata$growth_forms_included_trees == "Yes" & plot_metadata$has_stem_data == "yes")
plots <- BIEN_plot_name(plot_metadata[candidates,]$plot_name)

#stem data loaded from BIEN_stem_datasource() for all datasources available
stems <- load("stems.rdata")
print(paste("Numbers of stems in original data", nrow(stems))
print("Should be over 12 million")

#Try to clean up the data a wee bit
non_na_stems <- stems[which(!is.na(stems$stem_height_m) & !is.na(stems$stem_dbh_cm)),]
rm(stems)

#Try to clean up the data a little bit
scrubbed_stems <- non_na_stems[which(non_na_stems$stem_dbh_cm >= 10 & non_na_stems$stem_height_m > 0),]
rm(non_na_stems)
gc()