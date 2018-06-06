library(ggplot2)

library(maps)
library(plyr)
library(raster)
library(baad.data)

library(plotbiomes)
whittaker_plot <- ggplot() +
  # add biome polygons
  geom_polygon(data = Whittaker_biomes,
               aes(x    = temp_c,
                   y    = precp_cm,
                   fill = biome),
               # adjust polygon borders
               colour = "gray98",
               size   = 1) + theme_bw()

macro <- read.csv("macros.csv")

lats <- c(9.16, 35.05, 42.54, 44.23, 18.32, 40.04)
longs <- c(-79.85, -83.43, -72.18, -122.15, -65.82, -105.56)
names <- c("BCI", "LUQ", "COW", "HAR", "HJA", "NWT")
sites <- unique(macro$Site)
site_vars <- data.frame(longs=longs, lats=lats)

bci <- macro[which(macro$Site == 'Barro Colorado Island'),]
luq <- macro[which(macro$Site == 'Luquillo LTER'),]
luq <- luq[which(luq$Main_Stem == 'na' | luq$Main_Stem == 'an'),] #an...yep
cow <- macro[which(macro$Site == 'Coweeta LTER'),]
har <- macro[which(macro$Site == 'Harvard Forest LTER'),]
hja <- macro[which(macro$Site == 'HJ Andrews Experimental Forest LTER'),]
nwt <- macro[which(macro$Site == 'Niwot Ridge LTER'),]

macro <- macro[which(macro$Main_Stem == 'na'),]
macro <- macro[which(macro$Habit == 'TREE' | macro$Habit == 'Tree'),]
macro <- rbind(macro, luq)  #Luqillo doesn't have habit data... take it all

bioclim <- getData('worldclim', var='bio', res=10)
spatial_points <- SpatialPoints(site_vars)
site_vars$temp <- extract(bioclim$bio1, spatial_points) * 0.1
site_vars$precip <- extract(bioclim$bio12, spatial_points) / 10
site_vars$codes <- names

#First question: what are crown densities like? Compute projected crown area by assuming allometries
line_area = 200 #200m^2
gentry_area = 5 * line_area #1000m^2
plot_area = 100 * 5 * gentry_area #500000cm^2 = 5000m^2 = 0.5ha

bin = 10
size_classes <- seq(1, 201, by=bin)
densities <- data.frame(density=numeric(), size=integer(), site=factor())
for(i in size_classes)
{
 class <- macro[which(macro$Year5_DBH >= i & macro$Year5_DBH < i+bin),]
 new_den <- data.frame(density=numeric(), size=integer(), site=factor())
 for(site in sites)
 {
   dat <- data.frame(density=length(which(class$Site == site))/plot_area, size=i, site=site)       
   new_den <- rbind(new_den, dat)
 }
 densities <- rbind(densities, new_den)
}

densities <- densities[which(densities$density > 0),]
bci_densities <- densities[which(densities$site == 'Barro Colorado Island'),]
luq_densities <- densities[which(densities$site == 'Luquillo LTER'),]
cow_densities <- densities[which(densities$site == 'Coweeta LTER'),]
har_densities <- densities[which(densities$site == 'Harvard Forest LTER'),]
hja_densities <- densities[which(densities$site == 'HJ Andrews Experimental Forest LTER'),]
nwt_densities <- densities[which(densities$site == 'Niwot Ridge LTER'),]

density_allom <- lm(log(densities$density^(-(1/2))) ~ log(densities$size))
bci_allom <- lm(log(bci_densities$density^(-(1/2))) ~ log(bci_densities$size))
luq_allom <- lm(log(luq_densities$density^(-(1/2))) ~ log(luq_densities$size))
cow_allom <- lm(log(cow_densities$density^(-(1/2))) ~ log(cow_densities$size))
har_allom <- lm(log(har_densities$density^(-(1/2))) ~ log(har_densities$size))
hja_allom <- lm(log(hja_densities$density^(-(1/2))) ~ log(hja_densities$size))
nwt_allom <- lm(log(nwt_densities$density^(-(1/2))) ~ log(nwt_densities$size))

biome_densities <- c(bci_allom$coefficients[1], luq_allom$coefficients[1], cow_allom$coefficients[1], 
                     har_allom$coefficients[1], hja_allom$coefficients[1], nwt_allom$coefficients[1])

ggplot(densities, aes(x=log(size), y=log(density^(-(1/2))), color=site)) + geom_point() +
  geom_abline(slope = bci_allom$coefficients[2], intercept = bci_allom$coefficients[1], color = 'red') + 
  geom_abline(slope = cow_allom$coefficients[2], intercept = cow_allom$coefficients[1], color = 'yellow') +
  geom_abline(slope = har_allom$coefficients[2], intercept = har_allom$coefficients[1], color = 'green') +
  geom_abline(slope = hja_allom$coefficients[2], intercept = hja_allom$coefficients[1], color = 'cyan') +
  geom_abline(slope = luq_allom$coefficients[2], intercept = luq_allom$coefficients[1], color = 'blue') +
  geom_abline(slope = nwt_allom$coefficients[2], intercept = nwt_allom$coefficients[1], color = 'magenta') +
  geom_abline(slope = density_allom$coefficients[2], intercept = density_allom$coefficients[1], color = 'black') 

height_alloms <- baad$data[which(baad$data$h.t > 0 & baad$data$d.bh > 0),]
height_alloms$d.bh <- height_alloms$d.bh * 100
height_alloms$h.t <- height_alloms$h.t * 100
trf_heights <- height_alloms[which(height_alloms$vegetation == "TropRF"),]
tsf_heights <- height_alloms[which(height_alloms$vegetation == "TropSF"),]
temf_heights <- height_alloms[which(height_alloms$vegetation == "TempF"),]
temrf_heights <- height_alloms[which(height_alloms$vegetation == "TempRF"),]
borf_heights <- height_alloms[which(height_alloms$vegetation == "BorF"),]

height_allom <- lm(log(height_alloms$h.t) ~ log(height_alloms$d.bh))
trf_allom <- lm(log(trf_heights$h.t) ~ log(trf_heights$d.bh))
tsf_allom <- lm(log(tsf_heights$h.t) ~ log(tsf_heights$d.bh))
temf_allom <- lm(log(temf_heights$h.t) ~ log(temf_heights$d.bh))
temrf_allom <- lm(log(temrf_heights$h.t) ~ log(temrf_heights$d.bh))
borf_allom <- lm(log(borf_heights$h.t) ~ log(borf_heights$d.bh))

biome_heights_h <- c(trf_allom$coefficients[1], tsf_allom$coefficients[1], temf_allom$coefficients[1], 
                     temf_allom$coefficients[1], temrf_allom$coefficients[1], borf_allom$coefficients[1])
biome_heights_b <- c(trf_allom$coefficients[2], tsf_allom$coefficients[2], temf_allom$coefficients[2], 
                     temf_allom$coefficients[2], temrf_allom$coefficients[2], borf_allom$coefficients[2])

ggplot(height_alloms, aes(x=log(d.bh), y=log(h.t), color=vegetation)) + geom_point(alpha=0.5) +
  geom_abline(slope = trf_allom$coefficients[2], intercept = trf_allom$coefficients[1], color = 'cyan') + 
  geom_abline(slope = tsf_allom$coefficients[2], intercept = tsf_allom$coefficients[1], color = 'magenta') +
  geom_abline(slope = temf_allom$coefficients[2], intercept = temf_allom$coefficients[1], color = 'green') +
  geom_abline(slope = temrf_allom$coefficients[2], intercept = temrf_allom$coefficients[1], color = 'turquoise') +
  geom_abline(slope = borf_allom$coefficients[2], intercept = borf_allom$coefficients[1], color = 'red') +
  geom_abline(slope = height_allom$coefficients[2], intercept = height_allom$coefficients[1], color = 'black')

crown_alloms <- baad$data[which(baad$data$a.cp > 0 & baad$data$d.bh > 0),]
crown_alloms$d.bh <- crown_alloms$d.bh * 100
crown_alloms$a.cp <- crown_alloms$a.cp * 100
trf_crowns <- crown_alloms[which(crown_alloms$vegetation == "TropRF"),]
tsf_crowns <- crown_alloms[which(crown_alloms$vegetation == "TropSF"),]
temf_crowns <- crown_alloms[which(crown_alloms$vegetation == "TempF"),]
temrf_crowns <- crown_alloms[which(crown_alloms$vegetation == "TempRF"),]
borf_crowns <- crown_alloms[which(crown_alloms$vegetation == "BorF"),]

#Must assume 2/3 slope for allometrically ideal - just find constants
crown_c2 <- mean(log(crown_alloms$a.cp) - (2/3)*log(crown_alloms$d.bh))
trf_c2 <- mean(log(trf_crowns$a.cp) - (2/3)*log(trf_crowns$d.bh))
tsf_c2 <- mean(log(tsf_crowns$a.cp) - (2/3)*log(tsf_crowns$d.bh))
temf_c2 <- mean(log(temf_crowns$a.cp) - (2/3)*log(temf_crowns$d.bh))
temrf_c2 <- mean(log(temrf_crowns$a.cp) - (2/3)*log(temrf_crowns$d.bh))
borf_c2 <- mean(log(borf_crowns$a.cp) - (2/3)*log(borf_crowns$d.bh))

biome_crowns <- c(trf_c2, tsf_c2, temf_c2, temf_c2, temrf_c2, borf_c2)

ggplot(crown_alloms, aes(x=log(d.bh), y=log(a.cp), color=vegetation)) + geom_point(alpha=0.5) +
  geom_abline(slope = (2/3), intercept = trf_c2, color = 'cyan') + 
  geom_abline(slope = (2/3), intercept = tsf_c2, color = 'blue') +
  geom_abline(slope = (2/3), intercept = temf_c2, color = 'yellow') +
  geom_abline(slope = (2/3), intercept = temrf_c2, color = 'green') +
  geom_abline(slope = (2/3), intercept = borf_c2, color = 'red') +
  geom_abline(slope = (2/3), intercept = crown_c2, color = 'black')

site_vars$c1 <- biome_densities
site_vars$c2 <- biome_crowns
site_vars$h <- biome_heights_h
site_vars$b <- biome_heights_b
ggplot(site_vars, aes(x=temp, y=c1, color=codes)) + geom_point(size=5)
ggplot(site_vars, aes(x=precip, y=c1, color=codes)) + geom_point(size=5)
ggplot(site_vars, aes(x=temp, y=c2, color=codes)) + geom_point(size=5)
ggplot(site_vars, aes(x=precip, y=c2, color=codes)) + geom_point(size=5)
ggplot(site_vars, aes(x=temp, y=h, color=codes)) + geom_point(size=5)
ggplot(site_vars, aes(x=precip, y=h, color=codes)) + geom_point(size=5)
ggplot(site_vars, aes(x=temp, y=b, color=codes)) + geom_point(size=5)
ggplot(site_vars, aes(x=precip, y=b, color=codes)) + geom_point(size=5)

site_whittaker <- whittaker_plot + geom_point(data = site_vars, aes(x=temp, y=precip)) +
  geom_text(data = site_vars, aes(x=temp, y=precip, label=codes), nudge_x = 1.25, nudge_y = 1)

world <- map_data("world")
wm <- ggplot(world, aes(x=long, y=lat, group=group)) + 
  geom_polygon(fill="white", colour="black", size=0.05) + coord_quickmap()
plot_2 <- wm + geom_point(data=site_vars, aes(x=longs, y=lats, group=NULL, color=codes), size = 5) +
                geom_text(data=site_vars, aes(x=longs, y=lats, group=NULL, label=codes), size = 5, nudge_x = -3, nudge_y = 7)
ggsave(file="world.tiff", width=48, height=24, units="cm")

#Plot year 5 stem sizes
ggplot(macro, aes(x=Year5_DBH)) + geom_histogram() + facet_grid(. ~ Site)

get_bien_data <- function() 
{
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
  print(paste("Numbers of stems in original data", nrow(stems)))
  print("Should be over 12 million")
  
  #Try to clean up the data a wee bit
  non_na_stems <- stems[which(!is.na(stems$stem_height_m) & !is.na(stems$stem_dbh_cm)),]
  rm(stems)
  
  #Try to clean up the data a little bit
  scrubbed_stems <- non_na_stems[which(non_na_stems$stem_dbh_cm >= 10 & non_na_stems$stem_height_m > 0),]
  rm(non_na_stems)
  gc()
}