library(camtrapR)
library(exiftoolr)
library(dplyr)
library(rgdal)
library(secr)
library(readxl)

getwd()
setwd("S:/Carcass_cameras_May_Big_boggy")

camtraps<-read_excel("CTtable_winter_carcasses_exclD14-16.xlsx")
camtraps


#MAKING IMAGE FOLDERS:

# create a temporary dummy directory for tests
# (normally, you'd set up an empty directory in a proper place, e.g. .../myStudy/rawImages)
wd_createStationDir <- file.path("S:/Carcass_cameras_May_Big_boggy/SDs_retrieved_end_May2022/camtrapr", "createStationFolders")

# create station directories in  wd_createStationDir
StationFolderCreate1 <- createStationFolders (inDir       = wd_createStationDir,
                                              stations    = as.character(camtraps$Station), 
                                              createinDir = TRUE)


#RENAMING IMAGES:

# raw image location
wd_images_raw <- file.path("S:/Carcass_cameras_May_Big_boggy/SDs_retrieved_end_May2022/camtrapr/createStationFolders2")

# destination for renamed images to be copied to
wd_images_raw_renamed <- file.path("SDs_retrieved_end_May2022/camtrapr/raw_images_renamed")       

renaming.table2 <- imageRename(inDir               = wd_images_raw,
                               outDir              = wd_images_raw_renamed,       
                               hasCameraFolders    = FALSE,
                               copyImages          = TRUE
)








# find the directory with sample images contained in the package
wd_images_ID <- file.path("S:/Carcass_cameras_May_Big_boggy/SDs_retrieved_end_May2022/camtrapr/raw_images_renamed")

length(list.files(wd_images_ID, pattern = "JPG", recursive = TRUE))

rec.db.species0 <- recordTable(inDir  = wd_images_ID,
                               IDfrom = "metadata",
                               metadataSpeciesTag = "Species")

##ignore below in normal code, fix for when metadata from digikam tagging is not written to image:
wd_images_ID <- file.path("F:/Carcass_cameras_May_Big_boggy/NEW RENAMED SDs_retrieved_oct2022")

rec_table5 <- recordTable(inDir  = wd_images_ID,
                          IDfrom = "directory",
                          metadataSpeciesTag = "Species",
                          video  = list(file_formats = c("jpg", "mp4", "avi", "mov"),
                                        dateTimeTag  = "QuickTime:CreateDate",
                                        db_directory = "C:/Users/eliem'/Pictures",
                                        db_filename = "digikam4.db")
)

?recordTableIndividual
###end of troubleshooting

head(rec.db.species0)

#skip this! Is already done below! Removing duplicates closer than 10 mins apart:
rec.db.species60 <- recordTable(inDir               = wd_images_ID,
                                IDfrom              = "metadata",
                                metadataSpeciesTag = "Species",
                                minDeltaTime        = 10,
                                deltaTimeComparedTo = "lastRecord",
                                timeZone            = "Australia/Sydney")

#see what species we recorded:
table(rec.db.species0$Species)


#####POSSIBLE TO EXCLUDE BELOW:::
# remove "camera not functioning" by setting argument exclude = "camera not functioning"
rec.db.species60.exclude <- recordTable(inDir               = wd_images_ID,
                                        IDfrom              = "metadata",
                                        metadataSpeciesTag = "Species",
                                        minDeltaTime        = 10,
                                        deltaTimeComparedTo = "lastRecord",
                                        timeZone            = "Australia/Sydney",
                                        exclude             = "Camera not functioning")

table(rec.db.species60.exclude$Species)

#DO THIS:
# remove duplicates closer than 10 mins apart + "nil" and "Camera not functioning" by setting argument exclude = "nil", "Camera not functioning"
rec.db.species60.exclude2 <-recordTable(inDir               = wd_images_ID,
                            IDfrom              = "metadata",
                                                                    metadataSpeciesTag = "Species",
                                                                    minDeltaTime        = 10,
                                                                    deltaTimeComparedTo = "lastRecord",
                                                                    timeZone            = "Australia/Sydney",
                                                                    exclude             = "nil")

#see what species we recorded:
table(rec.db.species60.exclude$Species)#

#extract camera metadata:
exifTagNames(inDir = wd_images_ID)


#Camera operation matrix: https://jniedballa.github.io/camtrapR/articles/camtrapr3.html 
# first load the camera trap station table
data(camtraps)

dateFormat <- "dmy H"    # requires lubridate package
# alternatively, use "%d/%m/%Y" (from base R)

camop_problem <- cameraOperation(CTtable      = camtraps,
                                 stationCol   = "Station",
                                 setupCol     = "Setup_date",
                                 retrievalCol = "Retrieval_date",
                                 writecsv     = FALSE,
                                 hasProblems  = TRUE,
                                 dateFormat   = dateFormat
)

# as a reminder, these are the dates in our station information table
camtraps[,-which(colnames(camtraps) %in% c("utm_y", "utm_x"))]

# make detection history (with trapping effort)
DetHistEagle <- detectionHistory(recordTable          = rec.db.species60.exclude,
                             camOp                = camop_problem,
                             stationCol           = "Station",
                             speciesCol           = "Species",
                             recordDateTimeCol    = "DateTimeOriginal",
                             species              = "Wedge-tailed eagle",
                             timeZone            = "Australia/Sydney",
                             occasionLength       = 1,
                             day1                 = "2022-05-09",
                             includeEffort        = FALSE,
                             scaleEffort          = FALSE,
                             writecsv             = TRUE,
                             minActiveDaysPerOccasion = 1,
                             outDir = "S:/Carcass_cameras_May_Big_boggy"
)

DetHistEagle[[1]]  # detection history

###make detection histories for each species then save them all as csvs and scale them but the number of days each camera was deployed

##LOOKING AT WHEN CAMERAS WERE OPERATIONAL:
par(mfrow = c(1,1))
camtrapR:::camopPlot(camOp = camop_problem)
camtrapR:::camopPlot(camOp = camop_problem, palette = "Heat")
camtrapR:::camopPlot(camOp = camop_problem, lattice = TRUE)

#MAPS
Mapstest1 <- detectionMaps(CTtable     = camtraps,
                           recordTable  = rec.db.species60.exclude,
                           Xcol         = "utm_x",
                           Ycol         = "utm_y",
                           stationCol   = "Station",
                           speciesCol   = "Species",
                           printLabels  = TRUE,
                           richnessPlot = TRUE,    # by setting this argument TRUE
                           speciesPlots = FALSE,
                           addLegend    = TRUE
)

# subset to 1 species
recordTableSample_PBE <- rec.db.species60.exclude[rec.db.species60.exclude$Species == "Dingo",]

Mapstest2 <- detectionMaps(CTtable      = camtraps,
                           recordTable   = rec.db.species60.exclude,
                           Xcol          = "utm_x",
                           Ycol          = "utm_y",
                           stationCol    = "Station",
                           speciesCol    = "Species",
                           speciesToShow = "Corvid",     # added
                           printLabels   = TRUE,
                           richnessPlot  = FALSE,     # changed
                           speciesPlots  = TRUE,      # changed
                           addLegend     = TRUE
)

#species activity overlap plots:
# define species of interest
speciesA_for_activity <- "Dingo"    # = Viverra tangalunga, Malay Civet
speciesB_for_activity <- "Fox"    # = Prionailurus bengalensis, Leopard Cat

# create activity overlap plot
activityOverlap (recordTable = rec.db.species60.exclude,
                 speciesA    = "Dingo",
                 speciesB    = "Fox",
                 writePNG    = FALSE,
                 plotR       = TRUE,
                 add.rug     = TRUE
)

##REPORT TEST:

reportTest <- surveyReport(recordTable          = rec.db.species60.exclude,
                            CTtable              = camtraps,
                            camOp                = camop_problem,   # new argument since v2.1
                            speciesCol           = "Species",
                            stationCol           = "Station",
                            setupCol             = "Setup_date",
                            retrievalCol         = "Retrieval_date",
                            CTDateFormat         = "dmy H", 
                            recordDateTimeCol    = "DateTimeOriginal",
                            recordDateTimeFormat = "%Y-%m-%d %H:%M:%S", #,
                            CTHasProblems        = TRUE    # deprecated in v2.1
)

reportTest[[1]]
reportTest[[2]]
reportTest[[3]]
reportTest[[5]]


####NOW ONTO THE NMDS!!!!!!

library(readxl)
winter_carcasses <- read_excel("NMDS_data2.fake.xlsx")

library(dplyr)
glimpse(winter_carcasses)

library(vegan)

##need to go back and figure out which distance calculation to use
nmds <- metaMDS(winter_carcasses[1:5], 
                distance = "euclidean",
                trace = FALSE,
                )




nmds

plot(nmds)

#euclidean was the only one that worked

##calculating distance matrix:
occurence_distmat <- vegdist(winter_carcasses, method = "euclidean")
occurence_distmat


occurence_NMS <-
  metaMDS(winter_carcasses,
          distance = "euclidean",
          k = 3,
          maxit = 999, 
          trymax = 500,
          wascores = TRUE)





plot(occurence_NMS)

ordiplot(nmds)

ordiplot (nmds, display = 'species', type = 'p')

veg.data <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/vltava-spe.txt', row.names = 1)

ordiplot (nmds, display = 'sp', type = 'n')
orditorp (nmds, display = 'sp')

ordiplot (nmds, display = 'sp', type = 'n')
points (nmds, col = winter_carcasses.env$Deer, pch = winter_carcasses.env$Deer)

install.packages("vegan3d")
library (vegan3d)
library(readxl)

winter_carcasses.sp_no0 <- read_excel("S:/Carcass_cameras_May_Big_boggy/NMDS_data3.sp.xlsx")

DCA <- decorana (veg = log1p (winter_carcasses.sp_no0))
ordirgl (nmds)

ordiplot(nmds) # generate a canvas based on the `ndms` result
# add all individuals as points and colour by aquaticSiteType: 
points(nmds,
       display = "sites", 
       cex = .6, # change the scale of the points
       pch = 16, # define shape of points
       col = factor(winter_carcasses$Deer)) # colour based on aquaticSiteType
# then, generate ellipses for clearer visualisation
ordiellipse(nmds, 
            groups = winter_carcasses$Deer,
            draw = "polygon",
            conf = 0.95, 
            label = TRUE,
            col = c("black", "red"),
            alpha = 0.2)
# then, plot maccroinvertebrate orders
orditorp(nmds, display = "species", col = "blue")
# stress is required in an NMDS plot:
legend("topleft", "Stress: 0.1383989")

ordiplot (nmds, display = 'si', type = 'n')
ordilabel (nmds, display = 'si')

###Make plots for canopy closure and for deer species
###need to find out why there are only 12 points on the plots

       
#We can further investigate if certain orders have driven the site distribution patterns. We call these intrinsic variables.       
intrinsic <- envfit(nmds, winter_carcasses[2:6])
intrinsic


ordiplot(nmds, type = "n")
plot(intrinsic, 
     col = "red",  
     p.max = 0.01)
ordiellipse(nmds, 
            groups = winter_carcasses$Deer,
            draw = "polygon",
            conf = 0.95, 
            label = TRUE,
            col = c("black", "red"),
            alpha = 0.2)

#plot in ggplot2
plot(nmds)

library(ggplot2)
#extract NMDS scores (x and y coordinates) for sites from newer versions of vegan package

data.scores = as.data.frame(scores(nmds)$sites)
forplot <- data.frame(data.scores, winter_carcasses[c(2, 6)]) # add site information 


ggplot(forplot, aes(NMDS1, NMDS2)) + # plot the 2 axes, then
  # add points for sites
  geom_point(aes(shape = winter_carcasses$Deer, colour = winter_carcasses$Deer), 
             size = 2, 
             alpha =.5) +
  # add 95% confidence interval ellipses
  stat_ellipse(aes(colour = winter_carcasses$Deer, fill = winter_carcasses$Deer), 
               geom = "polygon", 
               type = "t", 
               alpha = .1) +
  theme_bw() +
  # fix legend
  scale_shape_discrete(name = "Site") +
  scale_color_discrete(name = "Site") +
  scale_fill_discrete(name = "Site") +
  geom_blank()
###FIND OUT WHY NOT ALL THE POINTS ARE PLOTTED


##NOW ONTO PERMANOVA:

library(vegan)

str(winter_carcasses)

winter_carcasses.env <- read_excel("NMDS_data2.env.xlsx")
winter_carcasses.sp <- read_excel("NMDS_data2.sp.xlsx")

winter_carcasses.sp_m <- sqrt(winter_carcasses.sp)

dune_dist <- vegdist(winter_carcasses.sp_m, method = "euclidean")
dune_dist

library(factoextra)
fviz_dist(dune_dist)

dispersion <- betadisper(dune_dist, group = winter_carcasses.env$Deer)
dispersion
boxplot(dispersion)
anova(dispersion)
permutest(dispersion)


#performing the PERMANOVA: try for all variables
set.seed(224)
adonis2(winter_carcasses.sp_m ~ Canopy_closed, method = "euclidean", data = winter_carcasses.env, permutations = 9999)

###***significance for canopy closed and deer species

###have to explain why I chose adonis2 above ^

#PAIRWISE COMPARISONS:
# run this in console
install.packages("remotes")
remotes::install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
library(pairwiseAdonis)

pairwise.adonis(dune_dist, winter_carcasses.env$Deer)

?pairwise.adonis()

##don't need to run pairwise comparisons because deer and canopy_closed are both binary variables


###RUNNING AGAIN FOR ASSIGNMENT:
####NOW ONTO THE NMDS!!!!!!

library(readxl)
winter_carcasses <- read_excel("NMDS_data2.fake.xlsx")

library(dplyr)
glimpse(winter_carcasses)

library(vegan)

winter_carcasses_m <- sqrt(winter_carcasses[1:5])
dune_dist <- vegdist(winter_carcasses_m, method = "bray")
dune_dist
library(factoextra)
fviz_dist(dune_dist)


nmds <- metaMDS(winter_carcasses[1:5], 
                distance = "bray",
                trace = FALSE)
nmds

plot(nmds)

?vegdist


ordiplot(nmds, type = "n") # generate a canvas based on the `ndms` result
# add all individuals as points and colour by aquaticSiteType: 

colors <- c("#6495ED", # BLUE
            "#66BD63") # Darker green

points(nmds,
       display = "sites", 
       cex = 1, # change the scale of the points
       pch = 16, # define shape of points
       col = colors[factor(winter_carcasses$Deer)]) # colour based on aquaticSiteType
# then, generate ellipses for clearer visualisation
ordiellipse(nmds, 
            groups = winter_carcasses$Deer,
            draw = "polygon",
            conf = 0.95, 
            label = TRUE,
            col = c("black", "red"),
            alpha = 0.2)
# then, plot maccroinvertebrate orders
orditorp(nmds, display = "sites", col = "black", cex = 0.9, air = 0.5)
orditorp(nmds, display = "species", col = "red", cex = 1.5)

# stress is required in an NMDS plot:
legend("topleft", "Stress: 0.098")

par(mfrow = c(1, 1))

intrinsic <- envfit(nmds, winter_carcasses[1:5])
intrinsic

ordiplot(nmds, type = "n")
plot(intrinsic, 
     col = "red",  
     p.max = 0.05)
ordiellipse(nmds, 
            groups = winter_carcasses$Deer,
            draw = "polygon",
            conf = 0.95, 
            label = TRUE,
            col = c("black", "red"),
            alpha = 0.2)


####PERMANOVA:

winter_carcasses.sp <- read_excel("NMDS_data2.sp.fake.xlsx")
winter_carcasses.env <- read_excel("NMDS_data2.env.fake.xlsx")

winter_carcasses.sp.m <- sqrt(winter_carcasses.sp)

dune_dist <- vegdist(winter_carcasses.sp.m, method = "bray")
dune_dist

par(mfrow = c(1, 2))


dispersion1 <- betadisper(dune_dist, group = winter_carcasses.env$Canopy_closed)
#> Warning in betadisper(dune_dist, group = dune.env$Management): some squared
#> distances are negative and changed to zero
boxplot(dispersion1)

dispersion2 <- betadisper(dune_dist, group = winter_carcasses.env$Deer)
#> Warning in betadisper(dune_dist, group = dune.env$Management): some squared
#> distances are negative and changed to zero
boxplot(dispersion2)

anova(dispersion1)
permutest(dispersion1)

#adonis:
set.seed(224)
adonis2(winter_carcasses.sp.m ~ Deer+Canopy_closed, data = winter_carcasses.env, by="margin", permutations = 9999)

library(pairwiseAdonis)
pairwise.adonis2(winter_carcasses.sp.m ~ Deer+Canopy_closed, data = winter_carcasses.env)
