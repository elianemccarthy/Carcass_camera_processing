wd_images_ID <- file.path("S:/sdcardstagging/SD_card_delivered_15th_sep_2021/ETDP085_EM")

library(camtrapR)##
library(exiftoolr)
library(dplyr)
library(rgdal)
library(secr)
library(readxl)

rec.db.species60.exclude2 <-recordTable(inDir               = wd_images_ID,
                                        IDfrom              = "metadata",
                                        metadataSpeciesTag = "Species",
                                        minDeltaTime        = 10,
                                        deltaTimeComparedTo = "lastRecord",
                                        timeZone            = "Australia/Sydney",
                                        exclude             = "nil")

head(rec.db.species60.exclude2)

###it works!!!!!!!!!!!!!!!!!!
#so, once I have the harddrive of all camera array tagged data, I read the harddrive into digikam, then open each tag folder, using the 'Tags' tab on the left side of the digikam window, and add a hierarchal tag to it. Then write the new metadata to all images, and read into digikam as normal. 