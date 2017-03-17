convert_def_string<- function(x, y){
x[y != "character"] <- NA
return(x)
}


create_eml <- function(df_p, df_c, df_m){
require(EML)

sp <- read.csv("output/species_code.csv")
meta_p<-read.csv("output/metadata_data_p.csv",  stringsAsFactors = FALSE)
meta_c<-read.csv("output/metadata_data_c.csv",  stringsAsFactors = FALSE)
meta_m<-read.csv("output/metadata_data_m.csv",  stringsAsFactors = FALSE)

## Attributes
attributes_p <- data.frame(
                        attributeName = meta_p$variables,
                        attributeDefinition = meta_p$definition,
                        definition = convert_def_string(meta_p$definition, meta_p$type),
                        unit = meta_p$unit,
                        numberType = gsub("numeric", "real", gsub("character", NA, meta_p$type)),
                        stringsAsFactors = FALSE
                        )


attributes_c <- data.frame(
                        attributeName = meta_c$variables,
                        attributeDefinition = meta_c$definition,
                        definition = convert_def_string(meta_c$definition, meta_c$type),
                        unit = meta_c$unit,
                        numberType = gsub("numeric", "real", gsub("character", NA, meta_c$type)),
                        stringsAsFactors = FALSE
                        )

attributes_m <- data.frame(
                        attributeName = meta_m$variables,
                        attributeDefinition = meta_m$definition,
                        definition = convert_def_string(meta_m$definition, meta_m$type),
                        unit = meta_m$unit,
                        numberType = gsub("numeric", "real", gsub("character", NA, meta_m$type)),
                        stringsAsFactors = FALSE
                        )

attributeList_p<- set_attributes(attributes_p, col_classes = gsub("integer", "numeric", meta_p$type))
attributeList_c<- set_attributes(attributes_c, col_classes = gsub("integer", "numeric", meta_c$type))
attributeList_m<- set_attributes(attributes_m, col_classes = gsub("integer", "numeric", meta_m$type))

physical_p<- set_physical("data_p.csv")
physical_c<- set_physical("data_c.csv")
physical_m<- set_physical("data_m.csv")

dataTable_p<- new("dataTable",
                 entityName = "data_p.csv",
                 entityDescription = "Plot data",
                 physical = physical_p,
                 attributeList = attributeList_p)

dataTable_c<- new("dataTable",
                 entityName = "data_c.csv",
                 entityDescription = "Tree cartography data",
                 physical = physical_c,
                 attributeList = attributeList_c)

dataTable_m<- new("dataTable",
                 entityName = "data_m.csv",
                 entityDescription = "Tree measurement data",
                 physical = physical_c,
                 attributeList = attributeList_c)

## coverage
geographicDescription <- "Plots are located in the northern French Alps. Plot location is either based on measurement with a differential GPS or an estimation of the position on a 1/25000 map. Ecological conditions are very heterogeneous. Climate is under oceanic (high precipitations all year long), continental (low precipitations, high seasonality of the temperature) and mediterranean (high temperatures and low precipitations) influences and can be divided into five climatic sectors: external northern sector with an oceanic climate, external southern sector with an oceanic-mediterranean climate, intermediate northern sector with an oceanic-continental climate, internal sector with a continental climate and intermediate southern sector mixing the influences of the three climates. Geology is also highly variable including calcareous and crystalline rocks, schists, alluvium, and moraines. Plot elevations range from 800 m (lower limit of the montane belt) to 1942 m a.s.l (subalpine belt). Plot slope was derived from a 1/25000 DEM and ranges from 1.5 to 41.9°."


coverage <-
  set_coverage(begin = '1994-01-01', end = '2015-12-31',
               sci_names = paste(as.vector(sp$Latin.name), collapse = " "),
               geographicDescription = geographicDescription,
               westBoundingCoordinate= range(df_p$long)[1],
               eastBoundingCoordinate= range(df_p$long)[2],
               northBoundingCoordinate= range(df_p$lat)[1],
               southBoundingCoordinate= range(df_p$lat)[2],
               altitudeMin = range(df_p$elevation)[1], altitudeMaximum = range(df_p$elevation)[2],
               altitudeUnits = "meter")


## Methods
methods <- set_methods("ms/methods.md")

## PArties
R_person <- as.person("Marc Fuhr <marc.fuhr@irstea.fr>")
fuhr <-as(R_person, "creator")



Irstea_address <- new("address",
                  deliveryPoint = "2 rue de la Papeterie-BP 76",
                  city = "St-Martin-d'Hères",
                  administrativeArea = "",
                  postalCode = "F-38402",
                  country = "France")

publisher <- new("publisher",
                 organizationName = "Univ. Grenoble Alpes, Irstea, UR EMGR",
                 address = Irstea_address)

contact <-
  new("contact",
    individualName = fuhr@individualName,
    electronicMail = fuhr@electronicMailAddress,
    address = Irstea_address,
    organizationName = "Irstea, UR EMGR",
    phone = "000-000-0000")

keys <- new("keywordSet",
        keyword = c("France", "permanent plot", "mountain forest", "tree recruitment", "tree growth",
                    "tree mortality", "tree diameter", "tree height", "tree canopy radius",
                    "tree canopy height", "forest management", "tree spatial coordinates"))

pubDate <- "2017"

title <- "Long-term tree inventory data from mountain forest plots in France"

abstract <- "We present repeated tree measurement data from 63 permanent plots in mountain forests in France. Plot elevations range from 800 (lower limit of the montane belt) to 1942 m a.s.l (subalpine belt). Forests mainly consist of pure or mixed stands dominated by European beech (Fagus sylvatica), Silver fir (Abies alba) and Norway spruce (Picea abies), in association with various broadleaved species at low elevation and with Arolla pine (Pinus cembra) at high elevation. The plot network includes 23 plots in stands that have not been managed for the last 40 years (at least) and 40 plots in plots managed according to an uneven-aged system with single-tree or small-group selection cutting. Plot sizes range from 0.2 ha to 1.9 ha. Plots were installed from 1994 to 2004 and re-measured 2 to 5 times during the 1994-2015 period. During the first census (installation), living trees more than 7.5 cm in dbh were identified, their diameter at breast height (dbh) was measured and their social status (strata) noted. Trees were spatially located, either with x, y and z coordinates (40 plots) or within subplots 0.25 ha square (23 plots). In addition, in a subset of plots (58 plots), tree heights and tree crown dimensions were measured on a subset of trees and dead standing trees and stumps were included in the census. Remeasurements after installation include live tree diameters (including recruited trees), tree status (living, damaged, dead, stump), and for a subset of trees, height. At the time of establishment of the plots, plot densities ranges from 181 to 1328 stemsha-1 and plot basal areas ranges from 13.6 to 81.3 m2ha-1 ."

intellectualRights <- "This data are freely avalaible for non-commercial scientific use (Creative Commons Attribution 3.0 Unported (CC BY 3.0) https://creativecommons.org/licenses/by/3.0/) We would appreciate that researchers contact us for collaborative projects using these data. We encourage researchers to contact us for access to new data that are added annually. This paper must be cited if data in this database are used in publications. We suggest acknowledgement in oral presentations as follows: 'data from Irstea/ONF ALPFORPLOTS supported our work'. Prospective investigators should contact us to inform us of their intent to use the data and provide us with a copy of any publication resulting from their research. Finally, it is imperative that researchers contact IRSTEA or ONF if they want to make their own additional measurements in the field to ensure forest owner authorization. The dataset is released under the Creative Commons Zero."

dataset <- new("dataset",
               title = title,
               creator = fuhr,
               pubDate = pubDate,
               intellectualRights = intellectualRights,
               abstract = abstract,
               keywordSet = keys,
               coverage = coverage,
               contact = contact,
               methods = methods,
               dataTable= list(dataTable_p, dataTable_c, dataTable_m)) #
eml <- new("eml",
           packageId = "5df6f9a49d8534a74e9a1992c663ecfc57d5ebc8",
                                        # git commit of https://github.com/Irstea/ALPFORPLOTS
           system = "git commit of https://github.com/Irstea/ALPFORPLOTS", # type of identifier
           dataset = dataset)

if(!eml_validate(eml)) stop("eml no valide")
write_eml(eml, "output/MetaDataALPFORPLOTS.xml")
}
