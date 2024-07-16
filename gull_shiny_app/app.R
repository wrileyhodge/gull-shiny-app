library(shiny)
library(tidyverse)
library(sf)
library(lubridate)
library(shinythemes)
library(shinyWidgets)
library(DT)
library(glue)
library(gganimate)
library(RColorBrewer)
library(leaflet)
library(leaflet.providers)
library(ggnewscale)
library(readxl)
library(patchwork)
library(dplyr)
library(ggspatial)
library(lubridate)
library(rsconnect)
# library(renv)

################### READING IN DATA #####################
gull_sf <- read_sf("data/nest_map/tidy_data/all_gdi_nests")
gdi_outline <- read_sf("data/nest_map/island_sf/outlinecolored")
nests_2023 <- read_sf("data/nest_map/tidy_data/all_nests")
outlines_2023 <- read_sf("data/nest_map/tidy_data/outlines")
hab_2023 <- read_sf("data/nest_map/tidy_data/habitats")

density_2023 <- read_sf("data/nest_map/tidy_data/density_data/all")
density_2023_hergs <- read_sf("data/nest_map/tidy_data/density_data/herg_only")
density_2023_no_dcco <- read_sf("data/nest_map/tidy_data/density_data/all_no_dcco")

all_islands_summary <- read_csv("data/nest_map/tidy_data/all_islands_long.csv")

gps_data_all <- read_csv("data/gps_data/gps_data_all.csv")

#make a list of years
yearlist <- as.list(unique(gull_sf$year))
islandlist <- as.list(unique(nests_2023$island))
specieslist <- as.list(unique(nests_2023$species))

islandlist_sum <- as.list(unique(all_islands_summary$island))
specieslist_sum <- as.list(unique(all_islands_summary$species))

islandlist_den <- as.list(unique(density_2023$island))
specieslist_den <- as.list(unique(density_2023$species))

bandlist <- as.list(unique(gps_data_all$band))

##########################THEMES AND FUNCTIONS#####################
###my theme for graphs, putting here so I dont have all this code in every if else statement....
gen_theme <- theme_bw() +
  theme(axis.title.y = element_text(vjust = 3),
        axis.title.x = element_text(vjust = -1.5),
        plot.title = element_text(hjust = .5),
        plot.margin = margin(.5,.5,1,1, "cm"),
        text = element_text(color = "#284b6b"),
        plot.background = element_rect(fill = "#c7d9e9ff", color = "#284b6b"),
        panel.background = element_rect(fill = "#dfe7f5ff"),
        panel.grid = element_blank(),
        axis.text = element_text(color = "#284b6b"),
        strip.background = element_rect(fill = "#a7bfd5ff"),
        legend.background = element_rect(size = .1, fill = "#dfe7f5ff"),
        legend.box.background = element_rect(fill = "#dfe7f5ff", color = "black"),
        legend.box.margin = margin(.5,.5,.5,.5, "cm"), 
        legend.key = element_rect(fill = "#dfe7f5ff"))

min_theme <- theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.text = element_blank())

min_theme_legend <- theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank())

se <- function(x) sd(x)/sqrt(length(x))

extract_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

legend_graph <- nests_2023 %>%
  rename(Species = species) %>%
  mutate(Species = case_when(Species == "herg" ~ "Herring Gull",
                             Species == "gbbg" ~ "Great Black-backed Gull",
                             Species == "coei" ~ "Common Eider",
                             Species == "dcco" ~ "Double Crested Cormorant")) %>%
  filter(island == "shabby") %>%
  ggplot() + 
  geom_sf(data = (outlines_2023%>%filter(island == "shabby"))) +
  geom_sf(data = (hab_2023%>%filter(island == "shabby") %>% mutate(Habitat = habitat) %>% mutate(Habitat = case_when(Habitat == "berm" ~ "Rocky Shoreline",
                                                                                                                     Habitat == "meadow" ~ "Meadow",
                                                                                                                     Habitat == "vegetation" ~ "Tall Vegetation"))), aes(fill = Habitat)) +
  scale_fill_manual(values = c("#b1c9b1", "#bfbfbf", "#728272")) +
  new_scale_fill() +
  geom_sf(aes(fill = Species), pch = 21, color = "black", size = 2, alpha = .9) +
  scale_fill_manual(values = c("#31B5A7", "#8931B5", "#252A8E", "#8EC0F5")) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.box.background = element_rect(color = "black"))

master_legend<-extract_legend(legend_graph)

############################UI###########################

ui <- fluidPage(theme = shinytheme("flatly"),
                navbarPage(title = "Visualizing Long Term Seabird Data from the Alice Eno Research Station and Acadia National Park"),
                tabsetPanel(tabPanel("Introduction", 
                                     pickerInput("textIn", "Text Displayed", choices =  c("Introduction", "Manuscript: Factors Impacting Nesting Density and Distribution in Herring Gulls"), 
                                                 options = list(`actions-box` = TRUE), multiple = FALSE
                                     ),
                                     mainPanel(htmlOutput(outputId = "Introduction"))),
                            tabPanel("GDI Colony Maps (1999-2023)",
                                     # sidebarLayout(
                                     sidebarPanel(
                                       pickerInput("maptypeIn", "Map Type", choices =  c("Side by Side Map", "Same Map with Different Years"), 
                                                   options = list(`actions-box` = TRUE), multiple = FALSE
                                       ),
                                       pickerInput("yearIn", "Year:", choices = yearlist, options = list(`actions-box` = TRUE),
                                                   multiple = TRUE, selected = 1999)
                                     ),
                                     mainPanel(plotOutput(outputId = "map", width = "100%"))),
                            
                            tabPanel("2023 Colony Maps",
                                     sidebarPanel(
                                       pickerInput("mapgraphIn", "Graph or Map", choices = c("Maps", "Graphs", "Maps and Graphs")),
                                       conditionalPanel(condition = "input.mapgraphIn == 'Maps'",
                                                        pickerInput("maptype23In", "Map Type", choices = c("Single Map", "Multiple Maps"), 
                                                                    options = list(`actions-box` = TRUE)
                                                        ),
                                                        conditionalPanel(condition = "input.maptype23In == 'Single Map'",
                                                                         pickerInput("islandIn", "Island", choices =  islandlist, 
                                                                                     options = list(`actions-box` = TRUE), multiple = FALSE),
                                                                         pickerInput("speciesIn", "Species", choices = specieslist, options = list(`actions-box` = TRUE),
                                                                                     multiple = TRUE, selected = "herg")),
                                                        conditionalPanel(condition = "input.maptype23In == 'Multiple Maps'",
                                                                         pickerInput("island2In", "Island", choices =  islandlist, 
                                                                                     options = list(`actions-box` = TRUE), multiple = TRUE, selected = "shabby"),
                                                                         pickerInput("species2In", "Species", choices = specieslist, options = list(`actions-box` = TRUE),
                                                                                     multiple = TRUE, selected = "herg"))),
                                       conditionalPanel(condition = "input.mapgraphIn == 'Graphs'",
                                                        pickerInput("graphtypeIn", "Graph Type", choices = c("Summary of Total Species Nesting", 
                                                                                                             "Summary of Nesting Density")),
                                                        conditionalPanel(condition = "input.graphtypeIn == 'Summary of Total Species Nesting'",
                                                                         pickerInput("sumIslandIn", "Island", choices = islandlist_sum, 
                                                                                     options = list(`actions-box` = TRUE),
                                                                                     multiple = TRUE,
                                                                                     selected = islandlist_sum),
                                                                         pickerInput("sumSpeciesIn", "Species", choices = specieslist_sum, 
                                                                                     options = list(`actions-box` = TRUE),
                                                                                     multiple = TRUE,
                                                                                     selected = specieslist_sum[1:4]
                                                                         )
                                                                         
                                                        ),
                                                        conditionalPanel(condition = "input.graphtypeIn == 'Summary of Nesting Density'",
                                                                         pickerInput("denIslandIn", "Island", choices = islandlist_den,
                                                                                     options = list(`actions-box` = TRUE),
                                                                                     multiple = TRUE,
                                                                                     selected = "gdi_south"),
                                                                         pickerInput("denSpeciesIn", "Species", choices = specieslist_den,
                                                                                     options = list(`actions-box` = TRUE),
                                                                                     multiple = FALSE,
                                                                                     selected = "herg"),
                                                                         pickerInput("densityIn", "Measure of Density", 
                                                                                     choices = c("Distance to Nearest Neighbor", 
                                                                                                 "Number of nests in a 5 meter radius of each nest"),
                                                                                     options = list(`actions-box` = TRUE),
                                                                                     multiple = FALSE,
                                                                                     selected = "Distance to Nearest Neighbor"),
                                                                         pickerInput("layerIn", "By Habitat or Heterogeneity",
                                                                                     choices = c("Habitat", "Heterogeneity"),
                                                                                     options = list(`actions-box` = TRUE),
                                                                                     multiple = FALSE,
                                                                                     selected = "Habitat")
                                                        )
                                                        
                                       ),
                                       conditionalPanel(condition = "input.mapgraphIn == 'Maps and Graphs'",
                                                        pickerInput("island3In", "Island", choices =  islandlist, 
                                                                    options = list(`actions-box` = TRUE), multiple = FALSE),
                                                        pickerInput("species3In", "Species", choices = specieslist, options = list(`actions-box` = TRUE),
                                                                    multiple = FALSE, selected = "herg"),
                                                        pickerInput("layer3In", "By Habitat or Heterogeneity",
                                                                    choices = c("Habitat", "Heterogeneity"),
                                                                    options = list(`actions-box` = TRUE),
                                                                    multiple = FALSE,
                                                                    selected = "Habitat"),
                                                        pickerInput("density3In", "Measure of Density for Graph", 
                                                                    choices = c("Distance to Nearest Neighbor", 
                                                                                "Number of nests in a 5 meter radius of each nest"),
                                                                    options = list(`actions-box` = TRUE),
                                                                    multiple = FALSE,
                                                                    selected = "Distance to Nearest Neighbor")
                                       )
                                     ),
                                     
                                     mainPanel(plotOutput(outputId = "map2023", width = "100%"))),
                            tabPanel("GPS Tagged Gulls",
                                     sidebarPanel(
                                       pickerInput("bandIn", "Band", choices = bandlist, 
                                                   options = list(`actions-box` = TRUE), multiple = TRUE, selected = bandlist
                                       ),
                                       pickerInput("basemapIn", "Base Map", choices = c("Grey Canvas", "Satellite", "Topographic"), 
                                                   options = list(`actions-box` = TRUE), multiple = FALSE, selected = "Grey Canvas"
                                       ),
                                       dateRangeInput("daterangeIn", "Date Range", start = min(gps_data_all$date), end = max(gps_data_all$date), 
                                                      min = min(gps_data_all$date), max = max(gps_data_all$date), format = "yyyy-mm-dd", separator = " to ")
                                       
                                     ),
                                     mainPanel(leafletOutput(outputId = "gps_map", width = "100%")))
                            
                            
                ))



server <- function(input, output) {
  output$Introduction <- renderText({
    if(input$textIn == "Introduction") {
      glue("<br>Welcome to my Shiny App!", 
           "<br>",
           "<br>",
           "The purpose of this shiny app is to display long term seabird data collected on Great Duck Island, Mount Desert Rock, and seabird islands in Acadia National. The data currently displayed on this app is about nesting density and distribution, and the results of GPS tagged Herring Gulls. Additionally, this app is showing data that is relevant to my undergraduate thesis at College of the Atlantic. If you would like to know more about that, change the text options above, the second tab will give you the introduction, methods, and discussion of a manuscript about nesting density and distribution in Herring Gulls. 
",
           "<br>",
           "<br>",
           "The first tab allows you to look at the nesting distribution of Herring Gulls on the south end of Great Duck Island over the last 25 years. Every year, researchers at the Alice Eno Research Station map every nest using high precision GPS. In this tab, you can either look at individual years next to each other, or you can look at different years mapped on top of eachother. You can toggle between these using the options under ‘Map Type.’ 
",
           "<br>",
           "<br>",
           "The second tab allows you to look at the nesting distribution of Herring Gulls on a Great Duck, Mount Desert Rock, Shabby Island, Schoodic Island, and Heron Island. These islands are abbreviated in the app to gdi (gdi_north refers to the north end colony and gdi_south refers to the south end), mdr, shabby, schoodic, and heron, respectively. The species of interest in this app are Herring Gulls (Larus argentatus) (abbr: herg), Great Black-backed Gulls (Larus marinus) (abbr: gbbg), Common Eiders (Somateria Mollissima) (abbr: coei), and Double Crested Cormorants (Nannopterum auritum) (abbr: dcco). 
",
           "<br>",
           "<br>",
           "This second tab gives you more options than the first. You can look at either single islands, or multiple islands next to each other by changing the ‘Map Type’ option. You can also look at graphical representations of total nesting species and nesting density by changing the ‘Map or Graph’ option to graph. Under ‘Graph’, you will first see a graph showing the total number of birds nesting on different islands by species, however, by changing ‘Graph Type,’ you can look at summaries of nesting density. If you are curious about what the different options mean here, change the text option to the left to look at information about my senior project. In short, you have the ability to change between different landscape classifications and different measures of density. Lastly, if you choose ‘Maps and Graphs,’ you can look at the map of an island alongside a graphical summary of nesting density. 
",
           "<br>",
           "<br>",
           "The final tab is a preliminary interactive map that allows you to look at the movements of 6 Herring Gulls that were fitted with GPS tags during the 2023 field season. You can choose between different birds under the ‘Band’ options, and you can zoom and drag the map using your mouse. You can also input a date range that will filter the displayed data.
",
           "<br>",
           "<br>",
           "<br>")}
    else if (input$textIn == "Manuscript: Factors Impacting Nesting Density and Distribution in Herring Gulls") {
      glue(
        "INTRODUCTION",
        "<br>",
        "<br>",
        "Colonial breeding occurs across a variety of taxa, ranging from fish, mammals, and reptiles, to birds. Colonial breeding is generally defined as a form of group living where individuals nest in dense aggregations, and the sole resource of an individual's territory is the nest site (Rolland et al 1998). Because colonial breeding is relatively widespread amongst birds, (approx. 13% of birds nest in colonies, Lack 1968), birds have been a focal taxa for research on colonial breeding. Considerable research has been dedicated to understanding why, in evolutionary terms, seabirds nest in colonies (see Coulson 1991 or Danchin and Wagner 1997 for review). We focus on a related, but different question: what are the factors determining how birds distribute themselves within a colony? Specifically, we address this question with the case study of Herring Gulls (Larus argentatus) nesting in the Gulf of Maine (GOM).
We approach our research question through the lens of habitat selection. At its root, habitat selection can be explained as individuals distributing themselves between habitats proportionally to the amount of available resources in each habitat (Fretwell and Lucas 1970). Habitat selection must be understood as an individual choice; widespread patterns are the culmination of individual choices (Jones 2001). The ideal free distribution (IFD) (Fretwell and Lucas 1970) states that individuals will originally occupy an optimal habitat (habitat A), but through a density dependent mechanism, the optimal habitat will decrease in quality, until habitat B and habitat A are equal. At this point, individuals will nest in both habitats A and B (Tregenza 1994). This theory expanded to include ‘despotic’ behavior, where fit individuals will exclude less fit individuals from optimal areas; this is dubbed the ideal despotic distribution (IDD). Both theories assume that individuals have full knowledge of habitat quality and the IFD assumes that individuals are free to choose their nest site accordingly. These theories thus have steep practical limitations; nonetheless their basic tenants offer valuable predictions to test data against (Avgar et al 2020). The habitat specific resources have often been understood to directly impact individual fitness (see Chalfoun and Schmidt 2012 for review). But as Jones (2001) discusses, adaptive differences are often assumed without a demonstration of increased fitness.
For colonial seabirds, habitat specific resources may be different because of their unique ecology. Seabirds often nest on offshore islands, are highly gregarious, long lived, and may travel long distances to feed during the breeding season. For passerines, habitat selection and nesting location may directly impact foraging quality / availability; but where a seabird nests within a colony likely has no impact on where they are foraging. If we apply Rolland et al (1998)’s definition of colonial breeding where the only resource is a nest site, the question becomes, what defines a nest site?
Herring gulls may nest in a variety of different habitats within a colony, and nesting density may vary considerably both within and between habitats. Herring Gulls differ from many other colonial seabirds in that a) their colonies are generally less dense (mean inter-nest distance in our study was 4.66 ± .08 meters, standard error), and b) chicks stray from the nest within a few days of hatching.
In this study, we test the predictions of the IFD and IDD with the case study of Herring Gulls. We recorded nest locations of a large sample of nests (n=2755) across 7 colonies in 2023, and analyzed nesting density in relation to habitat qualities. We contextualize this analysis with reproductive success data taken on a smaller sample over two years (n = 53 and n = 102), and site fidelity data; both latter data sets are from one colony (Great Duck Island; GDI). We also conducted behavioral observations on GDI on a sample of 43 nests. At one colony, we have spatial data on nests dating back 25 years, allowing us to further examine the predictions of the IFD and IDD. 
We present an alternative hypothesis. Instead of the only resource in question being the nest site (Rollands et al 1998), we argue that vantage points are an important resource in nest site selection. Further, instead of habitat-specific resources impacting individual fitness, we present a case where resources impacting social behavior may better explain observed differences in nesting density within a colony. 

        ",
        "<br>",
        "<br>",
        "METHODS",
        "<br>",
        "<br>",
        "Data Collection",
        "<br>",
        "Nest locations: Between May and June, 2023, we visited 7 gull colonies on 5 islands in the greater Mount Desert Island area, Maine, USA. A team consisting of three to seven researchers swept the island at arm’s length to identify nests; species ID was determined by experienced researchers. Nests were categorized as Herring Gull (the most abundant species on our study islands) unless we could confidently identify the nest as a Great Black-backed Gull (Larus Marinus). Nests were mapped into a GIS using a Trimble Geoexplorer 3500, which gave us submeter accuracy. We also used a data set collected with the same methods at one colony (GDI) that dates back 25 years. 
",
        "<br>",
        "
Habitat Stratification: Habitats were stratified using a combination of ground delineation and aerial imagery from a DJI Phantom 4 drone. Ground delineation was done with a Trimble Geoexplorer 3500, and aerial images were processed in ArcGIS Pro. We categorized all areas in the colony into two stratifications: habitat type and physical heterogeneity rating. We stratified habitat type into three categories: rocky shoreline, meadow, and tall vegetation. Physical heterogeneity was categorized on a 1-5 scale; this rating was meant to capture the physical variability of an area. At one extreme, flat areas such as homogenous meadow or smooth bedrock were rated 1, while on the other extreme, large piles of granite slabs and cobbles were rated 5.
",
        "<br>",
        "
Site Fidelity: In 2022, we color banded 89 individual Herring Gulls on Great Duck Island. Every band was associated with a GIS mapped nest in 2022; because we captured birds at the nest, we were confident in identifying nesting locations in 2022. In 2023, we attempted to resight all of the banded birds in our study from the lighthouse located in the center of the colony. We had to exclude 23 birds from our sample because we were unable to determine whether or not they returned to GDI in 2023; this was mostly because these nests were far out of sight from any places of observation. We attempted to sample birds across vegetation types; but because we hadn’t considered physical heterogeneity in our 2022 field season, we did not get a representative sample across this stratification, we had to exclude rating 5 from our analysis of site fidelity by habitat stratification.
",
        "<br>",
        "
Fledging Success: Fledging success was recorded on GDI in 2022 and 2023 (n = 53 for 2022, n = 102 for 2023). Fledging success was determined by a combination of daily nest checks, and observations from a lighthouse. We considered an individual fledged once it reached 700 grams, or was seen in complete first year plumage by the end of the field season. For similar reasons to our site fidelity data, we did not get a representative sample of fledging success across our heterogeneity stratification, and had to exclude rating 5 from our analysis.
",
        "<br>",
        "
Behavioral observations: On a sample of 43 nests, we recorded where birds were standing throughout the breeding season into a GIS using the ESRI Field Maps mobile app (ESRI). We used manually georeferenced drone photos (done in ArcGIS Pro using the Georeference Tool) as the base map when recording our data. This allowed us to get fine-grain spatial data on where individual birds were spending time and what they were doing. We categorized each point with a behavior: loafing, incubating, feeding chicks, attending chicks, chick calling, or aggressive behavior (including chasing conspecifics, assuming aggressive postures, grass pulling (Tinbergen 1953)). Sets of between 7 and 13 nests were monitored for hour-long periods and positions of attending parents were recorded every 10 minutes.
",
        "<br>",
        "<br>",
        "Data Analysis",
        "<br>",
        "
Colony Development (1999-2023): Historical nesting data was analyzed using a Generalized Mixed Effects Model (GLMM) using the lme4 (Bates et al 2015) and afex (Singmann et al 2023) R packages. We modeled density as our response with an underlying Poisson distribution. Density was calculated as the number of nests in a 5 meter radius of each nest. This metric of density was calculated in R using the st_buffer and st_join functions in the R package sf (Pebesma and Bivand 2023).  Our fixed effects were habitat type, physical heterogeneity rating, and kernel density estimations from the first year (1999), most recent year (2023), and previous year (year - 1). Year was modeled as a random effect. Kernel density estimates were calculated using the density function of the R package spatstat (Baddeley et al 2015); we used a sigma value of 10 meters. We included kernel density estimations to look at whether different colony states were good predictors of density in any given year. We used the R package partR2 to derive R^2 values and 95% confidence intervals from our GLMM through bootstrapping (sensu Stoffel et al 2020). Historically, only the property belonging to College of the Atlantic has consistently been mapped on GDI. To make our data comparable, we only included data from the area that was sampled every year. This meant we excluded 355 data points from our data set (total n = 10,975). 190 of these excluded data were from 2023 (total 2023 n = 994). We used a multinomial logistic regression with the R package nnet to determine if there were changes in the proportion of birds nesting in each strata across the years (Venables and Ripley 2002). We ran one model where the response was what habitat type each nest was in, and one model where the response was what heterogeneity rating each nest was in. In other words, the response was modeled as a nesting pair’s “choice” about what habitat or what heterogeneity to nest in. Year was our covariate. We used the R package ggeffects to derive the probability of a nest being any given strata for every year (Lüdecke 2018). We then compared both models to the intercept only model using an Anova table in the R package car (Fox and Weisberg 2019). In this case, the intercept only model is the same as using the observed proportion of nests in each strata to predict the probability of where a nest would be.
",
        "<br>",
        "
2023 Nest Data: Nesting density of 2023 data was analyzed using a separate GLMM. Like with the historical model, we modeled density as our response with an underlying Poisson distribution. We included habitat type and physical heterogeneity rating as fixed effects, and colony as a random effect. Like with the historical model, we used the R package partR2 to derive our R^2 values and 95% confidence intervals through bootstrapping.
",
        "<br>",
        "
Fledging Success: We used a Generalized Cochran Mantel Haenszel (CMH) test on our fledging success data to test for differences across both of our habitat stratifications; this was done using the R package coin (Hothorn et al 2008). Herring Gulls have a median clutch size of 3, with each nest fledgling 0, 1, 2, or 3 chicks. Some researchers have treated fledging success as a continuous response, but due to the nature of this data, treating it as ordinal is more appropriate (Agresti 2010).
",
        "<br>",
        "
Behavioral Observations: Our primary interest in our behavioral observations was how gulls were utilizing the habitat within their territory. We were specifically interested in whether the physical features that we summarized in our heterogeneity rating were being used by the gulls. To this end, we used ArcGIS Pro to aggregate all points around an individual nest into a “use-area.” We don’t refer to this as a “territory” because “territory” has specific behavioral and ecological characteristics; the use-area is not necessarily equal to the defended territory. We divided each individual's use-area into two categories: feature or not feature. Features were generally large rocks, though washed up beach trash (mooring balls, lobster crates), driftwood, and other objects were considered features as well. From this, we calculated how many observations were recorded on a feature. Using an Exact Binomial Test, we tested to see if the number of observations of gulls on a feature was proportional to the percentage of their use-area that was considered a feature. We ran one test for each nest (n=43). To reduce Type 1 error in our 43 comparisons, we used the Bonferroni alpha adjustment; we considered results significant if p < .001.
",
        "<br>",
        "
All statistical analysis was carried out using R v 4.2.1 (R Core Development Team 2022)
",
        "<br>",
        "<br>",
        "DISCUSSION",
        "<br>",
        "<br>",
        "
Here, we studied nesting density with a large sample of 2755 nests across 7 colonies in one year, 25 years of data from one colony, and contextualized this data with reproductive success, site fidelity, and behavioral observations from one colony. These data offer a unique opportunity to test the IFD's predictions of nesting density. 
Under the IFD, we would expect birds in the first year to only nest in the optimal habitat, and the probability of nesting in the other habitats should be very low. As the optimal habitat filled up, the quality of the optimal habitat would decrease through a density dependent mechanism until it was equal to the next habitat. At this point, the probability of nesting in each habitat should be proportional to the amount of available resources in the possible habitats; in other words, we should see an increase in the probability of nesting in the second habitat as time goes on. This cycle would repeat until all habitats are occupied. We have two kinds of habitat stratifications (habitat type and heterogeneity rating) to test these predictions against, but we do not see these patterns represented in our data. The probability of birds nesting in different habitat types has not changed over the 25 years of data. Birds have been nesting across different habitat types for the 25 years of data on GDI. Additionally, the areas of highest density have shifted from 1999 to 2023; the area with the highest density in 2023 had no nests in 1999. If this was because of despotic behavior, the IDD would predict lower reproductive success in these other areas; again, we do not see this reflected in our data. We did see a decrease in the probability of birds nesting in heterogeneity rating 4 over the 25 years, the cause of this is unclear. An initial interpretation of this pattern could be that heterogeneity rating 4 is the optimal habitat, but because we don’t see the other patterns expected by the IFD reflected in the other heterogeneity ratings, this is unlikely. We interpret all of this to mean that there is no optimal habitat within the colony on GDI. 
An underlying assumption in the IFD, and habitat selection models in general, is that nest sites are being chosen on the basis of the amount of available resources that help an individual’s fitness (Jones 2001). If nest location impacted individual fitness, we would expect two things: 1) fledging success would vary significantly between habitats, and 2) individuals would exhibit higher site fidelity in optimal habitat. An extension to 2) would be that individuals would move from suboptimal to optimal habitats as they gained experience. Our data suggest that neither of these expectations are met for Herring Gulls nesting in the GOM.
Our data also suggests that the nest site is not the only resource that individuals are distributing themselves by. Both our GLMMs found that physical heterogeneity of an area explained the most variance in nesting density. Physical heterogeneity is defined by the amount of distinct physical features in an area. We found that as physical heterogeneity increased, so did the density of nests. Our behavioral observations show that individuals are using these distinct features around their nest sites. 88% of nests spent a significant amount of time standing on features. 65% of our observations were of a bird on a feature. In 95% of these observations of birds on a feature, the behavior was categorized as “loafing,” which we recorded when the bird was standing or sitting in a relaxed posture (85% of all behavioral observations were “loafing”). We argue that the importance of these features is that they offer a place to loaf. We hypothesize multiple functions of this: 1) features offer a vantage point. By standing on a feature, individuals are sending a visual signal to conspecifics that they are attending their use-area, and individuals are also able to receive these signals from other birds. 2) Features offer a distinct physical feature to delineate an individual's territory; distinct objects are easier to defend, and offer a physical landmark to define a use-area. A flat homogeneous surface does not provide the same possibilities for landmarking. Despite the significant differences in density across the different heterogeneity ratings, we observed no differences in reproductive success or site fidelity; this further supports the hypothesis that the importance of these features may be better explained in the context of social behavior. 
",
        "<br>",
        "<br>",
        "Literature Cited",
        "<br>",
        "<br>",
        "Agresti, A. 2010. Analysis of Ordinal Categorical Data. 2nd Edition, Wiley, Hoboken.
http://dx.doi.org/10.1002/9780470594001
",
        "<br>",
        "<br>",
        "Chalfoun, A. Schmidt K. (2012). Adaptive Breeding-Habitat Selection: Is it for the Birds? The Auk 129(4):589−599",
        "<br>",
        "<br>",
        "Coulson, J . (2001) “Colonial Breeding in Seabirds.” in Burger J. and Schreiber E.A (2001) Biology of Marine Birds, CRC Press, Boca Raton, FL.  pp. 87–115",
        "<br>",
        "<br>",
        "Danchin, E. and R. H. Wagner. (1997). The evolution of coloniality: the emergence of new perspectives. Trends in Ecology and Evolution 12:342-347. https://doi.org/10.1016/S0169-5347 (97)01124-5 ",
        "<br>",
        "<br>",
        "Fretwell, Stephen & Lucas, Henry. (1970). On Territorial Behavior and Other Factors Influencing Habitat Distribution of Birds. Acta Biotheoretica. 19. 16-36. 10.1007/BF01601953.",
        "<br>",
        "<br>",
        "Jones, J. (2001) Habitat Selection Studies in Avian Ecology: A Critical Review. The Auk, vol. 118, no. 2, pp. 557–62. JSTOR, https://doi.org/10.2307/4089822.",
        "<br>",
        "<br>",
        "Lack, D. (1968). Ecological adaptations for breeding in birds. Methuen, London. ",
        "<br>",
        "<br>",
        "Rolland, C., E. Danchin, and M. de Fraipont. (1998). The evolution of coloniality in birds in relation to food, habitat, predation, and life history traits: a comparative analysis. The American Naturalist  151:514-529. https://doi.org/10.1086/286137",
        "<br>",
        "<br>",
        "Tregenza, T. (1994). Common Misconceptions in Applying the Ideal Free Distribution. Animal Behaviour 47: 485–7.",
        "<br>",
        "<br>",
        "Tregenza, T. (1995) Building on the ideal free distribution. Advances in Ecological Research 26. 253-302.",
        "<br>",
        "<br>",
        "Tinbergen, N. (1953). The herring gull's world: a study of the social behavior of birds."
      )
    }
  })
  #####For GDI colony maps in first tab ########
  output$map <- renderPlot({
    if (input$maptypeIn == "Side by Side Map") {
      gull_sf %>%
        filter(year %in% input$yearIn) %>%
        ggplot() +
        geom_sf(data = gdi_outline) +
        geom_sf(data = hab_2023%>%filter(island == "gdi_south"), aes(fill = habitat)) +
        scale_fill_manual(values = c("#bfbfbf", "#b1c9b1", "#728272")) +
        geom_sf() +
        coord_sf(ylim = c(44.140385, 44.145726), xlim = c(-68.25038, -68.242629), expand = FALSE) +
        labs(title = "Distribution of Nesting Gulls on GDI", subtitle = glue('Year: {input$yearIn}')) +
        facet_wrap(~year, ncol = 2)  +
        annotation_scale() +
        gen_theme
    } else if (input$maptypeIn == "Same Map with Different Years") {
      gull_sf %>%
        filter(year %in% input$yearIn) %>%
        ggplot() +
        geom_sf(data = gdi_outline) +
        geom_sf(data = hab_2023%>%filter(island == "gdi_south"), aes(fill = habitat)) +
        scale_fill_manual(values = c("#bfbfbf", "#b1c9b1", "#728272")) +
        geom_sf(aes(color = year), alpha = .6, size = 2) +
        coord_sf(ylim = c(44.140385, 44.145726), xlim = c(-68.25038, -68.242629), expand = FALSE) +
        labs(title = "Distribution of Nesting Gulls on GDI", subtitle = glue('Year: {input$yearIn}')) +
        scale_color_gradient(low = "#ED455D", high = "#710616") +
        annotation_scale() +
        gen_theme
    }  
    else if (input$maptypeIn == "Animated Map") { #I AM GRUMBLY ABOUT WHY THIS ISN"T WORKING AND AM MOVING ON
      gdi_outline <- gdi_outline %>%
        st_transform("+init=epsg:4326")
      gull_sf <- gull_sf %>%
        st_transform("+init=epsg:4326")
      
      animation <- gull_sf %>%
        filter(year %in% input$yearIn) %>%
        ggplot() +
        geom_sf(data = gdi_outline) +
        geom_sf(data = gull_sf, color = "#00264D", aes(group = year), size = 2) +
        coord_sf(ylim = c(44.140385, 44.145726), xlim = c(-68.25038, -68.242629), expand = FALSE) + 
        transition_states(year,
                          transition_length = .1,
                          state_length = 20
        ) +
        labs(
          title = "Distribution of Nesting Gulls", 
          subtitle = 'Year: {closest_state}') +
        theme_bw() +
        theme(axis.text = element_text(size = 15),
              title = element_text(size = 20),
              plot.subtitle = element_text(size = 18))
      
      animate(animation)
    }
  }, height = 600, width = 600) #end panel 1 bracket
  #######For 2023 Colony Maps ########
  output$map2023 <- renderPlot({
    if(input$mapgraphIn == "Maps") {
      if (input$maptype23In == "Single Map") {
        map <- nests_2023 %>%
          filter(island %in% input$islandIn) %>%
          filter(species %in% input$speciesIn) %>%
          ggplot() + 
          geom_sf(data = (outlines_2023%>%filter(island %in% input$islandIn))) +
          geom_sf(data = (hab_2023%>%filter(island %in% input$islandIn)), aes(fill = habitat)) +
          scale_fill_manual(values = c("#bfbfbf", "#b1c9b1", "#728272")) +
          new_scale_fill() +
          geom_sf(aes(fill = cols), pch = 21, color = "black", size = 3, alpha = .9) +
          scale_fill_identity() +
          labs(title = glue('{input$islandIn}')) +
          annotation_scale() +
          min_theme 
        
        (map | master_legend)
      }
      else if (input$maptype23In == "Multiple Maps") {
        if(length(input$island2In) == 1) {
          plot1 <- nests_2023 %>%
            filter(island == input$island2In[1]) %>%
            filter(species %in% input$species2In) %>%
            ggplot() + 
            geom_sf(data = (outlines_2023%>%filter(island == input$island2In[1]))) +
            geom_sf(data = (hab_2023%>%filter(island == input$island2In[1])), aes(fill = habitat)) +
            scale_fill_manual(values = c("#bfbfbf", "#b1c9b1", "#728272")) +
            new_scale_fill() +
            geom_sf(aes(fill = cols), pch = 21, color = "black", size = 3, alpha = .9) +
            scale_fill_identity() +
            labs(title = glue('{input$island2In}')) +
            annotation_scale() +
            min_theme 
          (plot1 | master_legend)
        }
        else if (length(input$island2In) == 2) {
          plot1 <- nests_2023 %>%
            filter(island == input$island2In[1]) %>%
            filter(species %in% input$species2In) %>%
            ggplot() + 
            geom_sf(data = (outlines_2023%>%filter(island == input$island2In[1]))) +
            geom_sf(data = (hab_2023%>%filter(island == input$island2In[1])), aes(fill = habitat)) +
            scale_fill_manual(values = c("#bfbfbf", "#b1c9b1", "#728272")) +
            new_scale_fill() +
            geom_sf(aes(fill = cols), pch = 21, color = "black", size = 3, alpha = .9) +
            scale_fill_identity() +
            labs(title = glue('{input$island2In[1]}')) +
            annotation_scale() +
            min_theme 
          
          plot2 <- nests_2023 %>%
            filter(island == input$island2In[2]) %>%
            filter(species %in% input$species2In) %>%
            ggplot() + 
            geom_sf(data = (outlines_2023%>%filter(island == input$island2In[2]))) +
            geom_sf(data = (hab_2023%>%filter(island == input$island2In[2])), aes(fill = habitat)) +
            scale_fill_manual(values = c("#bfbfbf", "#b1c9b1", "#728272")) +
            new_scale_fill() +
            geom_sf(aes(fill = cols), pch = 21, color = "black", size = 3, alpha = .9) +
            scale_fill_identity() +
            labs(title = glue('{input$island2In[2]}')) +
            annotation_scale() +
            min_theme 
          
          (plot1 | plot2 | master_legend)
        }
        else if (length(input$island2In) == 3) {
          plot1 <- nests_2023 %>%
            filter(island == input$island2In[1]) %>%
            filter(species %in% input$species2In) %>%
            ggplot() + 
            geom_sf(data = (outlines_2023%>%filter(island == input$island2In[1]))) +
            geom_sf(data = (hab_2023%>%filter(island == input$island2In[1])), aes(fill = habitat)) +
            scale_fill_manual(values = c("#bfbfbf", "#b1c9b1", "#728272")) +
            new_scale_fill() +
            geom_sf(aes(fill = cols), pch = 21, color = "black", size = 3, alpha = .9) +
            scale_fill_identity() +
            labs(title = glue('{input$island2In[1]}')) +
            annotation_scale() +
            min_theme 
          
          plot2 <- nests_2023 %>%
            filter(island == input$island2In[2]) %>%
            filter(species %in% input$species2In) %>%
            ggplot() + 
            geom_sf(data = (outlines_2023%>%filter(island == input$island2In[2]))) +
            geom_sf(data = (hab_2023%>%filter(island == input$island2In[2])), aes(fill = habitat)) +
            scale_fill_manual(values = c("#bfbfbf", "#b1c9b1", "#728272")) +
            new_scale_fill() +
            geom_sf(aes(fill = cols), pch = 21, color = "black", size = 3, alpha = .9) +
            scale_fill_identity() +
            labs(title = glue('{input$island2In[2]}')) +
            annotation_scale() +
            min_theme 
          
          plot3 <- nests_2023 %>%
            filter(island == input$island2In[3]) %>%
            filter(species %in% input$species2In) %>%
            ggplot() + 
            geom_sf(data = (outlines_2023%>%filter(island == input$island2In[3]))) +
            geom_sf(data = (hab_2023%>%filter(island == input$island2In[3])), aes(fill = habitat)) +
            scale_fill_manual(values = c("#bfbfbf", "#b1c9b1", "#728272")) +
            new_scale_fill() +
            geom_sf(aes(fill = cols), pch = 21, color = "black", size = 3, alpha = .9) +
            scale_fill_identity() +
            labs(title = glue('{input$island2In[3]}')) +
            annotation_scale() +
            min_theme 
          
          (plot1 | plot2 | plot3 | master_legend)
        }
        else if (length(input$island2In) == 4) {
          plot1 <- nests_2023 %>%
            filter(island == input$island2In[1]) %>%
            filter(species %in% input$species2In) %>%
            ggplot() + 
            geom_sf(data = (outlines_2023%>%filter(island == input$island2In[1]))) +
            geom_sf(data = (hab_2023%>%filter(island == input$island2In[1])), aes(fill = habitat)) +
            scale_fill_manual(values = c("#bfbfbf", "#b1c9b1", "#728272")) +
            new_scale_fill() +
            geom_sf(aes(fill = cols), pch = 21, color = "black", size = 3, alpha = .9) +
            scale_fill_identity() +
            labs(title = glue('{input$island2In[1]}')) +
            annotation_scale() +
            min_theme 
          
          plot2 <- nests_2023 %>%
            filter(island == input$island2In[2]) %>%
            filter(species %in% input$species2In) %>%
            ggplot() + 
            geom_sf(data = (outlines_2023%>%filter(island == input$island2In[2]))) +
            geom_sf(data = (hab_2023%>%filter(island == input$island2In[2])), aes(fill = habitat)) +
            scale_fill_manual(values = c("#bfbfbf", "#b1c9b1", "#728272")) +
            new_scale_fill() +
            geom_sf(aes(fill = cols), pch = 21, color = "black", size = 3, alpha = .9) +
            scale_fill_identity() +
            labs(title = glue('{input$island2In[2]}')) +
            annotation_scale() +
            min_theme 
          
          plot3 <- nests_2023 %>%
            filter(island == input$island2In[3]) %>%
            filter(species %in% input$species2In) %>%
            ggplot() + 
            geom_sf(data = (outlines_2023%>%filter(island == input$island2In[3]))) +
            geom_sf(data = (hab_2023%>%filter(island == input$island2In[3])), aes(fill = habitat)) +
            scale_fill_manual(values = c("#bfbfbf", "#b1c9b1", "#728272")) +
            new_scale_fill() +
            geom_sf(aes(fill = cols), pch = 21, color = "black", size = 3, alpha = .9) +
            scale_fill_identity() +
            labs(title = glue('{input$island2In[3]}')) +
            annotation_scale() +
            min_theme 
          
          plot4 <- nests_2023 %>%
            filter(island == input$island2In[4]) %>%
            filter(species %in% input$species2In) %>%
            ggplot() + 
            geom_sf(data = (outlines_2023%>%filter(island == input$island2In[4]))) +
            geom_sf(data = (hab_2023%>%filter(island == input$island2In[4])), aes(fill = habitat)) +
            scale_fill_manual(values = c("#bfbfbf", "#b1c9b1", "#728272")) +
            new_scale_fill() +
            geom_sf(aes(fill = cols), pch = 21, color = "black", size = 3, alpha = .9) +
            scale_fill_identity() +
            labs(title = glue('{input$island2In[4]}')) +
            annotation_scale() +
            min_theme 
          
          ((plot1 | plot2) / (plot3 | plot4) | master_legend)
        }
        else if (length(input$island2In) == 5) {
          plot1 <- nests_2023 %>%
            filter(island == input$island2In[1]) %>%
            filter(species %in% input$species2In) %>%
            ggplot() + 
            geom_sf(data = (outlines_2023%>%filter(island == input$island2In[1]))) +
            geom_sf(data = (hab_2023%>%filter(island == input$island2In[1])), aes(fill = habitat)) +
            scale_fill_manual(values = c("#bfbfbf", "#b1c9b1", "#728272")) +
            new_scale_fill() +
            geom_sf(aes(fill = cols), pch = 21, color = "black", size = 3, alpha = .9) +
            scale_fill_identity() +
            labs(title = glue('{input$island2In[1]}')) +
            annotation_scale() +
            min_theme 
          
          plot2 <- nests_2023 %>%
            filter(island == input$island2In[2]) %>%
            filter(species %in% input$species2In) %>%
            ggplot() + 
            geom_sf(data = (outlines_2023%>%filter(island == input$island2In[2]))) +
            geom_sf(data = (hab_2023%>%filter(island == input$island2In[2])), aes(fill = habitat)) +
            scale_fill_manual(values = c("#bfbfbf", "#b1c9b1", "#728272")) +
            new_scale_fill() +
            geom_sf(aes(fill = cols), pch = 21, color = "black", size = 3, alpha = .9) +
            scale_fill_identity() +
            labs(title = glue('{input$island2In[2]}')) +
            annotation_scale() +
            min_theme 
          
          plot3 <- nests_2023 %>%
            filter(island == input$island2In[3]) %>%
            filter(species %in% input$species2In) %>%
            ggplot() + 
            geom_sf(data = (outlines_2023%>%filter(island == input$island2In[3]))) +
            geom_sf(data = (hab_2023%>%filter(island == input$island2In[3])), aes(fill = habitat)) +
            scale_fill_manual(values = c("#bfbfbf", "#b1c9b1", "#728272")) +
            new_scale_fill() +
            geom_sf(aes(fill = cols), pch = 21, color = "black", size = 3, alpha = .9) +
            scale_fill_identity() +
            labs(title = glue('{input$island2In[3]}')) +
            annotation_scale() +
            min_theme 
          
          plot4 <- nests_2023 %>%
            filter(island == input$island2In[4]) %>%
            filter(species %in% input$species2In) %>%
            ggplot() + 
            geom_sf(data = (outlines_2023%>%filter(island == input$island2In[4]))) +
            geom_sf(data = (hab_2023%>%filter(island == input$island2In[4])), aes(fill = habitat)) +
            scale_fill_manual(values = c("#bfbfbf", "#b1c9b1", "#728272")) +
            new_scale_fill() +
            geom_sf(aes(fill = cols), pch = 21, color = "black", size = 3, alpha = .9) +
            scale_fill_identity() +
            labs(title = glue('{input$island2In[4]}')) +
            annotation_scale() +
            min_theme 
          
          plot5 <- nests_2023 %>%
            filter(island == input$island2In[5]) %>%
            filter(species %in% input$species2In) %>%
            ggplot() + 
            geom_sf(data = (outlines_2023%>%filter(island == input$island2In[5]))) +
            geom_sf(data = (hab_2023%>%filter(island == input$island2In[5])), aes(fill = habitat)) +
            scale_fill_manual(values = c("#bfbfbf", "#b1c9b1", "#728272")) +
            new_scale_fill() +
            geom_sf(aes(fill = cols), pch = 21, color = "black", size = 3, alpha = .9) +
            scale_fill_identity() +
            labs(title = glue('{input$island2In[5]}')) +
            annotation_scale() +
            min_theme 
          
          (((plot1 | plot2 | plot3) / (plot4 | plot5)) | master_legend)
        }
      } 
    }
    else if (input$mapgraphIn == "Graphs") {
      if(input$graphtypeIn == "Summary of Total Species Nesting") {
        all_islands_summary %>%
          filter(year == 2023) %>%
          filter(species %in% input$sumSpeciesIn,
                 island %in% input$sumIslandIn) %>%
          filter(count != 0) %>%
          ggplot(aes(y = count, x = island, fill = species)) +
          geom_col(position = "dodge", color = "black") +
          geom_text(aes(label = count, group = species), position = position_dodge(width = .8), hjust = -.5, vjust =-.1, size = 3) +
          scale_fill_viridis_d() +
          labs(x = "Island",
               y = "Number of Nests",
               title = "Number of Nesting Birds",
               subtitle = "By Species and Island") +
          coord_flip() +
          gen_theme
      }
      else if(input$graphtypeIn == "Summary of Nesting Density") {
        if(input$layerIn == "Habitat") {hab_values = c("#bfbfbf", "#b1c9b1", "#728272")}
        else {hab_values = c("#E3C5E5", "#C8A3CB", "#AA76AF", "#935999", "#6C3870")}
        density_2023 %>%
          mutate(Habitat = habitat,
                 Heterogeneity = rating,
                 Island = island) %>%
          relocate(n, .after = dist_f) %>%
          pivot_longer(dist_f:n, names_to = "density_type", values_to = "density") %>%
          mutate(density_type = case_when(density_type == "dist_f" ~ "Distance to Nearest Neighbor",
                                          density_type == "n" ~ "Number of nests in a 5 meter radius of each nest")) %>%
          filter(density_type == input$densityIn) %>%
          relocate(Habitat, .after = Heterogeneity) %>%
          pivot_longer(Heterogeneity:Habitat, names_to = "layer_type", values_to = "layer") %>%
          filter(layer_type %in% input$layerIn) %>%
          filter(!is.na(layer)) %>%
          filter(Island %in% input$denIslandIn,
                 species %in% input$denSpeciesIn) %>%
          group_by(layer, Island) %>%
          summarise(mean_count = mean(density), se = se(density)) %>%
          ggplot() +
          geom_col(aes(y = mean_count, x = layer, fill = layer)) +
          geom_errorbar(aes(x = layer, ymin = (mean_count - se), ymax = (mean_count + se), group = layer), size = 0.5, width = 0.5) +
          scale_fill_manual(values = hab_values) +
          facet_wrap(~Island) +
          labs(title = glue('Nesting Density of {input$denSpeciesIn}s'),
               subtitle = glue('by {input$densityIn} and {input$layerIn}'),
               caption = "Error Bars represent +_ 1 Standard Error",
               y = glue('{input$densityIn}'),
               x = glue('{input$layerIn}')) +
          gen_theme
        
      }
    }
    else if (input$mapgraphIn == "Maps and Graphs") {
      if(length(input$island3In) == 1) {
        if(input$layer3In == "Habitat") {hab_values = c("#bfbfbf", "#b1c9b1", "#728272")}
        else {hab_values = c("#E3C5E5", "#C8A3CB", "#AA76AF", "#935999", "#6C3870")}
        
        hab_2023_1 <- hab_2023 %>%
          filter(island == input$island3In[1])%>%
          mutate(Habitat = habitat,
                 Heterogeneity = rating) %>%
          relocate(Habitat, .after = Heterogeneity) %>%
          pivot_longer(Heterogeneity:Habitat, names_to = "layer_type", values_to = "layer") %>%
          filter(layer_type %in% input$layer3In)
        
        map1 <- nests_2023 %>%
          filter(island %in% input$island3In) %>%
          filter(species %in% input$species3In) %>%
          ggplot() + 
          geom_sf(data = (outlines_2023%>%filter(island == input$island3In))) +
          geom_sf(data = (hab_2023_1), aes(fill = layer)) +
          scale_fill_manual(values = hab_values) +
          new_scale_fill() +
          geom_sf(aes(fill = cols), pch = 21, color = "black", size = 3, alpha = .9) +
          scale_fill_identity() +
          labs(title = glue('{input$island3In[1]}')) +
          annotation_scale() +
          min_theme 
        
        plot1 <-  density_2023 %>%
          mutate(Habitat = habitat,
                 Heterogeneity = rating,
                 Island = island) %>%
          relocate(n, .after = dist_f) %>%
          pivot_longer(dist_f:n, names_to = "density_type", values_to = "density") %>%
          mutate(density_type = case_when(density_type == "dist_f" ~ "Distance to Nearest Neighbor",
                                          density_type == "n" ~ "Number of nests in a 5 meter radius of each nest")) %>%
          filter(density_type == input$density3In) %>%
          relocate(Habitat, .after = Heterogeneity) %>%
          pivot_longer(Heterogeneity:Habitat, names_to = "layer_type", values_to = "layer") %>%
          filter(layer_type %in% input$layer3In) %>%
          filter(!is.na(layer)) %>%
          filter(Island %in% input$island3In,
                 species %in% input$species3In) %>%
          group_by(layer, Island) %>%
          summarise(mean_count = mean(density), se = se(density)) %>%
          ggplot() +
          geom_col(aes(y = mean_count, x = layer, fill = layer)) +
          geom_errorbar(aes(x = layer, ymin = (mean_count - se), ymax = (mean_count + se), group = layer), size = 0.5, width = 0.5) +
          scale_fill_manual(values = hab_values) +
          facet_wrap(~Island) +
          labs(title = glue('Nesting Density of {input$species3In}s'),
               subtitle = glue('by {input$densityIn} and {input$layer3In}'),
               caption = "Error Bars represent +_ 1 Standard Error",
               y = glue('{input$density3In}'),
               x = glue('{input$layer3In}')) +
          gen_theme
        
        
        (map1 | master_legend | plot1)
      }
      
      else if(length(input$island3In) == 2) {}
      else if(length(input$island3In) == 3) {}
      
    }
  }, height = 600, width = 900)
  
  #######For GPS TAGS ########   
  
  output$gps_map <- renderLeaflet({
    if(input$basemapIn == "Grey Canvas") {
      
      data_for_map <- gps_data_all %>%
        filter(band %in% input$bandIn) %>%
        filter(date >= input$daterangeIn[1]) %>%
        filter(date <= input$daterangeIn[2]) %>%
        st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
        group_by(band, color) %>%
        dplyr::summarize(do_union=FALSE) %>%
        st_cast("LINESTRING")
      
      leaflet(data = data_for_map) %>%
        addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
        setView(lng = -68.2,
                lat = 44.38,
                zoom = 8) %>%
        addPolylines(color = data_for_map$color, weight = 1) %>%
        addLegend(
          position = "bottomright",
          colors = as.vector(data_for_map$color),
          labels = as.vector(data_for_map$band),
          title = "Birds Shown",
          opacity = 1) }
    else if (input$basemapIn == "Satellite"){
      
      data_for_map <- gps_data_all %>%
        filter(band %in% input$bandIn) %>%
        filter(date >= input$daterangeIn[1]) %>%
        filter(date <= input$daterangeIn[2]) %>%
        st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
        group_by(band, color) %>%
        dplyr::summarize(do_union=FALSE) %>%
        st_cast("LINESTRING")
      
      leaflet(data = data_for_map) %>%
        addProviderTiles(providers$Esri.WorldImagery) %>%
        setView(lng = -68.2,
                lat = 44.38,
                zoom = 8) %>%
        addPolylines(color = data_for_map$color, weight = 1) %>%
        addLegend(
          position = "bottomright",
          colors = as.vector(data_for_map$color),
          labels = as.vector(data_for_map$band),
          title = "Birds Shown",
          opacity = 1)
    }
    else if (input$basemapIn == "Topographic"){
      
      data_for_map <- gps_data_all %>%
        filter(band %in% input$bandIn) %>%
        filter(date >= input$daterangeIn[1]) %>%
        filter(date <= input$daterangeIn[2]) %>%
        st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
        group_by(band, color) %>%
        dplyr::summarize(do_union=FALSE) %>%
        st_cast("LINESTRING")
      
      leaflet(data = data_for_map) %>%
        addProviderTiles(providers$Esri.WorldTopoMap) %>%
        setView(lng = -68.2,
                lat = 44.38,
                zoom = 8) %>%
        addPolylines(color = data_for_map$color, weight = 1) %>%
        addLegend(
          position = "bottomright",
          colors = as.vector(data_for_map$color),
          labels = as.vector(data_for_map$band),
          title = "Birds Shown",
          opacity = 1)
    }
  })
  
} #end server bracket


# Run the application 
shinyApp(ui = ui, server = server)