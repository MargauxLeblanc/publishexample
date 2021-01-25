library(tidyverse)
library(tmap)
library(geojsonio)
library(plotly)
library(rgdal)
library(mapview)
library(sf)
library(sp)
library(car)
library(fs)
library(janitor)
library(dplyr)
library(data.table)
library(spdep)
library(spatstat)
library(here)
library(sp)
library(rgeos)
library(maptools)
library(GISTools)
library(geojson)
library(tmaptools)
library(ggplot2)
library(spatialreg)
library(broom)
library(br)
library(spgwr)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(ggpubr)


# load spatial data 
Londonwards <-  st_read( "data/statistical-gis-boundaries-london/ESRI/London_Ward_CityMerged.shp")

qtm(Londonwards)

# set projection
londonwards_BNG <- st_transform(Londonwards, 27700)

# Load Well-being data
WBscore1 <- read.csv("data/london-ward-well-being-probability-scores/well-being_dashboad_scores_allweight1.csv", sep=";")

#merge boundaries and data
LonWard_WB_score <- londonwards_BNG%>%
  left_join(.,
            WBscore1, 
            by = c("GSS_CODE" = "Ward.Code"))

rm(WBscore1)

LonWard_WB_score <- rename(LonWard_WB_score, WB_score_2013 = X2013)


### EDA 
# check the current class and CRS 
class(LonWard_WB_score)
st_crs(LonWard_WB_score)

# plot score WB (equal weights) in wards
# interactive 
 tmap_mode("view")
 tm_shape(LonWard_WB_score) +
   tm_polygons( "WB_score_2013", style="jenks", popup.vars=c("NAME", "WB_score_2013"),  title="Well-being Score (weight= 1)") +
   tm_layout (legend.outside = TRUE)
 
# report document format
 tmap_mode("plot")
tm_shape(LonWard_WB_score) + 
  tm_fill("WB_score_2013", style = "jenks", palette = "RdYlGn", title = "Well-being Score") +
  tm_borders(alpha = 0.1) +
  tm_scale_bar(position=c(0.02,0.01), text.size=0.5)+
  tm_compass(north=0, position=c(0.02,0.10), text.size = 0.6)+
  tm_layout(main.title = "Well-Being score distribution across London wards", main.title.size = 1 , 
            legend.outside =  T,legend.outside.position =  "right", legend.title.size = 0.8)

# summary
summary(LonWard_WB_score$WB_score_2013)


# histogram
# not exactly normally distributed, some random low counts toward the median. Plus some residuals. 
ggplot(LonWard_WB_score, aes(x=WB_score_2013)) +
  geom_histogram(binwidth=.5, colour='black', fill='white')

# boxplot
ggplot(LonWard_WB_score, aes(x=WB_score_2013))+
  geom_boxplot()


### Let's look at distribution of WB in London 

# calculate the centroids of all Wards in London
coordsW <- LonWard_WB_score%>%
  st_centroid()%>%
  st_geometry()

plot(coordsW,axes=TRUE)

#create a neighbours list
LWard_nb <- LonWard_WB_score %>% 
  poly2nb(., queen=T)

#plot them
plot(LWard_nb, st_geometry(coordsW), col="red")
#add a map underneath
plot(LonWard_WB_score$geometry, add=T)

#creating a spatial weights object from these weights
Lward.lw <- LWard_nb %>%
  nb2listw(., style="C")

head(Lward.lw$neighbours)


# Moran's I 
I_LWard_Global_WB <- LonWard_WB_score %>%
  pull(WB_score_2013) %>%
  as.vector()%>%
  moran.test(., Lward.lw)

I_LWard_Global_WB
# results
# Moran I statistic       Expectation          Variance 
#      0.594993366        -0.001602564         0.000538256 


# Geary’s C : This tells us whether similar values or dissimilar values are clustering
C_LWard_Global_WB <- 
  LonWard_WB_score %>%
  pull(WB_score_2013) %>%
  as.vector()%>%
  geary.test(., Lward.lw)

C_LWard_Global_WB
# results
# Geary C statistic       Expectation          Variance 
#   0.4088523229         1.0000000000         0.0006712584 


#Local Moran's I 
#use the local moran function to generate I for each ward in the city
I_LWard_Local_WB <- LonWard_WB_score %>%
  pull(WB_score_2013) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()

#what does the output (the localMoran object) look like?
slice_head(I_LWard_Local_WB, n=5)

# copy Local Moran's I results (the I score (column 1) and the z-score standard deviation (column 4))
# back into well-being score df
LonWard_WB_score <- LonWard_WB_score %>%
  mutate(density_I =as.numeric(I_LWard_Local_WB$Ii))%>%
  mutate(density_Iz =as.numeric(I_LWard_Local_WB$Z.Ii))

# plot a map of the local Moran’s I outputs
summary(LonWard_WB_score$density_I)
summary(LonWard_WB_score$density_Iz)


ggplot(LonWard_WB_score, aes(x=density_I)) +
  geom_histogram(binwidth=.5, colour='black', fill='white')

ggplot(LonWard_WB_score, aes(x=density_Iz)) +
  geom_histogram(binwidth=.5, colour='black', fill='white')

breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)

MoranColours<- rev(brewer.pal(8, "RdGy"))

# interactive map
tmap_mode("view")
tm_shape(LonWard_WB_score) +
  tm_polygons ("density_Iz", style="fixed", breaks=breaks1, palette=MoranColours, popup.vars=c("NAME", "WB_score_2013"),  title="Local Moran's I, Well-Being") +
  tm_layout (legend.outside = TRUE)

# map for report 
tmap_mode("plot") 
tm_shape(LonWard_WB_score) +
  tm_polygons("density_Iz",
              style="fixed",  breaks=breaks1,
              palette=MoranColours,  midpoint=NA,
              title="Local Moran's I, Well-Being")+
  tm_scale_bar(position=c(0.02,0.01), text.size=0.5)+
  tm_compass(north=0, position=c(0.02,0.10), text.size = 0.6)+
  tm_layout(main.title = "Local Moran's I, Well-Being Score in London", main.title.size = 1, legend.outside =  T,legend.outside.position =  "right")


# Getis Ord G∗i statisic for hot and cold spots:
Gi_LWard_Local_WB <- LonWard_WB_score %>%
  pull(WB_score_2013) %>%
  as.vector()%>%
  localG(., Lward.lw)

head(Gi_LWard_Local_WB)

LonWard_WB_score <- LonWard_WB_score %>%
  mutate(density_G = as.numeric(Gi_LWard_Local_WB))

# plot Getis Ord on an interactive map
tmap_mode("view")
tm_shape(LonWard_WB_score) +
  tm_polygons ("density_G", style="fixed", breaks=breaks1, palette=GIColours, popup.vars=c("NAME", "WB_score_2013", "density_G"),  title="Getis Ord Gi, Well-Being Score in London") +
  tm_layout (legend.outside = TRUE)


# Getis ord map report version
tmap_mode("plot")

GIColours<- rev(brewer.pal(8, "RdBu"))

tm_shape(LonWard_WB_score) +
  tm_polygons("density_G",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi*, Well-Being") +
  tm_scale_bar(position=c(0.02,0.01), text.size=0.5)+
  tm_compass(north=0, position=c(0.02,0.10), text.size = 0.6)+
  tm_layout(main.title = "Getis Ord Gi, Well-Being Score in London", main.title.size = 1, legend.outside =  T,legend.outside.position =  "right" )


### Load and clean Independent variable files

## household income
income <- read.csv("data/IV_Income_household/income_nospace.csv", sep=";" )

income <- clean_names(income)
names(income)
#  remove useless columns 
income_select <- dplyr::select(income, -c("ward_name",  "lad_code" ,  "borough"  ,  "x2001_02" ,  "x2002_03"  , "x2003_04"  ,
                                          "x2004_05" ,  "x2005_06",   "x2006_07" ,  "x2007_08"  , "x2008_09" ,  "x2009_10" ,  "x2010_11"  , "x2011_12" , "x2001_02_1", "x" ,  "x2002_03_1" ,"x2003_04_1" ,"x2004_05_1" ,"x2005_06_1",
                                          "x2006_07_1" , "x2007_08_1" , "x2008_09_1" , "x2009_10_1" , "x2010_11_1" , "x2011_12_1"))

rm(income)
income_select <- rename(income_select, GSS_CODE = i_code)
income_select <- rename(income_select, inc_hh_mean = x2012_13)
income_select <- rename(income_select, inc_hh_median = x2012_13_1)

## population density
populationdens <- read.csv("data/housing-density-ward.csv" )

#Filter out only the 2013 data
popdens_year <- dplyr::filter(populationdens, Year == "2013")
rm(populationdens)

popdens13_select <- dplyr::select(popdens_year, -c("Borough" , "Ward_Name" ,"Year"  ,"Hectares"  , "Square_Kilometres" , "Population_per_hectare" ))
popdens13_select <- rename(popdens13_select, GSS_CODE = Code)
rm(popdens_year)

## Inspect independent variables 
# summary income 
head(income_select)
summary(income_select$inc_hh_median)

# boxplot income
ggplot(income_select, aes(x=inc_hh_median))+
  geom_boxplot()

# histogram income
ggplot(income_select, aes(x=inc_hh_median)) +
  geom_histogram(binwidth=1000, colour='black', fill='white')
# not exactly normally distributed, some outliers high values counts 

# summary population density
head(popdens13_select)
summary(popdens13_select$Population_per_square_kilometre)

# boxplot population density
ggplot(popdens13_select, aes(x=Population_per_square_kilometre))+
  geom_boxplot()

# histogram income population density
ggplot(popdens13_select, aes(x=Population_per_square_kilometre)) +
  geom_histogram(binwidth=500, colour='black', fill='white')
# same, bit skewed, some outliers  

## join &  drop row one value missing from pop density  
LonWard_WB_score_IV <- dplyr::left_join(LonWard_WB_score,income_select,by=c("GSS_CODE"))
LonWard_WB_score_IV <- dplyr::left_join(LonWard_WB_score_IV,popdens13_select,by=c("GSS_CODE")) %>%
  drop_na()


### transform income / 1000 =   household income in thousands ? 
LonWard_WB_score_IV$inc_hh_median  <- (LonWard_WB_score_IV$inc_hh_median / 1000)

### transform pop density / 1000 = 1000 ppl per km? 
LonWard_WB_score_IV$Population_per_square_kilometre  <- (LonWard_WB_score_IV$Population_per_square_kilometre / 1000)


##### LM assumptions check
#  assumption 1 = There is a linear relationship between the dependent and independent variables

#linear regression  WB & household income median &

q <- qplot(x = `inc_hh_median`, 
           y = `WB_score_2013`, 
           data=LonWard_WB_score_IV)

q + stat_smooth(method="lm", se=FALSE, size=1) + 
  geom_jitter()

-20 + (0.0006*70000) + 0

ggplot(LonWard_WB_score_IV,aes(x=inc_hh_median,y=WB_score_2013))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(x = "Median Household Income (£)", y = "Well-Being Score 2013")+
  scale_x_continuous(breaks = seq(20000,90000,10000),limits = c(20000, 90000))+
  theme_bw()+
  stat_cor(method="pearson",size=3)



#linear regression  WB & population density 

q2 <- qplot(x = `Population_per_square_kilometre`, 
            y = `WB_score_2013`, 
            data=LonWard_WB_score_IV)

q2 + stat_smooth(method="lm", se=FALSE, size=1) + 
  geom_jitter()


ggplot(LonWard_WB_score_IV,aes(x=Population_per_square_kilometre,y=WB_score_2013))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(x = "Population per Km²", y = " Well-Being Score 2013")+
  theme_bw()+
  stat_cor(method="pearson", label.x = 600 ,size=3)


#let's check the distribution of these variables first

ggplot(LonWard_WB_score_IV, aes(x=WB_score_2013)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 1) + 
  geom_density(colour="red", 
               size=1, 
               adjust=1)

ggplot(LonWard_WB_score_IV, aes(x=inc_hh_median)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 1000) + 
  geom_density(colour="red",
               size=1, 
               adjust=1)

ggplot(LonWard_WB_score_IV, aes(x=Population_per_square_kilometre)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 1000) + 
  geom_density(colour="red",
               size=1, 
               adjust=1)

##  distrib  median household income
ggplot(LonWard_WB_score_IV, aes(inc_hh_median)) + 
  geom_histogram()

#  log of the household income median 
ggplot(LonWard_WB_score_IV, aes(x=log(inc_hh_median))) + 
  geom_histogram()

# root transfor
ggplot(LonWard_WB_score_IV, aes(x=(inc_hh_median)^(1/3))) + 
  geom_histogram() 

# plot 
qplot(x = (inc_hh_median)^(1/3), 
      y = WB_score_2013,
      data=LonWard_WB_score_IV)

# Compare this with the logged transformation:
qplot(x = log(inc_hh_median), 
      y = WB_score_2013, 
      data=LonWard_WB_score_IV)

##  distrib  pop density 
ggplot(LonWard_WB_score_IV, aes(Population_per_square_kilometre)) + 
  geom_histogram()

# raising our pop density  variable to cube root power leads to a more normal distribution:
ggplot(LonWard_WB_score_IV, aes(x=(Population_per_square_kilometre)^(1/3))) + 
  geom_histogram()  

# plot 
qplot(x = (Population_per_square_kilometre)^(1/3), 
      y = WB_score_2013,
      data=LonWard_WB_score_IV)

## Assumption 2 = The residuals -> normal distribution

#run the linear regression model with household income & pop density 
# and storing outputs in an object called model2

Regressiondata <- LonWard_WB_score_IV%>%
  dplyr::select(WB_score_2013,  inc_hh_median, Population_per_square_kilometre )

# OG model 
#model2 <- lm(WB_score_2013 ~ I(inc_hh_median^(-1)) + 
 #             I(Population_per_square_kilometre^(0.5)), data = Regressiondata)


model2 <- lm(WB_score_2013 ~ I(inc_hh_median^(1/3)) + 
               I(Population_per_square_kilometre^(1/3)), data = Regressiondata)

#show the summary of those outputs
tidy(model2)
glance(model2)
summary(model2)
tab_model(model2)

tab_model(model2, file = "LMtableTrans.html" , pred.labels = c("(Intercept)", "Median Household Income, Power 1/3", "Population density, Power 1/3"  ))
#file = "test.html"

#and for future use, write the residuals out
model_data2 <- model2 %>%
  augment(., Regressiondata)

# also add the residuals to the shapelayer
LonWard_WB_score_IV <- LonWard_WB_score_IV %>%
  mutate(model2resids = residuals(model2))

#plot residuals
model_data2%>%
  dplyr::select(.resid)%>%
  pull()%>%
  qplot()+ 
  geom_histogram() 
#  good   

### Assumption 3 - Multicolinearity

vif(model2)
# all good 

## 4 = Homoscedaticity 

#print some model diagnostics. 
par(mfrow=c(2,2))    #plot to 2 by 2 array
plot(model2)


## 5 =indep of error 

#run durbin-watson test
DW <- durbinWatsonTest(model2)
tidy(DW)

#now plot the residuals (of lm model)
# get interactive output 

tmap_mode("view")
tm_shape(LonWard_WB_score_IV) +
  tm_polygons ("model2resids", palette="-RdYlBu", popup.vars=c("NAME", "WB_score_2013", "model2resids"),  title="Linear Model Residuals") +
  tm_layout (legend.outside = TRUE)

# REPORT 
tmap_mode("plot")
tm_shape(LonWard_WB_score_IV) +
  tm_polygons("model2resids",
              palette="-RdYlBu",
              midpoint=NA,
              title="LM Residuals") +
  tm_scale_bar(position=c(0.02,0.01), text.size=0.5)+
  tm_compass(north=0, position=c(0.02,0.10), text.size = 0.6)+
  tm_layout(main.title = "Linear Model residuals", main.title.size = 1, legend.outside =  T,legend.outside.position =  "right" )

# residuals' moran's I

# centroids for 624 wards 
coordsW <- LonWard_WB_score_IV%>%
  st_centroid()%>%
  st_geometry()

# queen case weight matrix
LWard_nb <- LonWard_WB_score_IV %>%
  poly2nb(., queen=T)

#or nearest neighbours
knn_wards <-coordsW %>%
  knearneigh(., k=4)

LWard_knn <- knn_wards %>%
  knn2nb()

#plot them
par(mfrow=c(1,1))
plot(LWard_nb, st_geometry(coordsW), col="red")
plot(LWard_knn, st_geometry(coordsW), col="blue")

#create a spatial weights matrix object from these weights

Lward.queens_weight <- LWard_nb %>%
  nb2listw(., style="C")

Lward.knn_4_weight <- LWard_knn %>%
  nb2listw(., style="C")

Queen <- LonWard_WB_score_IV %>%
  st_drop_geometry()%>%
  dplyr::select(model2resids)%>%
  pull()%>%
  moran.test(., Lward.queens_weight)%>%
  tidy()

Nearest_neighbour <- LonWard_WB_score_IV %>%
  st_drop_geometry()%>%
  dplyr::select(model2resids)%>%
  pull()%>%
  moran.test(., Lward.knn_4_weight)%>%
  tidy()

Queen
Nearest_neighbour  
#  Morans's I [0.47 ; 0.5] moderate spatial autocorrelation in the residuals


### Spatial Regression Models
## Dealing with Spatially Autocorrelated Residuals - Spatial Lag and Spatial Error models
#   The Spatial Lag (lagged dependent variable) model

# spatially-lagged regression model with a queen’s case weights matrix

slag_dv_model2_queen <- lagsarlm(WB_score_2013 ~ I(inc_hh_median^(1/3)) + 
                                   I(Population_per_square_kilometre^(1/3)),
                                 data = LonWard_WB_score_IV, 
                                 nb2listw(LWard_nb, style="C"), 
                                 method = "eigen")


#what do the outputs show?
tidy(slag_dv_model2_queen)

#glance() gives model stats but this need something produced from a linear model
#here we have used lagsarlm()
glance(slag_dv_model2_queen)  

t<-summary(slag_dv_model2_queen)  
# AIC 2968

#run a spatially-lagged regression model with KNN
slag_dv_model2_knn4 <- lagsarlm(WB_score_2013 ~ I(inc_hh_median^(1/3)) + 
                                  I(Population_per_square_kilometre^(1/3)),
                                data = LonWard_WB_score_IV, 
                                nb2listw(LWard_knn, 
                                         style="C"), 
                                method = "eigen")

#what do the outputs show?
tidy(slag_dv_model2_knn4)  
glance(slag_dv_model2_knn4) 
summary(slag_dv_model2_knn4)  
# Rho: 0.44392,
# AIC 2956

#write out the residuals
LonWard_WB_score_IV <- LonWard_WB_score_IV %>%
  mutate(slag_dv_model2_knn_resids = residuals(slag_dv_model2_knn4))

KNN4Moran <- LonWard_WB_score_IV %>%
  st_drop_geometry()%>%
  dplyr::select(slag_dv_model2_knn_resids)%>%
  pull()%>%
  moran.test(., Lward.knn_4_weight)%>%
  tidy()

KNN4Moran

# The Spatial Error Model

sem_model2 <- errorsarlm(WB_score_2013 ~ I(inc_hh_median^(1/3)) + 
                           I(Population_per_square_kilometre^(1/3)),
                         data = LonWard_WB_score_IV,
                         nb2listw(LWard_knn, style="C"), 
                         method = "eigen")

tidy(sem_model2)
glance(sem_model2) 
summary(sem_model2)

# AIC 2837
# R² = 0.852

### GWR
# reminder (+ residuals' Moran above )
tmap_mode("view")
qtm(LonWard_WB_score_IV, fill = "model2resids")


st_crs(LonWard_WB_score_IV) = 27700

LonWard_WB_SP <- LonWard_WB_score_IV %>%
  as(., "Spatial")

st_crs(coordsW) = 27700

coordsWSP <- coordsW %>%
  as(., "Spatial")  

coordsWSP

#calculate kernel bandwidth
GWRbandwidth <- gwr.sel(WB_score_2013 ~ I(inc_hh_median^(1/3)) + 
                          I(Population_per_square_kilometre^(1/3)),
                        data = LonWard_WB_SP, 
                        coords=coordsWSP,
                        adapt=T)

#run the gwr model
gwr.model = gwr(WB_score_2013 ~  I(inc_hh_median^(1/3)) + 
                  I(Population_per_square_kilometre^(1/3)), 
                data = LonWard_WB_SP, 
                coords=coordsWSP, 
                adapt=GWRbandwidth, 
                hatmatrix=TRUE, 
                se.fit=TRUE)

#### different transf
#calculate kernel bandwidth
GWRbandwidth <- gwr.sel(WB_score_2013 ~ log(inc_hh_median) + 
                          I(Population_per_square_kilometre^(1/3)),
                        data = LonWard_WB_SP, 
                        coords=coordsWSP,
                        adapt=T)
#run the gwr model
gwr.model = gwr(WB_score_2013 ~  log(inc_hh_median) + 
                  I(Population_per_square_kilometre^(1/3)), 
                data = LonWard_WB_SP, 
                coords=coordsWSP, 
                adapt=GWRbandwidth, 
                hatmatrix=TRUE, 
                se.fit=TRUE)

#print the results of the model
gwr.model 

# AIC 2325
# AICc 2804 = better than spat err & spat lag
# quasi global ? R² 0.94

results <- as.data.frame(gwr.model$SDF)

#attach coefficients to original SF
LonWard_WB_score_IV2 <- LonWard_WB_score_IV %>%
  mutate( coefHhincome = results$I.inc_hh_median..1.3..,
          coefpopdensity = results$I.Population_per_square_kilometre..1.3..)

# coeff household income  
tm_shape(LonWard_WB_score_IV2) +
  tm_polygons(col = "coefHhincome", 
              palette = "-RdYlBu", 
              alpha = 0.5)


#run the significance test
sigTest = gwr.model$SDF$"I.inc_hh_median..1.3.." / gwr.model$SDF$"I.inc_hh_median..1.3.._se"

#store significance results
LonWard_WB_score_IV2 <- LonWard_WB_score_IV2 %>%
  mutate(GWRhhincSig = sigTest)

# map  report  GWR coeff hh income Sig
tmap_mode("plot")
tm_shape(LonWard_WB_score_IV2) +
  tm_polygons(col = "GWRhhincSig",
              palette = "-RdYlBu", title="GWR Coefficient")+
  tm_layout(main.title = "GWR significant Coefficient Household Income", main.title.size = 1, legend.outside = T, legend.outside.position =  "right"  )

summary(LonWard_WB_score_IV2$GWRhhincSig)

# same with pop density?
tm_shape(LonWard_WB_score_IV2) +
  tm_polygons(col = "coefpopdensity",
              palette = "-RdBu", 
              alpha = 0.5)

# Sig test pop density from Bristol ac uk 
t2 = gwr.model$SDF$"I.Population_per_square_kilometre..1.3.." / gwr.model$SDF$"I.Population_per_square_kilometre..1.3.._se"

#store significance results
LonWard_WB_score_IV2 <- LonWard_WB_score_IV2 %>%
  mutate(GWRpopdensSigt2 = t2)

#plot
tmap_mode("plot")
tm_shape(LonWard_WB_score_IV2) +
  tm_polygons(col = "GWRpopdensSigt2",
              palette = "-RdYlBu",  title="GWR Coefficient")+
  tm_layout(main.title = "GWR significant Coefficient Population Density", main.title.size = 1, legend.outside = T, legend.outside.position =  "right"  )

summary(LonWard_WB_score_IV2$GWRpopdensSigt2)

# here areas in blue are areas where WB would go down as pop density increases (like in lm), 
# and area in yellow / orange where WB would go up if pop density was higher. (?)