install.packages("ggplot2")
install.packages('dplyr')
install.packages("ggpubr")
install.packages('rcartocolor')

library(devtools)
install_github("hrbrmstr/waffle")

library(ggplot2)
library(waffle)
library(dplyr)
library(ggpubr)
library(RColorBrewer)
library(rcartocolor)

beers = read.csv('data/beers.csv')

breweries = read.csv('data/breweries.csv')

sd_county = c('Carlsbad', 'Chula Vista', 'Coronado', 'Del Mar', 'El Cajon',
              'Encinitas', 'Escondido', 'Imperial Beach', 'La Mesa',
              'Lemon Grove', 'National City', 'Oceanside', 'Poway', 'San Diego',
              'San Marcos', 'Santee', 'Solana Beach', 'Vista')

orange_county = c('Aliso Viejo', 'Anaheim', 'Brea', 'Buena Park', 'Costa Mesa',
                  'Cypress', 'Dana Point', 'Fountain Valley', 'Fullerton',
                  'Garden Grove', 'Huntington Beach', 'Irvine', 'Laguna Beach',
                  'Laguna Hills', 'Laguna Niguel', 'Laguna Woods', 'La Habra',
                  'Lake Forest', 'La Palma', 'Los Alamitos', 'Mission Viejo',
                  'Newport Beach','Orange', 'Placentia', 'Rancho Santa Margarita',
                  'San Clemente', 'San Juan Capistrano', 'Santa Ana', 'Seal Beach',
                  'Stanton', 'Tustin', 'Villa Park', 'Westminster', 'Yorba Linda')

los_ang_county = c('Agoura Hills', 'Alhambra', 'Arcadia', 'Artesia', 'Avalon',
                   'Azusa', 'Baldwin Park', 'Bell', 'Bellflower', 'BSell Gardens',
                   'Beverly Hills', 'Bradbury', 'Burbank', 'Calabasas', 'Carson',
                   'Cerritos', 'Claremont', 'Commerce', 'Compton', 'Covina',
                   'Cudahy', 'Culver City', 'Diamond Bar', 'Downey', 'Duarte',
                   'El Monte', 'El Segundo', 'Gardena', 'Glendale', 'Glendora',
                   'Hawaiian Gardens', 'Hawthorne', 'Hermosa Beach', 'Hidden Hills',
                   'Huntington Park', 'Industry', 'Inglewood', 'Irwindale',
                   'La Canada Flintridge', 'La Habra Heights', 'Lakewood',
                   'La Mirada', 'Lancaster','La Puente', 'La Verne', 'Lawndale',
                   'Lomita', 'Long Beach', 'Los Angeles', 'Lynwood', 'Malibu',
                   'Manhattan Beach', 'Maywood', 'Monrovia', 'Montebello',
                   'Monterey Park', 'Norwalk', 'Palmdale', 'Palos Verdes Estates',
                   'Paramount', 'Pasadena', 'Pico Rivera', 'Pomona',
                   'Rancho Palos Verdes', 'Redondo Beach','Rolling Hills',
                   'Rolling Hills Estates','Rosemead','San Dimas','San Fernando',
                   'San Gabriel','San Marino','Santa Clarita','Santa Fe Springs',
                   'Santa Monica','Sierra Madre','Signal Hill','South El Monte',
                   'South Gate','South Pasadena','Temple City','Torrance',
                   'Vernon','Walnut','West Covina','West Hollywood',
                   'Westlake Village','Whittier')

sd_los_al = c(los_ang_county,sd_county)

ca_breweries = breweries[breweries$state == ' CA',]

oc_breweries = ca_breweries[ca_breweries$city %in% orange_county,]

sd_brew = ca_breweries[ca_breweries$city %in% sd_county,]

los_ang_brew = ca_breweries[ca_breweries$city %in% los_ang_county,]

sd_los_al_brew = ca_breweries[ca_breweries$city %in% sd_los_al,]

names(sd_los_al_brew) = c('brewery_id',"name" , "city",  "state")

my_beer = beers[beers$brewery_id %in% sd_los_al_brew$brewery_id,]

final_beer = merge(my_beer,sd_los_al_brew, by = 'brewery_id' )

remove_brewery = c("Butcher's Brewing", 'Claremont Craft Ales', 
                   'Mother Earth Brew Company')

final_beer = final_beer[!(final_beer$name.y %in% remove_brewery),]

types = as.data.frame(table(final_beer$name.y))

table(final_beer$style)


# Waffle plot 

carto_pal(n=12,name='Safe')
brewer.pal(n=9,name = 'YlOrRd')


library(extrafont)

load.fontawesome(font = "fontawesome-webfont.ttf")
load.fontawesome(font = "fontawesome-webfont.otf")


font_import('/Users/noahferrel/Library/Fonts')
y
extrafont::fonttable() %>% 
  dplyr::as_tibble() %>% 
  dplyr::filter(grepl("Awesom", FamilyName)) %>% 
  select(FamilyName, FontName, fontfile)

waffle(types, rows = 7,colors = carto_pal(n=12,name='Safe'))

#+
  scale_fill_manual(values =carto_pal(n=12,name='Safe') )
  scale_fill_brewer(palette = 'Paired',name = 'Breweries') #title = "Beers per Brewery"

final_beer$ibu = as.numeric(final_beer$ibu)
  
# Box Plot

par(mar=c(15,5,1,1))
boxplot(abv~name.y,data=final_beer,col = carto_pal(n=12,name='Safe'),las=2,xlab = "")+mtext("Breweries", side=1, line=14)#+
  scale_fill_manual(values =carto_pal(n=12,name='Safe') ) #title = "ABV per Brewery"

par(mar=c(20,5,1,1))
boxplot(ibu~name.y,data=na.omit(final_beer),col = carto_pal(n=12,name='Safe'),las=2,xlab = "")+mtext("Breweries", side=1, line=14) #title = "ABV per Brewery"


ave_ounces = final_beer %>% 
  group_by(name.y) %>%
  summarise(avg = mean(ounces))

ave_ounces

brewer.pal(n=12,name = 'Paired')

adj_color = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "black", "#B15928")

ggdotchart(ave_ounces, x = "name.y", y = "avg",
           color = "name.y",                                # Color by groups
           palette = carto_pal(n=12,name='Safe'),#adj_color, #brewer.pal(n=12,name = 'Paired'), # Custom color palette
           sorting = "descending",                       # Sort value in descending order
           rotate = TRUE,                                # Rotate vertically
           dot.size = 4,                                 # Large dot size
           y.text.col = TRUE,                            # Color y text by groups
           xlab = "Average Ounces",
           ggtheme = theme_pubr()                        # ggplot2 theme
)+ theme_cleveland() + scale_fill_manual(values = carto_pal(n=12,name='Safe'))      



style 

#_________Circle Packing Plot___________________________________________________

install.packages('packcircles')
install.packages('cartography')

library(packcircles)
library(ggplot2)
library(viridis)
library(cartography)

# Select Data


# Generate the layout. sizetype can be area or radius, following your preference on what to be proportional to value.
packing <- circleProgressiveLayout(types$Freq, sizetype='area')
data <- cbind(types, packing)
dat.gg <- circleLayoutVertices(packing, npoints=100)
data$text_size <- types$Freq+1000
t= as.character(data$Var1)
data$names = gsub(' ', '\n',t)
data$text_size = c(5,11,4.5,6,5.5,6,8,3.5,4,4,8,5) 
# Basic color customization
p <- ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=as.factor(id)), colour = "black", alpha = 0.6) +
  scale_fill_manual(values =carto_pal(n=12,name='Safe') ) +
  geom_text(data = data, aes(x, y, label = names),size = data$text_size) +
  scale_size_continuous(range = c(1,4)) +
  theme_void() + 
  theme(legend.position="none") +
  coord_equal()

p

plot(1,1)
legendCirclesSymbols(pos = "left",
                     var = c(min(data$Freq), max(data$Freq)),
                     inches = 0.2, style = "e",col = 'white',
                     title.txt = 'Number of Unique Beers',cex = 4,
                     title.cex =0.9,values.cex = 0.9,frame = TRUE)


##___________________California Beer Map_______________________________________

Ballast_loc = c('1540 Disneyland Dr #201, Anaheim, CA 92802',33.809304396052035, -117.92266960913166,
                '110 N Marina Dr Long Beach, CA', 33.74663640725254, -118.11474485146265,
                '9045 Carroll Way San Diego, CA', 32.888127658680595, -117.15789787272077,
                '2215 India St San Diego, CA', 32.727797042165214, -117.169632070876,
                '5401 Linda Vista Rd #406 San Diego, CA', 32.76686042757759, -117.19530681689929,
)

my_array = t(array(Ballast_loc,dim = c(3,length(Ballast_loc))))

mydf = as.data.frame(my_array)

mydf = cbind(mydf,rep('Ballast Point Brewing Company',length(Ballast_loc)))
names(mydf) = c('Address','long','lat','brewery')

install_github('UrbanInstitute/urbnmapr')
library(tidyverse)
library(urbnmapr)

ggplot() + 
  geom_polygon(data = urbnmapr::states, mapping = aes(x = long, y = lat, group = group),
               fill = 'grey', color = 'white') +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)

household_data <- left_join(countydata, counties, by = "county_fips") 

household_data %>%
  ggplot(aes(long, lat, group = group, fill = medhhincome)) +
  geom_polygon(color = NA) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = "Median Household Income")


countydata %>% 
  left_join(counties, by = "county_fips") %>% 
  filter(state_name =="California") %>% 
  ggplot(mapping = aes(long, lat, group = group, fill = horate)) +
  geom_polygon(color = "#ffffff", size = .25) +
  geom_polygon(data = mydf, aes(long, lat, group = brewery, fill=as.factor(brewery)), colour = "black", alpha = 0.6)
  

library(mapproj)

states_map<-map_data("state")

cal_map = states_map[states_map$region == 'california',]

basemap<-ggplot(cal_map,aes(x=long,y=lat))

# Color map sequential increasing with colorspace
basemap + scale_fill_continuous_sequential(palette="Viridis")


lon <- mydf$long  #fake longitude vector
lat <- mydf$lat  #fake latitude vector


map(database= "state",regions = c('CA'), col="#f1a662", fill=TRUE)
points(lon,lat, pch=20, cex=10, col="red")  #plot converted points


ggplot(data = world) +
  geom_sf() +
  geom_sf(data = counties, fill = NA, color = gray(.5)) +
  geom_sf(data = flcities) +
  geom_text(data = flcities, aes(x = lng, y = lat, label = city), 
            size = 3.9, col = "black", fontface = "bold") +
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)

##_______________Beer Type Matrix______________________________________________
install.packages(c("fastDummies", "recipes"))

library(fastDummies)
library(recipes)
library(tidyr)
library(dplyr)
library(ggplot2)
# or simply just use: library(tidyverse)


type_brew = final_beer[,c('X','style','name.y')]
head(type_brew)

df <- data.frame(file_name = c("1_jan_2018.csv",
                               "2_feb_2018.csv", 
                               "3_mar_2018.csv"))

types_split = type_brew[grep('/',type_brew$style),]
types_fine = type_brew[-grep('/',type_brew$style),]

temp = as.data.frame(type_brew[6,])

for(x in 1:dim(types_split)[1]){
  temp2 = strsplit(types_split$style[x],'/')
  for(y in 1:length(temp2[[1]])){
    temp3 = as.data.frame(cbind(temp2[[1]][y],types_split$name.y[x]))
    names(temp3) = c("style", "name.y")
    temp = rbind(temp,temp3)
  }
}

temp$style = gsub(" ", "", temp$style, fixed = TRUE)


sep_styles = rbind(types_fine,temp)

uniq_styles = unique(sep_styles)
uniq_styles$style = gsub(" ", "", uniq_styles$style, fixed = TRUE)
uniq_styles$style = as.factor(uniq_styles$style)

summary(uniq_styles)

dumb = dummy_cols(uniq_styles,select_columns = 'style')

dumb = dumb[,2:24]


sep_df = lapply(unique(dumb$name.y), function(x) dumb[dumb$name.y == x,])

grouped_df = lapply(sep_df, function(x) cbind(x[1,1],t(colSums(x[,2:23]))))

final_style = as.data.frame(do.call(rbind,grouped_df))
names(final_style)  = gsub("style_", "", names(final_style), fixed = TRUE)
names(final_style) = c('Breweries',names(final_style)[2:23])

final_style %>% 
  as.data.frame() %>%
  mutate(id=rownames(.),
         Labels = as.factor(Breweries)) %>%
  pivot_longer(cols=names(final_style[,2:23])) %>%
  filter(value==1) %>%
  ggplot(aes(x=id, y=name, color=Breweries)) + 
  geom_point(size =7)+scale_color_manual(values =carto_pal(n=12,name='Safe') ) +
  theme_pubr(legend = 'right')
  
scale_color_manual()