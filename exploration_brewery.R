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

  