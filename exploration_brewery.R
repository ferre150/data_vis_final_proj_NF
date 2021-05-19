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


#_________Read_in_and_clean_data________________________________________________

# read in csv that contains beer info
beers = read.csv('data/beers.csv')

#read in csv with brewery info
breweries = read.csv('data/breweries.csv')

# all cities in San Diego county
sd_county = c('Carlsbad', 'Chula Vista', 'Coronado', 'Del Mar', 'El Cajon',
              'Encinitas', 'Escondido', 'Imperial Beach', 'La Mesa',
              'Lemon Grove', 'National City', 'Oceanside', 'Poway', 'San Diego',
              'San Marcos', 'Santee', 'Solana Beach', 'Vista')

# all cities in OC
orange_county = c('Aliso Viejo', 'Anaheim', 'Brea', 'Buena Park', 'Costa Mesa',
                  'Cypress', 'Dana Point', 'Fountain Valley', 'Fullerton',
                  'Garden Grove', 'Huntington Beach', 'Irvine', 'Laguna Beach',
                  'Laguna Hills', 'Laguna Niguel', 'Laguna Woods', 'La Habra',
                  'Lake Forest', 'La Palma', 'Los Alamitos', 'Mission Viejo',
                  'Newport Beach','Orange', 'Placentia', 'Rancho Santa Margarita',
                  'San Clemente', 'San Juan Capistrano', 'Santa Ana', 'Seal Beach',
                  'Stanton', 'Tustin', 'Villa Park', 'Westminster', 'Yorba Linda')

# all cities in LA county
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

# concatenate LA and SD
sd_los_al = c(los_ang_county,sd_county)

# subset the Breweries to just the CA
ca_breweries = breweries[breweries$state == ' CA',] 

# subset the breweries by cities in OC
oc_breweries = ca_breweries[ca_breweries$city %in% orange_county,]

# subset the breweries by cities in SD
sd_brew = ca_breweries[ca_breweries$city %in% sd_county,]

# subset the breweries by cities in LA
los_ang_brew = ca_breweries[ca_breweries$city %in% los_ang_county,]

# # subset the breweries by cities in SD and LA
sd_los_al_brew = ca_breweries[ca_breweries$city %in% sd_los_al,]

names(sd_los_al_brew) = c('brewery_id',"name" , "city",  "state")

#subset beers by brewery ids in SD and LA
my_beer = beers[beers$brewery_id %in% sd_los_al_brew$brewery_id,]

# merge the Beers with their brewery
final_beer = merge(my_beer,sd_los_al_brew, by = 'brewery_id' )

#remove breweries with less than three beers
remove_brewery = c("Butcher's Brewing", 'Claremont Craft Ales', 
                   'Mother Earth Brew Company')

final_beer = final_beer[!(final_beer$name.y %in% remove_brewery),]


#_________Create_a_waffle_plot_of_the_unique_beers______________________________

# the number of beers per brewery
types = as.data.frame(table(final_beer$name.y))
#table(final_beer$style)

# Waffle plot 

waffle(types, rows = 7,colors = carto_pal(n=12,name='Safe'))


#_________Create_a_box_plot_of_the_IBU_and_abv__________________________________

# Cast the IBU to numeric
final_beer$ibu = as.numeric(final_beer$ibu)

# Box Plot ABV

par(mar=c(15,5,1,1))
boxplot(abv~name.y,data=final_beer,col = carto_pal(n=12,name='Safe'),
        las=2,xlab = "") + mtext("Breweries", side=1, line=14)#+

# Box Plot IBU
par(mar=c(20,5,1,1))
boxplot(ibu~name.y,data=na.omit(final_beer),col = carto_pal(n=12,name='Safe')
        ,las=2,xlab = "")+mtext("Breweries", side=1, line=14) #title = "ABV per Brewery"

#_________PLot_the_Average_Size_of-the_Breweries__________________________________
ave_ounces = final_beer %>% 
  group_by(name.y) %>%
  summarise(avg = mean(ounces))

ave_ounces = as.data.frame(ave_ounces)

brewer.pal(n=12,name = 'Paired')

adj_color = c("#DDCC77", "#999933", "#44AA99", "#AA4499", "#661100", "#88CCEE",
              "#332288", "#888888", "#882255", "#CC6677", "#6699CC", "#117733")

ggdotchart(ave_ounces, x = "name.y", y = "avg",
           color = "name.y",                                # Color by groups
           palette = adj_color,#carto_pal(n=12,name='Safe'),       # 
           sorting = "descending",                       # Sort value in descending order
           rotate = TRUE,                                # Rotate vertically
           dot.size = 4,                                 # Large dot size
           y.text.col = TRUE,                            # Color y text by groups
           xlab = "Average Ounces",
           ggtheme = theme_pubr(legend = 'none')                        # ggplot2 theme
)+ theme_cleveland()

#_________Violin_plot_IBU_and_ABV___________________________________________________

abv_df = final_beer[c('abv','name.y')]
names(abv_df) = c('abv',"Breweries")

ibu_df = na.omit(final_beer[c('ibu','name.y')])
names(ibu_df) = c('ibu',"Breweries")

ounce_df = final_beer[c('ounces','name.y')]
names(ounce_df) = c('ounces',"Breweries")

ggplot(abv_df,aes(Breweries,abv,fill = Breweries))+geom_violin()+ coord_flip()+ 
  geom_boxplot(width=0.1) + scale_fill_manual(values = carto_pal(n=12,name='Safe'),
                                              breaks = unique(abv_df$Breweries)) +
  theme_pubr(legend = 'right')


ggplot(ibu_df,aes(Breweries,ibu,fill = Breweries))+geom_violin()+ coord_flip()+ 
  geom_boxplot(width=0.1) + scale_fill_manual(values = carto_pal(n=12,name='Safe'),
                                              breaks = unique(abv_df$Breweries)) +
  theme_pubr(legend = 'right')

ggplot(ounce_df,aes(Breweries,ounces,fill = Breweries))+geom_violin()+ coord_flip()+ 
  geom_boxplot(width=0.1) + scale_fill_manual(values = carto_pal(n=12,name='Safe'),
                                              breaks = unique(abv_df$Breweries)) +
  theme_pubr(legend = 'right')

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
  scale_fill_manual(values =carto_pal(n=12,name='Safe'),breaks = c(5,12,1,6,7,2,10,4,9,3,11,8)) +
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

Brewery_list = unique(final_beer$name.y)

Ballast_loc = c('1540 Disneyland Dr #201, Anaheim, CA 92802',33.809304396052035, -117.92266960913166,
                '110 N Marina Dr Long Beach, CA', 33.74663640725254, -118.11474485146265,
                '9045 Carroll Way San Diego, CA', 32.888127658680595, -117.15789787272077,
                '2215 India St San Diego, CA', 32.727797042165214, -117.169632070876,
                '5401 Linda Vista Rd #406 San Diego, CA', 32.76686042757759, -117.19530681689929
)

Golden_loc = c('5410 W San Fernando Rd Los Angeles, CA 90039', 34.15045778311418, -118.2744569668166,
                '2210 E Orangewood Ave Anaheim, CA 92806', 33.795534416604376, -117.8852393772423,
                '16390 Pacific Coast Hwy, Huntington Beach, CA 92649', 33.72454742044249, -118.07665106179581,
               '201 World Way, Los Angeles, CA 90045',33.941517321230855, -118.40184643831698,
               '317 S Broadway, Los Angeles, CA 90013', 34.05100553082239, -118.24950049795332
)

Pizza_loc = c('135 N. Highway 101 Solana Beach, CA 90725', 32.9929293357069, -117.27218712755574,
               '301 N El Camino Real, San Clemente, CA 92672', 33.42882747718038, -117.61347012267696,
              '571 Carlsbad Village Dr, Carlsbad, CA 92008', 33.15982597634919, -117.3479485019722,
              '1956 Bacon St, San Diego, CA 92107', 32.74794596392057, -117.2502737731884,
              '2730 Gateway Rd, Carlsbad, CA 92009', 33.12861452372483, -117.25018511141947
)

Tail_loc = c('6827 AIROSO AVE SAN DIEGO, CALIFORNIA', 32.777251870318935, -117.05161556882209)

Hess_loc = c('3725 Greenwood St, San Diego, CA 92110', 33.62819290735436, -117.9202140503766)

Saint_loc = c('9550 Distribution Ave, San Diego, CA 92121', 32.880459755617046, -117.16348736002583,
              '978 N Coast Hwy 101, Encinitas, CA 92024', 33.0658373625825, -117.30293762433492
              )

Manz_loc = c('10151 Prospect Ave, Santee, CA 92071', 32.83074108282011, -116.97659159042266)

Mike_loc = c('805 Ocean Ln, Imperial Beach, CA 91932', 32.58073494815664, -117.13208374104312,
             '7955 Silverton Ave #1201, San Diego, CA 92126', 32.88950196712233, -117.14986286002568,
             '3812 Grim Ave, San Diego, CA 92104', 32.74771757271175, -117.12834821629032,
             '4893 Voltaire St, San Diego, CA 92107', 32.75080446471145, -117.24490919552974,
             '2313 Oak Grove Rd, Walnut Creek, CA 94598', 37.928719748407396, -122.01666830221723,
             '879 W Harbor Dr Suite W14-E, San Diego, CA 92101', 32.70939666807193, -117.17193912205761
             )

Dude_loc = c('395 Santa Monica Place, #304 Santa Monica, CA 90401', 34.01386053115494, -118.49397782906449,
             '405 West Center Street Promenade Anaheim, CA 92805', 33.833296180317284, -117.91801916100611,
             '1840 W 208th St Torrance, CA 90501', 33.841649197588715, -118.3115418017758,
             'Westfield Valencia Town Center, 24250 Town Center Dr #190, Santa Clarita, CA 91355', 34.41681649980409, -118.55982716438426,
             '6615 Hollywood Blvd Los Angeles, CA 90028', 34.10184465179344, -118.33403191534236
             )

Monkey_loc = c('1517 30th St, San Diego, CA 92102', 32.721681981543426, -117.13008561574287)

Mission_loc = c('1441 L St, San Diego, CA 92101', 32.70720278166529, -117.15159683825293)



lis_to_df = function(list,Brewery_name){
  my_array = t(array(list,dim = c(3,length(list)/3)))
  mydf = as.data.frame(my_array)
  mydf = cbind(mydf,rep(Brewery_name,length(list)/3))
  names(mydf) = c('Address','long','lat','brewery')
  mydf$long = as.numeric(mydf$long)
  mydf$lat = as.numeric(mydf$lat)
  return(mydf)
}

bal_df = lis_to_df(Ballast_loc,"Ballast Point Brewing Company")
gol_df =lis_to_df(Golden_loc,"Golden Road Brewing")
piz_df = lis_to_df(Pizza_loc, "Pizza Port")
modern_df = lis_to_df(Modern_loc,'Modern Times')
tail_df = lis_to_df(Tail_loc,"TailGate Beer")
hess_df =lis_to_df(Hess_loc,"Hess")
saint_df = lis_to_df(Saint_loc, "Saint Archer Brewery")
manz_df = lis_to_df(Manz_loc,'Manzanita Brewing Company')
mike_df = lis_to_df(Mike_loc,"Mike Hess Brewing Company")
dude_df =lis_to_df(Dude_loc,"The Dudes' Brewing Company")
monk_df = lis_to_df(Monkey_loc, "Monkey Paw Pub & Brewery")
miss_df = lis_to_df(Mission_loc,'Mission Brewery')

library(mapproj)
library(maps)


?map()

#_______________________________________________________________________________
map(database= "state",regions = c('CA'), col="#f9e5b0",fill = T, # #f1a662
    xlim = c(-119, -116.5), ylim = c(32.5, 34.3),boundary = T)
map.axes()	

# Ballast Point Locations
points(bal_df$lat,bal_df$long,col = '#DDCC77', cex = 1,pch = 19)
#Golden Road Locations 
points(gol_df$lat,gol_df$long,col = '#AA4499', cex = 1,pch = 19)
#Pizza Port Locations
points(piz_df$lat,piz_df$long,col = '#882255', cex = 1,pch = 19)
#Modern Times Beer Location
points(modern_df$lat,modern_df$long,col = '#332288', cex = 1,pch = 19)
# TailGate Locations
points(tail_df$lat,tail_df$long,col = '#6699CC', cex = 1,pch = 19)
#Hess Locations 
points(hess_df$lat,hess_df$long,col = '#661100', cex = 1,pch = 19)
#Saint Locations
points(saint_df$lat,saint_df$long,col = '#44AA99', cex = 1,pch = 19)
#Manz Location
points(manz_df$lat,manz_df$long,col = '#999933', cex = 1,pch = 19)
#Mike hess Locations 
points(mike_df$lat,mike_df$long,col = '#88CCEE', cex = 1,pch = 19)
#The dudes' Locations
points(dude_df$lat,dude_df$long,col = '#CC6677', cex = 1,pch = 19)
#Monkey Locations
points(monk_df$lat,monk_df$long,col = '#888888', cex = 1,pch = 19)
#Mission Location
points(miss_df$lat,miss_df$long,col = '#117733', cex = 1,pch = 19)

points(-117.84997346379632,33.79454772568703,col = 'black',cex =1,pch=19)


##_______________Beer Type Matrix______________________________________________
install.packages(c("fastDummies", "recipes",'magrittr'))

library(fastDummies)
library(recipes)
library(tidyr)
library(dplyr)
library(ggplot2)
library(magrittr)

type_brew = final_beer[,c('X','style','name.y')]
head(type_brew)

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
  geom_point(size =7)+scale_color_manual(values =carto_pal(n=12,name='Safe'),
                                         breaks = unique(abv_df$Breweries)) +
  theme_pubr(legend = 'right')+ grids(linetype = "solid",size = 4)+
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

#_____________extra


my_array = t(array(Ballast_loc,dim = c(3,length(Ballast_loc)/3)))
mydf = as.data.frame(my_array)
mydf = cbind(mydf,rep('Ballast Point Brewing Company',length(Ballast_loc)))
names(mydf) = c('Address','long','lat','brewery')

mydf$long = as.numeric(mydf$long)
mydf$lat = as.numeric(mydf$lat)
