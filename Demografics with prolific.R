demo<- read_xlsx("demo.xlsx")

# Years of play ####
table(demo$yoe)
demo$experienceyear <- ifelse(demo$yoe=="<0.5", 0.5, demo$yoe)
demo$experienceyear <- ifelse(demo$yoe=="10+", 10, demo$experienceyear)
demo$experienceyear<- as.numeric(demo$experienceyear)
ggplot(demo, aes(experienceyear))+
  geom_histogram(fill="#8A4198FF", colour="black")
summetrics(demo, "experienceyear")
#Time Taken ####
demo$timehr<- demo$`Time taken`/3600
summetrics(demo,"timehr")
ggplot(demo, aes(timehr))+
  geom_histogram(fill="#8A4198FF", colour="black")
#Ethnicity ####
table(demo$`Ethnicity simplified`)
#Gender & Sex ####
table(demo$Sex)
table(demo$gender)
#Age ####
demo$Age<- as.numeric(demo$Age)
summetrics(demo,"Age")
#Nationality ####

demo$Nationality[demo$Nationality=="United States"]<-"USA"
demo$Nationality[demo$Nationality=="United Kingdom"]<-"UK"

world<-map_data("world")

world<- filter(world,region%in%unique(demo$Nationality))
nationality <-as.data.frame(table(demo$Nationality))
names(nationality)[1]<- "region"

ue<-c("UK", "Italy", "Ireland", "Czech Republic", "Poland", "Estonia",
      "Hungary", "Portugal", "Greece", "Slovakia", "Slovenia", "Belgium")
nam<-c("USA","Mexico", "Canada")
sam<-c("Argentina")
afr<-c("South Africa", "Zimbabwe", "Nigeria")
#Europe ####

natue<- filter(nationality,region%in%ue)
euro<- filter(world,region%in%ue)
euro <- left_join(euro, natue, by = "region")

centroids <- euro %>%
  group_by(region) %>%
  summarize(long = mean(range(long)),
            lat = mean(range(lat)),
            .groups = 'drop')
centroids$lat[centroids$region=="Belgium"]<-centroids$lat+2
centroids$long[centroids$region=="Slovakia"]<-24
centroids$lat[centroids$region=="Czech Republic"]<-49
centroids$long[centroids$region=="Czech Republic"]<-12
centroids$long[centroids$region=="Slovenia"]<-16
centroids$lat[centroids$region=="Slovenia"]<-45


eup<-ggplot(data = euro, aes(x = long, y = lat, group = group, fill = Freq)) +
  geom_polygon(color = "black") +
  geom_label(
    data = centroids,
    aes(x = long, y = lat, label = region),
    inherit.aes = FALSE,
    size = 3,
    color = "white",       # text color
    fill = "#1A5354FF",        # box background color
    fontface = "italic"    # italic text
  ) +
  coord_fixed(1.3) +
  ggplot2::scale_fill_gradient(low = "#008EA0FF", high = "#8A4198FF", na.value = "grey90") +
  labs(title = "Nationalities in Dataset",
       fill = "Count") +
  theme_classic()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())


#Africa ####

natafr<- filter(nationality,region%in%afr)
africa<- filter(world,region%in%afr)
africa <- left_join(africa, natafr, by = "region")

centroids <- africa %>%
  group_by(region) %>%
  summarize(long = mean(range(long)),
            lat = mean(range(lat)),
            .groups = 'drop')

afrp<-ggplot(data = africa, aes(x = long, y = lat, group = group, fill = Freq)) +
  geom_polygon(color = "black") +
  geom_label(
    data = centroids,
    aes(x = long, y = lat, label = region),
    inherit.aes = FALSE,
    size = 3,
    color = "white",       # text color
    fill = "#1A5354FF",        # box background color
    fontface = "italic"    # italic text
  ) +
  coord_fixed(1.3) +
  ggplot2::scale_fill_gradient(low = "#008EA0FF", high = "#8A4198FF", na.value = "grey90") +
  theme_classic()+
  theme(axis.title = element_blank(),
        title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none")
#North America ####

natnam<- filter(nationality,region%in%nam)
noam<- filter(world,region%in%nam)
noam <- left_join(noam, natnam, by = "region")

centroids <- noam %>%
  group_by(region) %>%
  summarize(long = mean(range(long)),
            lat = mean(range(lat)),
            .groups = 'drop')
centroids$long[centroids$region=="USA"]<-centroids$long[centroids$region=="Mexico"]
centroids$long[centroids$region=="Canada"]<-centroids$long[centroids$region=="Mexico"]
centroids$lat[centroids$region=="USA"]<-40
centroids$lat[centroids$region=="Canada"]<-60
noamp<-ggplot(data = noam, aes(x = long, y = lat, group = group, fill = Freq)) +
  geom_polygon(color = "black") +
  geom_label(
    data = centroids,
    aes(x = long, y = lat, label = region),
    inherit.aes = FALSE,
    size = 3,
    color = "white",       # text color
    fill = "#1A5354FF",        # box background color
    fontface = "italic"    # italic text
  ) +
  coord_fixed(1.3) +
  ggplot2::scale_fill_gradient(low = "#008EA0FF", high = "#8A4198FF", na.value = "grey90") +
  xlim(-180, -48) +
  theme_classic()+
  theme(axis.title = element_blank(),
        title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none")


#South America ####

natsam<- filter(nationality,region%in%sam)
soam<- filter(world,region%in%sam)
soam <- left_join(soam, natsam, by = "region")

centroids <- soam %>%
  group_by(region) %>%
  summarize(long = mean(range(long)),
            lat = mean(range(lat)),
            .groups = 'drop')

soamp<-ggplot(data = soam, aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "black", fill = "#008EA0FF") +
  geom_label(
    data = centroids,
    aes(x = long, y = lat, label = region),
    inherit.aes = FALSE,
    size = 3,
    color = "white",       # text color
    fill = "#1A5354FF",        # box background color
    fontface = "italic"    # italic text
  ) +
  coord_fixed(1.3)+
  theme_classic()+
  theme(axis.title = element_blank(),
        title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none")
#Graph Combination ####
# Combine with shared legend
(noamp | eup) / ( soamp | afrp) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

