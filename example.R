# -----------------------------------------------------------------------#
# Workshop on how to build your hexSticker!
# Author: Vanessa di Lego
# 07-06-2022
# ------------------------------------------------------------------------#

library(ggplot2) #for barplots
library(gplots) #for heatmaps
library(RColorBrewer) #for palettes
library(dplyr) #for dataset manipulation
library(knitr) #for neaty dataset printing
library(timelineS) #for timeline plot
library(circlize) #for chord-diagrams
library(fmsb) #for radar plots
library(here)
library(data.table)
library(geomtextpath)
library(hexSticker)
library(showtext)
library(magrittr)
library(lintr)
library(sf)
library(cowplot)
library(countrycode)
library(ggpubr)
library(gridExtra)
library(grid)
library(biscale)
library(cowplot)
library(sp)
library(cartography)
library(tmap)
library(raster)

# for this first part, data and codes are adapted from https://datascienceplus.com/visualizing-tennis-grand-slam-winners-performances/

# this is an alternative way to read multiple packages at once
# it is helpful when you have a lot to load

library_toload <- c("dplyr", "knitr", "ggplot2", "gplots", "RColorBrewer", "timelineS", "circlize", "fmsb")
invisible(lapply(library_toload, function(x) {suppressPackageStartupMessages(library(x, character.only=TRUE))}))

# reading the data
url_file <- "https://datascienceplus.com/wp-content/uploads/2017/04/tennis-grand-slam-winners.txt"
slam_win <- read.delim(url(url_file), sep="\t", stringsAsFactors = FALSE)
kable(head(slam_win, 10))

# correcting the slam name
slam_win[grep("Australian Open", slam_win$TOURNAMENT), "TOURNAMENT"] = "Australian Open"

# converting to date
year_to_date_trnm <- function(the_year, the_trnm) {
  the_date <- NULL
  if (the_trnm == "Australian Open") {
    the_date <- (paste(the_year, "-01-31", sep=""))
  } else if (the_trnm == "French Open") {
    the_date <- (paste(the_year, "-06-15", sep=""))
  } else if (the_trnm == "Wimbledon") {
    the_date <- (paste(the_year, "-07-15", sep=""))
  } else if (the_trnm == "U.S. Open") {
    the_date <- (paste(the_year, "-09-07", sep=""))
  }
  the_date
}

# how to build a timeline
slam_win$YEAR_DATE <- as.Date(mapply(year_to_date_trnm, slam_win$YEAR, slam_win$TOURNAMENT), format="%Y-%m-%d")
tl_rec <- 1:20
timelineS(slam_win[tl_rec, c("WINNER", "YEAR_DATE")], line.color = "red", scale.font = 3,
          scale = "month", scale.format = "%Y", label.cex = 0.7, buffer.days = 100,
          labels = paste(slam_win[tl_rec, "WINNER"], slam_win[tl_rec, "TOURNAMENT"]))


# creating several summary measures

# Number of wins per player
slam_top_chart <- slam_win %>%
  group_by(WINNER) %>%
  summarise(NUM_WINS=n()) %>%
  arrange(desc(NUM_WINS))

# top 4 winners

slam_top_chart$WINNER <- factor(slam_top_chart$WINNER, levels = slam_top_chart$WINNER[order(slam_top_chart$NUM_WINS)])
top_winners_gt4 <- slam_top_chart %>%
  filter(NUM_WINS >= 4)

# top winners by player and tournament
slam_top_chart_by_trn <-slam_win %>%
  filter(WINNER %in% top_winners_gt4$WINNER) %>%
  group_by(TOURNAMENT, WINNER) %>%
  summarise(NUM_WINS=n()) %>%
  arrange(desc(NUM_WINS))

# ploting top 10
slam_top_chart_by_trn$NUM_WINS <- factor(slam_top_chart_by_trn$NUM_WINS)
kable(head(slam_top_chart_by_trn, 10))



p_hex<-ggplot(slam_top_chart_by_trn%>% filter(WINNER%in%c("Rafael Nadal",
                                                    "Novak Djokovic",
                                                    "Roger Federer")),
       aes(TOURNAMENT,NUM_WINS, color=WINNER, group=WINNER))+
  geom_line(size = 0.8)+
  geom_text_repel(aes(label = WINNER),
                  data = slam_top_chart_by_trn%>%
                    filter(WINNER%in%c("Rafael Nadal",
                                       "Novak Djokovic",
                                       "Roger Federer") &
                  TOURNAMENT %in%c("Wimbledon")),
                  size = 5,
                  hjust = 1,
                  vjust = 0.5,
                  nudge_x = 0.7,
                  nudge_y = 0.5,
                  force=2,
                  point.padding = 1,
                  lineheight = 2)+
scale_color_manual(values = c("#845EC2","#AF5D00","#008C81"), name="Players")+
  theme_void()+
  theme_transparent()+
  theme(axis.title = element_blank(),
        legend.position = "none")

## Loading Google fonts (http://www.google.com/fonts)
font_add_google("Gochi Hand", "gochi")

sticker(p_hex, package="GOAT", p_size=16, s_x=1.08, s_y=0.8, s_width=1.5, s_height=1,
        filename=here("Figures","st_1.png"), h_fill="#B0A8B9", h_color="#f39c12",
        p_color = "#926C00")


# a great part of winning in tennis is related to mental toughness
# data on mental toughness was downloaded from https://www.ultimatetennisstatistics.com/mentalToughness
mt<-fread(here("Data","MentalToughness.csv"))

mt<-mt %>%
  select(1:3,6)

kable(head(mt, 10))

# let´s do a sticker on mental toughness

mt<-mt %>%
mutate(iso3=countrycode(country_name, origin="country.name",destination =  "iso3c"))

# grabbing world data

data(World)

# checking coordinate system

st_crs(World)

#changing to Robinson system; Tim?s request, hope this is what he expected

world_rob<-st_transform(World, "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

#checking again coordinates

st_crs(world_rob)

# joining data with shape file

setnames(world_rob, "iso_a3", "ISO3")
mt_map<-left_join(mt, world_rob, by="ISO3")

st_geometry(mt_map) <- mt_map$geometry

map_m <- ggplot()+
  geom_sf(data = mt_map %>% filter(continent%in%"Europe" & !ISO3%in%"RUS"),
          mapping = aes(fill = rating),
          color = "white", size = 0.1) +
  theme_minimal()+
  scale_fill_viridis_c(option="magma")+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
      legend.key.height = unit(0.3, 'cm'),
      legend.key.size = unit(0.1, 'cm'))

map_hex<-ggdraw(map_m)

sticker(map_hex, package="MENTAL", p_size=20, s_x=1.08, s_y=0.94, s_width=2, s_height=1.4,
        filename=here("Figures","map.png"), h_fill="#B0A8B9", h_color="#f39c12",
        p_color = "black")
