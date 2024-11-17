###############################################################################
# Name: 015_figure1.R
# Author: Emmerich Davies <emmerich.davies@gmail.com>
# Purpose: Creates a map showing how assembly constituencies are nested within
# districts
###############################################################################

###############################################################################
# Load packages
###############################################################################

library(sf)
library(dplyr)
library(stringr)
library(ggmap)
library(ggplot2)
library(here)

###############################################################################
# Load data
###############################################################################

# Assembly constituency
sf_ac_rajasthan <- st_read(
        here("data/raw/assembly constituency maps/AC_RAJASTHAN.shp")
    ) %>%
    select(CONS_NAME, geometry) %>%
    mutate(
        CONS_NAME = str_replace(CONS_NAME, "\\(SC\\)", ""),
        CONS_NAME = str_replace(CONS_NAME, "\\(ST\\)", "")
    ) %>%
    filter(
        CONS_NAME == "Malpura" | CONS_NAME == "Niwai" | CONS_NAME == "Tonk" |
        CONS_NAME == "Deoli-Uniara"
    )

# Location of Tonk:
lon <- c(75.78)
lat <- c(26.17)
df_tonk <- as.data.frame(cbind(lon, lat))

figure_1 <- ggplot(sf_ac_rajasthan) +
    geom_sf(
        fill = "white",
        colour = "black"
    ) +
    geom_point(
        data = df_tonk, aes(x = lon, y = lat),
        size = 2,
        colour = "red"
    ) +
    theme_bw() +
    theme(
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0)
    ) +
    labs(
        x = "",
        y = ""
    )

figure_1

ggsave(
    here::here("data/output/figures/district_ac_overlay.pdf"),
    height = 3,
    width = 3,
    unit = "in"
)
