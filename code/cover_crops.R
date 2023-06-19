

# figure 2: cover crops =========================================================

library(tidyverse)
library(sf)
library(patchwork)

# load USDA Agricultural Census data on cover crops
d <- read_csv("inputs/cover_crops.csv")

# load county spatial data
f <- st_read("inputs/Counties/cb_2018_us_county_500k.shp") %>% 
        select(STATEFP,
               COUNTYFP,
               fips = GEOID, 
               geometry) %>% 
        right_join(d, by="fips")

# panel a: 2017 cover crop adoption rate
p_a <- f %>%
        filter(year == 2017) %>%
        mutate(cover_crop = as.numeric(cover_crop_acres) / as.numeric(acres_operated)) %>%
        select(geometry, cover_crop) %>%
        ggplot(aes(fill = ifelse(cover_crop < .01, .01, cover_crop))) +
        geom_sf(color = NA) +
        scale_fill_viridis_c(trans = "log10",
                             breaks = c(.01, .03, .1, .3),
                             labels = c("<0.01", .03, .1, .3),
                             na.value = "gray90") +
        theme_void() +
        theme(legend.position = "bottom",
              strip.text = element_text(size = 18)) +
        guides(fill = guide_colorbar(barwidth = 21, barheight = .5, title.position = "top")) +
        labs(fill = "proportion of farmed acreage employing cover crops in 2017")


# panel b: chagne in adoption rate between 2012 and 2017
p_b <- f %>%
        select(fips, year, cca = cover_crop_acres) %>%
        mutate(cca = as.numeric(cca),
               year = paste0("y", year)) %>%
        spread(year, cca) %>%
        mutate(diff = y2017-y2012) %>%
        ggplot(aes(fill = case_when(diff > 20000 ~ 20000,
                                    diff < -20000 ~ -20000,
                                    TRUE ~ diff) / 1000)) +
        geom_sf(color = NA) +
        scale_fill_gradientn(colors = c("darkred", "orange", "gray80", "dodgerblue", "darkblue"),
                             values = c(0, .4, .5, .6, 1),
                             limits = c(-20, 20),
                             breaks = c(-20, -10, 0, 10, 20),
                             labels = c(-20, -10, 0, 10, "20+"),
                             na.value = "gray90") +
        theme_void() +
        theme(legend.position = "bottom",
              strip.text = element_text(size = 18)) +
        guides(fill = guide_colorbar(barwidth = 21, barheight = .5, title.position = "top")) +
        labs(fill = "change in cover crops from 2012-2017 (thousands of acres)")

# combine panels and save figure
p <- p_a + p_b + 
        plot_layout(ncol = 1) +
        plot_annotation(tag_levels = 'a', 
                        tag_prefix = "(", tag_suffix = ")") & 
        theme(plot.tag = element_text(size = 20))
ggsave("outputs/figure_2.pdf", 
       p, width = 6, height = 8, units = "in")