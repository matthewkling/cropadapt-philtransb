
# libraries ====================================================================================

library(tidyverse)
library(patchwork)
library(sf)
library(rgdal)
library(spdep)
library(spatialreg)



# data import and formatting =========================================================================

# load county crop dataset, which was generated in Earth Engine at 
# https://code.earthengine.google.com/824794f20ce4320ac8625b49c9a49fec
d <- read_csv("inputs/county_crop_freq_ts_raw.csv") %>%
        select(counts, county_fips, state_fips, year)

# parse and clean
unpack_dict <- function(x){# function to convert json dictionary string to data frame
        x %>% 
                str_remove("\\{") %>% str_remove("\\}") %>%
                str_split(", ") %>%
                "[["(1) %>%
                str_split("=", simplify = T) %>%
                as.data.frame() %>%
                setNames(c("id", "freq"))
}
f <- 1:nrow(d) %>%
        map_df(function(i){
                if(d$counts[i] == "{}") return(NULL)
                unpack_dict(d$counts[i]) %>% 
                        mutate(fips = paste0(d$state_fips[i], d$county_fips[i]),
                               year = d$year[i])}) %>%
        as_tibble() %>%
        mutate(id = as.integer(id),
               freq = as.numeric(freq))

# add crop metadata
id <- read_csv("inputs/cdl_bands.csv") %>%
        rename(id = value,
               crop = description)
f <- f %>%
        left_join(id) %>%
        mutate(crop = tolower(crop))

# load county spatial data
counties <- st_read("inputs/Counties", "cb_2018_us_county_500k") %>%
        mutate(area = st_area(.),
               fips = paste0(STATEFP, COUNTYFP))

# load climatologies, 1950-1980
climate <- read_csv("inputs/climate.csv") %>%
        filter(start == 1950) %>%
        select(fips, aet, cwd) %>%
        mutate(fips = str_pad(fips, 5, "left", "0"))

# join crop and climate datasets
nonag <- c(0, 63:65, 81:195)
f <- f %>%
        filter(! id %in% nonag,
               crop != " ") %>%
        left_join(climate) %>%
        mutate(acres = freq / (10/3)^2 * 2.47105) %>%
        filter(is.finite(aet),
               is.finite(acres)) %>%
        gather(var, county_value, aet, cwd)



# spatiotemporal indices ================================================================

# crop & county climate means
county <- f %>% 
        group_by(crop, var) %>%
        mutate(crop_value = weighted.mean(county_value, acres, na.rm = T)) %>% # crop climate index
        group_by(fips, year, var) %>%
        summarize(crop_value = weighted.mean(crop_value, acres), # county_climate index
                  acres = sum(acres)) %>%
        ungroup()

# trends in crop values within counties
county_lm <- county %>% na.omit() %>%
        rename(value = crop_value) %>%
        group_by(var) %>%
        mutate(value = (value - mean(value)) / sd(value),
               year = year - mean(year)) %>%
        group_by(fips, var) %>%
        do(broom::tidy(lm(value ~ year, ., weights = acres))) %>%
        select(fips, var, term, estimate) %>%
        spread(term, estimate) %>%
        janitor::clean_names() %>%
        rename(crop_trend = year)

# climate trends
trends <- read_csv("inputs/climate.csv") %>%
        # trends <- read_csv("e:/ca2cc/ca2cc-county/data/ignore/climate.csv") %>%
        filter(start %in% c(1950, 1982, 2007, 2012)) %>%
        select(fips, year = start, aet, cwd) %>%
        mutate(fips = str_pad(fips, 5, "left", "0")) %>%
        gather(var, value, aet, cwd) %>%
        mutate(year = paste0("y", year)) %>%
        spread(year, value) %>%
        mutate(clim_delta = (y2007+y2012)/2 - y1950) # climate trend: recent mean vs baseline

# combined trend data
county_size <- county %>% group_by(fips) %>% summarize(acres = mean(acres))
td <- trends %>%
        right_join(county_lm) %>%
        right_join(county_size) %>%
        filter(var %in% c("cwd", "aet")) %>%
        mutate(variable = factor(var, levels = c("aet", "cwd"),
                                 labels = c("actual evapotranspiration", "climatic water deficit")))

# trend summaries 
tsd <- td %>%
        group_by(var, variable) %>%
        summarize(clim_delta = weighted.mean(clim_delta, acres, na.rm = T),
                  crop_trend = weighted.mean(crop_trend, acres, na.rm = T))



# figures =========================================================================

# scatterplots
scats <- td %>%
        mutate(var = toupper(var)) %>%
        ggplot(aes(clim_delta, crop_trend, weight = acres)) +
        facet_wrap(~ variable, scales = "free", ncol = 1) +
        geom_point(aes(color = sign(clim_delta) == sign(crop_trend)), size = .5) +
        geom_hline(yintercept = 0, color = "gray") +
        geom_vline(xintercept = 0, color = "gray") +
        geom_vline(data = tsd, aes(xintercept = clim_delta), 
                   color = "black") +
        geom_hline(data = tsd, aes(yintercept = crop_trend), 
                   color = "black") +
        geom_smooth(method = "lm", color = "black", se = F) +
        scale_color_manual(values = c("darkred", "dodgerblue")) +
        coord_cartesian(ylim = c(-.05, .05),
                        xlim = c(-100, 75)) +
        theme_minimal() +
        theme(legend.position = "none") +
        labs(y = "trend in farm climate index", 
             x = "trend in climate variable")

# maps
maps <- counties %>%
        mutate(fips = paste0(STATEFP, COUNTYFP)) %>%
        left_join(trends) %>%
        left_join(td) %>%
        filter(!is.na(variable)) %>%
        mutate(var = toupper(var)) %>%
        ggplot(aes(fill = sign(clim_delta) == sign(crop_trend))) +
        facet_wrap(~ variable, ncol = 1) +
        geom_sf(data = counties, fill = "gray90", color = NA) +
        geom_sf(color = NA) +
        scale_x_continuous(limits = c(-125, -66), expand = c(0, 0)) +
        scale_y_continuous(limits = c(25, 50), expand = c(0, 0)) +
        scale_fill_manual(values = c("darkred", "dodgerblue")) +
        theme_void() +
        theme(legend.position = "bottom") +
        labs(fill = "signs of climate and crop trends match")

# combine plots and save to disk
p <- scats + maps + guide_area() +
        plot_layout(design = c("AB
                               CC"), nrow = 2, widths = c(1, 2), heights = c(10, 1),
                    guides = "collect") 
ggsave("outputs/figure_3.png", 
       p, width = 8, height = 6.5, units = "in")



# hypothesis testing =========================================================================

# data logging function
path <- "outputs/stats.txt"
if(file.exists(path)) file.remove(path)
record <- function(x, preamble = NULL, summarize = F){
        sink(path, append = T)
        if(!is.null(preamble)) cat(c("\n", preamble, "\n\n"))
        print(x)
        sink()
}

# construct neighbors list
nb <- counties %>%
        filter(fips %in% td$fips) %>%
        arrange(fips) %>%
        spdep::poly2nb() %>%
        spdep::nb2listw(zero.policy = TRUE)

# prep data for regression models
datasets <- c("aet", "cwd") %>%
        map(function(x) td %>% 
                    filter(var == x) %>% 
                    arrange(fips) %>% 
                    mutate(clim_delta = clim_delta - mean(clim_delta))) %>% # centering predictor so intercept == mean(y)
        setNames(c("aet", "cwd"))

# function to check whether residuals of a regresison model are autocorrelated
resid_autocorr <- function(fit, nbl = nb, alpha = 0.05){
        fit %>%
                pluck("residuals") %>%
                moran.test(nbl, zero.policy = TRUE)
}

# standard OLS regression models
fits <- datasets %>%
        map(function(x) lm(crop_trend ~ clim_delta, data = x, weight = acres))
fits %>% map(summary) %>% record("STANDARD OLS MODELS, regression summaries:")
fits %>% map(resid_autocorr) %>% record("STANDARD OLS MODELS, residual autocorrelation tests:")

# fit spatial error regression models (since the prior models do have autocorrelated errors)
sperr_fits <- datasets %>% 
        map(function(x) errorsarlm(crop_trend ~ clim_delta,
                                   data = x, weights = acres, listw = nb,
                                   zero.policy = TRUE, na.action = na.omit))
sperr_fits %>% map(summary) %>% record("SPATIAL ERROR MODELS, regression summaries:") 
sperr_fits %>% map(resid_autocorr) %>% record("SPATIAL ERROR MODELS, residual autocorrelation tests:")