library(tidyverse)
library(lubridate)
library(relayer)

ord_species <- c("LACI", "LABO", "LANO")

timing_records <- readRDS( file.path(wd$bin, "records_coded_tidy.rds") ) %>% 
  filter(def_alive == "Yes") %>% 
  dplyr::mutate(Month = stringr::str_trim(Month)) %>% 
  dplyr::mutate(tidy_month = if_else(Month == "7-8", as.numeric(7), if_else(Month == "Two winters", as.numeric(1), as.numeric(Month)))) %>% 
  dplyr::mutate(Species = factor(Species, levels = ord_species))

Pd_detections <- read_csv(file.path(wd$data, "Pd_detections.csv") )

library(ggnewscale)

# Define angles.
myAng <- seq(-20, -340, length.out = 12)

# Plot!
p <- data.frame(
  Species = c(rep("LACI", 6), rep("LANO", 6), rep("LABO", 6)),
  tidy_month = rep(c(3,4,5,9,10,11), 3),
  n = rep("Migration",6*3),
  y = rep(3,6*3) # y = 3
) %>% 
  ggplot() +
  # Cave record plotting.
  geom_tile( data = {
    mdf %>% 
      group_by(Species, tidy_month) %>% 
      dplyr::summarise(n = n(), y = 4) %>% # y = 4
      ungroup
    },
    aes(x = tidy_month-0.5, y = y, fill = n)
  ) +
  scale_color_viridis_c(option = "A") +
  # Migration plotting
  geom_tile(aes(x = tidy_month-0.5, y = y, fill2 = n) ) %>% 
    rename_geom_aes(new_aes = c("fill" = "fill2")) +
  # Pd detection plotting
  geom_tile(
    data = {
      Pd_detections %>% 
        dplyr::rename(tidy_month = Month) %>% 
        group_by(Species, tidy_month) %>% 
        dplyr::summarise(n = n(), y = 2) # y = 2
    },
    aes(x = tidy_month-0.5, y = y, fill3 = n) 
    ) %>% 
  rename_geom_aes(new_aes = c("fill" = "fill3")) +
  # Color scaling
  scale_colour_viridis_c(aesthetics = "fill", guide = "colorbar", name = "Cave records", option = "viridis", direction = -1, end = 0.8, begin = 0.2) +
  scale_colour_viridis_d(aesthetics = "fill2", guide = "legend", name = NULL, option = "magma", begin = 0.9) +
  scale_colour_viridis_c(aesthetics = "fill3", guide = "legend", name = "Pd detections", option = "magma", direction = -1, breaks = c(1,2), begin = 0.5, end = 0.7) +
  
  # Plotting details.
  facet_wrap(~Species, ncol = 2, as.table = F) +
  geom_hline(yintercept = c(2:5-0.5), colour = "grey90", size = 0.2) +
  theme_classic()+
  coord_polar() +
  scale_x_continuous(labels =  month.abb[1:12], breaks = 1:12-0.5) +
  scale_y_continuous(limits = c(0,4.5), breaks = 2:5-0.5) +
  theme(
    legend.direction = "horizontal",
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    strip.background = element_blank(),
    # Angle axis text
    axis.text.x = element_text(size = 10, angle = myAng),
    strip.text = element_text(size = 14),
    legend.position = c(0.7, 1), legend.justification = c(0, 1)
  ) 
p
ggsave(p, filename = file.path(wd$figs, "Timeline.jpg"),units = "in", height = 6, width = 8)






mdf %>% 
  group_by(Species, tidy_month) %>% 
  dplyr::summarise(n = n(), y = 3) %>%
  ungroup %>% 
  bind_rows(
    .,
    {data.frame(
      Species = rep("LACI", 6),
      tidy_month = c(3,4,5,9,10,11),
      n = rep(5,6),
      y = rep(2,6)
      )},
    {data.frame(
      Species = rep("LANO", 6),
      tidy_month = c(3,4,5,9,10,11),
      n = rep(5,6),
      y = rep(2,6)
    )},
    {data.frame(
      Species = rep("LABO", 6),
      tidy_month = c(3,4,5,9,10,11),
      n = rep(5,6),
      y = rep(2,6)
    )},
  ) %>% 
  ggplot() +
  geom_tile(
    aes(x = tidy_month-0.5, y = y, fill = n)
    ) +
  facet_grid(~Species) +
  theme_classic()+
  coord_polar() +
  scale_fill_viridis_c(option = 1, direction = -1) +
  scale_x_continuous(labels = 0:12, breaks = 0:12) +
  scale_y_continuous(limits = c(0,5))
  
  




mdf <- timing_records %>% 
  group_by(ID) %>% 
  dplyr::mutate(
    start = case_when(
      Month == "7-8" ~ yday( mdy(paste(7, 1, 1990, sep = "-")) ), 
      Month == "Two winters" ~ yday(mdy("1/1/1900")) , # FIX THIS
      is.na(Day)  ~ yday( floor_date( mdy(paste(Month, 15, 1990, sep = "-")), "month" ) ),
      TRUE ~ yday( mdy(paste(Month, Day, 1990, sep = "-")) )
    ),
    end = case_when(
      Month == "7-8" ~ yday( mdy(paste(8, 31, 1990, sep = "-")) ), 
      Month == "Two winters" ~ yday(mdy("2/28/1900")) ,
      is.na(Day) ~ yday( ceiling_date( mdy(paste(Month, 15, 1990, sep = "-")) ) )-1,
      TRUE ~ yday( mdy(paste(Month, Day, 1990, sep = "-")) )
    ),
    DoY = mean(c(end,start))
  ) %>%
  ungroup() %>% 
  dplyr::group_by(Species) %>% 
  arrange(DoY) %>% 
  dplyr::mutate(myorder = row_number()) %>% 
  ungroup()

mdf %>% 
  ggplot() +
  geom_segment(aes(x=start, xend = end, y = myorder, yend = myorder)) +
  geom_point(aes(x = DoY, y = myorder)) +
  facet_wrap(~Species) +
  theme_classic()# +
  #coord_polar()



mdf %>% 
  ggplot() +
  geom_bar(aes(x=DoY, y=Species), fill = ,bins = 12) +
  scale_x_continuous(limits = c(0,365)) +
  theme_classic() +
  facet_wrap(~Species) +
  coord_polar()







timing_records %>% 
  ggplot() +
  geom_bin2d(aes(x=tidy_month, group = Species, fill = Species), alpha = 0.5) +
  scale_x_continuous(name = "Month", limits = c(0,12), expand = c(0,0)) +
  ylab("Density") +
  theme_minimal() +
  scale_fill_manual(
    values = rev(viridis::plasma(3)),
    labels = c(
      expression("Hoary bat ("*italic("Lasiurus cinereus")*")"), 
      expression("Eastern red bat ("*italic("Lasiurus borealis")*")"),
      expression("Silver-haired bat ("*italic("Lasionycteris noctivagans")*")")
    )
  ) +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.25,0.85),
    text = element_text(size = 15),
    axis.title.x = element_text(size = 20),
    legend.text.align = 0,
    plot.margin = unit(c(0.1,1,0.1,0.15), "cm"),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)
  ) +
  facet_wrap(~Species)



# New plot experimentation ------------------------------------------------

data.frame(
  Species = c(rep("LACI", 6), rep("LANO", 6), rep("LABO", 6)),
  tidy_month = rep(c(3,4,5,9,10,11), 3),
  n = rep("Migration",6*3),
  y = rep(3,6*3) # y = 3
) %>% 
  ggplot() +
  # Cave record plotting.
  geom_tile( data = {
    mdf %>% 
      group_by(Species, tidy_month) %>% 
      dplyr::summarise(n = n(), y = 4) %>% # y = 4
      ungroup
  },
  aes(x = tidy_month-0.5, y = y, fill = n)
  ) +
  scale_color_viridis_c(option = "A") +
  # Migration plotting
  geom_tile(aes(x = tidy_month-0.5, y = y, fill2 = n) ) %>% 
  rename_geom_aes(new_aes = c("fill" = "fill2")) +
  # Pd detection plotting
  geom_tile(
    data = {
      Pd_detections %>% 
        dplyr::rename(tidy_month = Month) %>% 
        group_by(Species, tidy_month) %>% 
        dplyr::summarise(n = n(), y = 2) # y = 2
    },
    aes(x = tidy_month-0.5, y = y, fill3 = n) 
  ) %>% 
  rename_geom_aes(new_aes = c("fill" = "fill3")) +
  # Color scaling
  scale_colour_viridis_c(aesthetics = "fill", guide = "colorbar", name = "Cave records", option = "viridis", direction = -1, end = 0.8, begin = 0.2) +
  scale_colour_viridis_d(aesthetics = "fill2", guide = "legend", name = NULL, option = "magma", begin = 0.9) +
  scale_colour_viridis_c(aesthetics = "fill3", guide = "legend", name = "Pd detections", option = "magma", direction = -1, breaks = c(1,2), begin = 0.5, end = 0.7) +
  
  # Plotting details.
  facet_wrap(~Species, ncol = 2, as.table = F) +
  geom_hline(yintercept = c(2:5-0.5), colour = "grey90", size = 0.2) +
  theme_classic()+
  coord_polar() +
  scale_x_continuous(labels =  month.abb[1:12], breaks = 1:12-0.5) +
  scale_y_continuous(limits = c(0,4.5), breaks = 2:5-0.5) +
  theme(
    legend.direction = "horizontal",
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    strip.background = element_blank(),
    # Angle axis text
    axis.text.x = element_text(size = 10, angle = myAng),
    strip.text = element_text(size = 14),
    legend.position = c(0.7, 1), legend.justification = c(0, 1)
  ) 
