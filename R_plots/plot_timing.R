
require(dplyr)

ord_species <- c("LACI", "LABO", "LANO")

timing_records <- readRDS( file.path(wd$bin, "records_coded_tidy.rds") ) %>% 
  filter(def_alive == "Yes") %>% 
  dplyr::mutate(Month = stringr::str_trim(Month)) %>% 
  dplyr::mutate(tidy_month = if_else(Month == "7-8", as.numeric(7), if_else(Month == "Two winters", as.numeric(1), as.numeric(Month)))) %>% 
  dplyr::mutate(Species = factor(Species, levels = ord_species))


timing_records %>% 
  ggplot() +
  geom_violin(aes(y = tidy_month, x = Species) ) 

timing_records %>% 
  ggplot() +
  geom_density(aes(tidy_month, group = Species, fill = Species), alpha = 0.5) +
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
  )

ggsave(filename = file.path(wd$fig, "density_by_month.tif"), device = "tiff", width = 10, height = 4.65, units = "in", bg = "transparent")

