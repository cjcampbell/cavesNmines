require(ggplot2)
require(ggpubr)
p <- ggplot() +
  
  geom_point(
    data = 
      data.frame(
        x = c(0,0,0),
        y = c(1,2,3),
        class = c("Alive", "Dead", "Dead"),
        Species = c("LACI", "LANO", "LABO")
      ),
    aes(x = x , y = y, shape = class, color = Species),
    size = 11, alpha = 0.85, stroke = 1.6
    ) +
  scale_shape_manual(name = "Observation type", values = c(21,23)) +
  scale_color_manual(
    values = rev(viridis::plasma(3)),
    labels = c(
      expression("Hoary bat ("*italic("Lasiurus cinereus")*")"), 
      expression("Eastern red bat ("*italic("Lasiurus borealis")*")"),
      expression("Silver-haired bat ("*italic("Lasionycteris noctivagans")*")")
      )
    ) +
  
  geom_rect(
    data = 
      data.frame(x1=c(1,3,1,5,4), x2=c(2,4,3,6,6), y1=c(1,1,4,1,3), y2=c(2,2,5,3,5), t=rep("USGS Karst areas", 5), r=c(1,2,3,4,5)), 
    aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=t), 
    color=NA
    ) +
  scale_fill_manual(name = NULL, values = "grey90") +
  
  guides(
    color = guide_legend(order = 1, ncol = 1, override.aes = list(shape = 15, size = 8), title.position = "top"),
    shape = guide_legend(order = 2, ncol=1, override.aes = list(size = 8), title.position = "top"),
    fill = guide_legend(order = 3)
  ) +
  
  theme_minimal() +
  theme(
    text = element_text(size = 10),
    legend.position = "bottom",
    legend.text.align = 0,
    axis.text = element_blank(),
    plot.margin = unit(c(0,0,0,0), "in"),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)
  )

# Extract the legend. Returns a gtable
leg <- ggpubr::get_legend(p)

# Convert to a ggplot and print
as_ggplot(leg)

ggsave(filename = file.path(wd$figs, "legend.tif"), device = "tiff", width = 8, height = 2, units = "in",  bg = "transparent")
