

#### Scatter Plot: GPI vs ABR ####

data_map <- bi_class(data_analysis %>% 
                       filter(Year %in% c(2023),!is.na(gpi_overall)), 
                     #group_by(sex, age),
                     x = gpi_overall, 
                     y = F15, 
                     style = "quantile", dim = 3)



scatter_plot = data_map %>%
  ggplot()+
  geom_point(aes(x=gpi_overall,y=F15,fill=bi_class,color=bi_class),size=3)+
  geom_smooth(mapping=aes(x=gpi_overall,y=F15),color='red',method = "lm", se = FALSE)+
  #scale_x_continuous(labels=scales::percent) +
  ggthemes::theme_economist_white()+
  bi_scale_fill(pal = "DkCyan", dim = 3)+
  bi_scale_color(pal = "DkCyan", dim = 3)+
  #ylim(c(1,4))+
  xlab("GPI")+
  ylab("ABR")+
  theme(legend.position = "none",
        plot.title = element_text(size = 30, face = "bold"),
        legend.text = element_text(size = 30, face = "bold"),
        axis.text.y = element_text(size = 30, face = "bold"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 30, face = "bold"),
        strip.text.y = element_text(size = 30, face = "bold"),
        strip.text.x = element_text(size = 30, face = "bold"),
        axis.title.y = element_text(face = "bold", size = 30),
        axis.title.x = element_text(face = "bold", size = 30))


#### Bivariate Map: GPI vs ABR ####

# extract world coordinates
data(World)


# checking coordinate system
st_crs(World)

#changing to Robinson system
world_rob<-st_transform(World, "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

 # joining data with shape file
setnames(world_rob, "iso_a3", "iso3")
names(world_rob)[1] = "iso3"
data_map_full <-left_join(data_map, world_rob, by="iso3")
 
data_map_full <- data_map_full %>% ungroup() %>%  st_as_sf()

map_plot = ggplot()+
  geom_sf(data = data_map_full ,  
          mapping = aes(fill = bi_class),
          color = "grey50", size = 0.1, show.legend = F) +
  bi_scale_fill(pal = "DkCyan", dim = 3)+
  #ggtitle("Adolescent Birth Rate and Overall Level of Violence, 2023")+
  theme_minimal()+
  theme(axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text(hjust = 0.5),
        title=element_text(face = 'bold',size=50),
        text = element_text(family = "serif"),
        panel.background = element_rect(fill = "white", color = NA), # Pink background
        plot.background = element_rect(fill = "white", color = NA)   # Pink background
        
        )+
  geom_curve(aes(x = -5867671, 
                 y = 6295418, 
                 xend = -3067671, 
                 yend =5000000 ), 
             curvature = -0.3,
             arrow = arrow(length = unit(0.04, "npc")), 
             size=1.6, color="black") +
  annotate(geom="text", 
           x=-3267671, 
           y=4100000, 
           label="Low level of violence\n and low teen fertility",
           size=20,
           fontface = "bold",
           family='serif')+
  geom_curve(aes(x = 9650000,  # starting point (adjust as needed)
                 y = 2150000, 
                 xend = 12200000, # endpoint near Laos (adjust as needed)
                 yend = 2200000),
    curvature = -0.3,
    arrow = arrow(length = unit(0.04, "npc")), 
    size=1.6,color="black") +
  annotate(geom="text", 
           #x=7031144, 
           #y=-1206450,
           x=14300000, 
           y=2400000, 
           label="Low level of violence and\n high teen fertility", 
           size=20,
           fontface = "bold",
           family='serif')+
  geom_curve(aes(x = 3500000,  # Move origin near Mozambique
                 y = -1500000, 
                 xend = 5037690,  # Adjusted to Mozambique location
                 yend = -4106450),
             curvature = 0.3,
             arrow = arrow(length = unit(0.04, "npc")), 
             size=1.6, 
             color="black") +
  annotate(geom="text", 
           x=5637690, 
           y=-4806450, 
           label="High level of violence\n and high teen fertility", 
           size=20,
           fontface = "bold",
           family='serif')+
  geom_curve(aes(x = 1700000,  # Approximate longitude of Libya
                 y = 2700000,  # Approximate latitude of Libya
                 xend = -3067671,  # Longitude in the South Atlantic Ocean near Brazil
                 yend = -3000000),  # Latitude in the ocean, closer to Brazil
             curvature = 0.5,
             arrow = arrow(length = unit(0.04, "npc")), 
             size = 1.6, color = "black") +  # Bright green color
  
  # Text annotation placed entirely in the Atlantic Ocean
  annotate(geom = "text", 
           x = -2200071,  # Adjusted x-position further west into the ocean
           y = -3900000,  # Adjusted y-position to ensure it's fully in the ocean
           label = "High level of violence\n and low teen fertility", 
           size = 20,
           fontface = "bold",
           family='serif')


legend = bi_legend(pal = "DkCyan",
                   dim = 3,
                   xlab = " \nLevel \nof Violence",
                   ylab = " \n Adolescent Birth Rate",
                   size = 25)+
  theme(plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent"),
        legend.text = element_text(size = 20),    # Increase legend text size
        legend.title = element_text(size = 16),   # Increase legend title size
        title = element_text(face='bold'),
        text = element_text(family = "serif"),
        axis.title = element_text(size = 50,face = 'bold')     # Increase axis text size
        )


#### Bivariate Legend ####

legend = bi_legend(pal = "DkCyan",
                   dim = 3,
                   xlab = "GPI",
                   ylab = "ABR",
                   size = 5)+
  theme(plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent"),
        legend.text = element_text(size = 14),    # Increase legend text size
        legend.title = element_text(size = 16),   # Increase legend title size
        title = element_text(face='bold'),
        text = element_text(family = "serif"),
        axis.title = element_text(size = 50,face = 'bold')     # Increase axis text size
  )





#### Regression Results ####







# Plot regression coefficients from subregional models
plot_coef_subregions = ggplot(data_plot %>% filter(subregion!='World'), 
                              mapping=aes(y=subregion,x = estimate, color = term)) +
  geom_point(aes(shape=term),
             position = position_dodge(width = 4),
             size=18) +
  geom_errorbar(aes(xmin = conf.low, 
                    xmax = conf.high), 
                linewidth = 3, 
                width = 0, 
                position = position_dodge(width = 4)) +
  scale_y_discrete(expand = expansion(mult = c(3, 3))) + 
  scale_x_continuous(breaks=seq(-40,30,10), labels=seq(-40,30,10),limits = c(-40,30)) +
  coord_flip() +  # Flip axes for better readability
  labs(title = "",
       y = "",
       x = "Estimated Association") +
  ggthemes::theme_economist_white()+
  geom_vline(mapping=aes(xintercept=0),color='black',size=2)+
  scale_color_manual(
    values = c('Conflict' = '#FF0033', 'Safety and Security' = '#9900FF', 'Overall Violence' = '#0099FF'),
    labels = c("Conflict", "Safety and Security", "Overall Violence")
  )+
  scale_shape_manual(
    values = c('Conflict' = 0, 'Safety and Security' = 2, 'Overall Violence' = 13),
    labels = c("Conflict", "Safety and Security", "Overall Violence")
  )+
  facet_wrap(~subregion,ncol=4,scale='free_x')+
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = "white"),
        legend.background = element_rect(fill='white'),
        text = element_text(family = "serif"),
        legend.title = element_blank(),
        panel.spacing.y = unit(2, "cm"),
        legend.spacing.y = unit(8.0, 'cm'),
        legend.spacing.x = unit(8.0, 'cm'),
        aspect.ratio = 1,
        plot.title = element_text(size = 70, face = "bold"),
        legend.text = element_text(size = 90, face = "bold"),
        legend.box.margin = margin(t = 10, b = 10),
        axis.text.y = element_text(size = 70, face = "bold",margin = margin(r = 20, l = 0, t = 0, b = 0)),
        axis.text.x = element_blank(),
        strip.text.y = element_text(size = 70, face = "bold"),
        strip.text.x = element_text(size = 70, face = "bold"),
        axis.title.x = element_text(face = "bold", size = 75,margin = margin(r = 20, l = 0, t = 0, b = 0)),
        axis.title.y = element_text(face = "bold", size = 75,margin = margin(r = 20, l = 0, t = 0, b = 0)))










ggsave(plot_poster,file='Figures_PAA/plot_coef.pdf', height = 90, width = 125,units = 'cm',dpi=700)
ggsave(plot_map,file='Figures_PAA/map_poster_new.pdf', height = 65, width = 125,units = 'cm',dpi=700)


  