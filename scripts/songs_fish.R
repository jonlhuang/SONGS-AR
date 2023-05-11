#### Data exploration of SONGS size structured data


#load libraries
library(tidyverse)
library(here)
library(patchwork)
library(ggpubr)
library(ggh4x) #additional axis 

#function

se <- function(x) sd(x)/sqrt(length(x))
fishYOY <- function(data,species,length,reef) {
  data %>% 
    filter(species_code == {{species}},
           total_length<length,
           reef_code%in%{{reef}}) %>% 
    mutate(count = 1) %>% 
    group_by(year) %>% 
    summarize(count = sum(count))
}
  
  

#import data
songs <- read.csv(here("data","songs_clean.csv"))
glimpse(songs)


#Data analysis
#plot one specie  
# songs %>% group_by( year,total_length, species_code, reef_code) %>% 
#   mutate(species_code = as_factor(factor(species_code, levels = c( "SEPU","PACL","PANE","EMJA","CHPU","OXCA"))),
#          total_length = as.numeric(total_length)) %>% 
#   summarise(count = sum(count)) %>% 
#   filter(species_code == "SEPU") %>% 
#   uncount(count) %>% 
#   ggplot(aes(x = total_length))+
#   geom_histogram(aes(fill = species_code),
#                  binwidth = 1)+
#   geom_freqpoly(binwidth = 3)+
#   scale_x_continuous(breaks = seq(0,max(songs$total_length),5))+
#     facet_wrap(~year)
  
#plot same specie on the same plot
#Data wrangle to have numeric density by size
songs_count <- songs %>% 
  group_by( year,total_length, species_code, reef_code) %>%  #group analysis 
  mutate(species_code = as_factor(factor(species_code,   #reorder the factors
                                         levels = c( "SEPU","PACL","PANE","EMJA","CHPU","OXCA"))),
         total_length = as.numeric(total_length), #change length to numeric
         reef_code = as_factor(reef_code),
         year = as.numeric(year)) %>%    
  summarise(count = sum(count), #sum the count through the years
            total_area_sampled = sum(total_area_sampled)) %>%    
  mutate(size_density = count/total_area_sampled) %>% 
  uncount(count) %>%                   #reorder so each row = 1 count
  select(year, species_code,reef_code,total_length,total_area_sampled, size_density) #reorder df order

#summary data
songs_count_sum <- songs %>% 
  group_by( year,total_length, species_code, reef_code) %>%  #group analysis 
  mutate(species_code = as_factor(factor(species_code,   #reorder the factors
                                         levels = c( "SEPU","PACL","PANE","EMJA","CHPU","OXCA"))),
         total_length = as.numeric(total_length), #change length to numeric
         reef_code = as_factor(reef_code),
         year = as_factor(year)) %>%    
  summarise(count = sum(count), #sum the count through the years
            total_area_sampled = sum(total_area_sampled)) %>%    
  mutate(size_density = count/total_area_sampled)%>% 
  group_by( year,total_length, species_code, reef_code) %>% 
  summarise(size_density = sum(size_density))

#######-----------Exploration----------##########

songs_year <- songs_count %>% 
  drop_na() %>% 
  mutate( blob_case =  case_when(year<2013 ~"pre-blob",
                    year>=2013 & year<=2016 ~"blob",
                    year>=2017 ~"post-blob",
                    TRUE ~NA),
          blob_case = as_factor(factor(blob_case,
                                       levels = c("pre-blob","blob","post-blob")))
  )
    
size_summary <- songs_year %>% 
  group_by(blob_case,species_code) %>% 
  summarize(mean = mean(total_length),
            sd = sd(total_length),
            se = se(total_length))

#Number of YOY
EMJA_YOYSUM <- fishYOY(songs_count_sum, "EMJA", 13, "WNR")
PACL_YOYSUM <- fishYOY(songs_count_sum, "PACL", 7.62, "WNR")
CHPU_YOYSUM <- fishYOY(songs_count_sum, "CHPU", 7.62, "WNR")
SEPU_YOYSUM <- fishYOY(songs_count_sum, "SEPU", 7.62, "WNR")
PANE_YOYSUM <- fishYOY(songs_count_sum, "PANE", 7.62, "WNR")
OXCA_YOYSUM <- fishYOY(songs_count_sum, "OXCA", 7.62, "WNR")

#######-----------Plot----------##########
# #Plot size distribution on WNR
# p1 <- ggplot(songs_count %>% filter(reef_code%in% "WNR"),    #data 
#        aes(x = total_length, color = year))+   #specify x & solor by year
#   geom_freqpoly(binwidth = 3)+   #use frequency polygon
#     facet_wrap(~species_code, scales = "free")+    #separate plots by species
#   scale_x_continuous(breaks = seq(0,max(songs$total_length), 5),
#                      limits = c(0,NA))+  #scale x to be max lenght at increment of 5cm
#   labs(x = "Total Length (mm)",    #edit axis & legend labels
#        y = "Count", 
#        color = "Year",
#        title = element_text("Size distribution on WNR"))+
#   scale_color_viridis_d()+  #color palatte
#   theme_classic()+
#   theme(panel.background = element_rect(fill = "grey96", color = "black"),
#         plot.title = element_text(hjust = 0.5),
#         legend.position = "none")
#   # ggsave(here("Plots", "size_distribution_WNR.pdf"),
#   #        width = 20, height = 10)
# 
# #plot size distribution on natural reefs
# p2 <- ggplot(songs_count %>% filter(reef_code%in% c("BK","SMK")),    #data 
#        aes(x = total_length, color = year))+   #specify x & solor by year
#   geom_freqpoly(binwidth = 3)+   #use frequency polygon
#   facet_wrap(~species_code, scales = "free")+    #separate plots by species
#   scale_x_continuous(breaks = seq(0,max(songs$total_length), 5),
#                      limits = c(0,NA))+  #scale x to be max lenght
#   labs(x = "Total Length (mm)",    #edit axis & legend labels
#        y = "Count", 
#        color = "Year",
#        title = element_text("Size distribution on BK & SMK"))+
#   scale_color_viridis_d()+  #color palatte
#   theme_classic()+
#   theme(panel.background = element_rect(fill = "grey96", 
#                                         color = "black"),
#         plot.title = element_text(hjust = 0.5))
# # ggsave(here("Plots", "size_distribution_BK_SMK.pdf"),
# #        width = 20, height = 10)
# 
# 
# p1/p2 #use patchwork to stack plots
# ggarrange((p1+rremove("xlab"))/p2, common.legend = TRUE, legend = "right")
# ggsave(here("Plots", "AR_NR_stacked_sizedistribution.pdf"),
#        width = 15, height = 10)


#####------UPDATE WITH DENSITY PLOTS - AR & NR------
p1 <- ggplot(songs_count %>% filter(reef_code%in% "WNR") %>% 
               mutate(year = as_factor(year)),    #data 
             aes(x = total_length, color = year))+   #specify x & solor by year
  geom_density(linewidth = 1.5)+   #use density plot
  facet_wrap(~species_code,ncol=1, scales = "free")+    #separate plots by species
  ggh4x::facetted_pos_scales(x = NULL, y = list( #custom scales for each facet
    SEPU = scale_y_continuous(limits = c(0,0.075)),
    PACL = scale_y_continuous(limits = c(0,0.15)),
    PANE = scale_y_continuous(limits = c(0,0.24)),
    EMJA = scale_y_continuous(limits = c(0,0.15)),
    CHPU = scale_y_continuous(limits = c(0,0.9)),
    OXCA = scale_y_continuous(limits = c(0,1.0))
  ))+
  scale_x_continuous(breaks = seq(0,max(songs$total_length), 10),
                     limits = c(0,NA))+  #scale x to be max lenght at increment of 5cm
  labs(x = "Total Length (mm)",    #edit axis & legend labels
       y = "Density", 
       color = "Year",
       title = element_text("Size distribution on Artifical Reef (WNR)"))+
  scale_color_viridis_d()+  #color palatte
  theme_classic()+
  theme(panel.background = element_rect(fill = "grey90", color = "black"),
        plot.title = element_text(hjust = 0.5, size = 40),
        axis.text = element_text(size = 30),
        axis.title = element_text(size = 17),
        strip.text = element_text(size = 30),
        legend.text = element_text(size = 30),
        legend.key.size = unit(1.5,"cm"),
        legend.title = element_text(size = 50)) #remove lengend for ggarrange
p1
# ggsave(here("output", "size_densitydistribution_WNR.pdf"),
#        width = 20, height = 10)

p2 <- songs_count %>% filter(reef_code%in% c("BK","SMK")) %>% 
  mutate(year = as_factor(year)) %>% 
  ggplot()+   #specify x & solor by year
  geom_density(
    aes(x = total_length, color = year, group = year),linewidth = 1.5)+   #use density plot
  facet_wrap(~species_code, ncol=1, scales = "free")+ #separate plots by species
  ggh4x::facetted_pos_scales(x = NULL, y = list( #custom scales for each facet
    SEPU = scale_y_continuous(limits = c(0,0.075)),
    PACL = scale_y_continuous(limits = c(0,0.15)),
    PANE = scale_y_continuous(limits = c(0,0.24)),
    EMJA = scale_y_continuous(limits = c(0,0.15)),
    CHPU = scale_y_continuous(limits = c(0,0.9)),
    OXCA = scale_y_continuous(limits = c(0,1.0))
  ))+
  scale_x_continuous(breaks = seq(0,max(songs$total_length), 10),
                     limits = c(0,NA))+  #scale x to be max lenght
  labs(x = "Total Length (mm)",    #edit axis & legend labels
       y = "Density", 
       color = "Year",
       title = element_text("Size distribution on Natural Reefs (BK & SMK)"))+
  # scale_color_viridis_d()+  #color palatte
  scale_fill_manual( values = c("#440154","#481f70","#443983",
                                        "#3b528b","#31688e","#287c8e",
                                       "#21918c","#20a486", "#35b779",
                                       "#5ec962", "#90d743","#c8e020",
                                       "#fde725"),
                    aesthetics = c("color", "fill"),
                    name = "Year")+
  theme_classic()+
  theme(panel.background = element_rect(fill = "grey90", color = "black"),
        plot.title = element_text(hjust = 0.5, size = 40),
        axis.text = element_text(size = 30),
        axis.title = element_text(size = 17),
        strip.text = element_text(size = 30),
        legend.text = element_text(size = 30),
        legend.key.size = unit(1.5,"cm"),
        legend.title = element_text(size = 50))

p2
# ggsave(here("output", "size_densitydistribution_BK_SMK.pdf"),
#        width = 20, height = 10)

# COMBINE THE PLOTS TO HAVE ONE PLOT
arrange <- ggarrange((p1+rremove("xlab")+rremove("ylab")+rremove("legend"))+(p2+rremove("ylab")+rremove("xlab")),
          labels = NULL,
          common.legend = TRUE,
          legend = "right")
annotate_figure(arrange, 
                left = text_grob("Density", #add common axis
                                rot = 90, 
                                size = 45),
                bottom = text_grob("Total Length (mm)",
                                  size = 45))+
  theme(plot.margin = margin(1,1,1,1, "cm"))
# ggsave(here("output", "scaledAR_NR_density_sizedistribution.pdf"),
#        width = 25, height = 33)


##### ---------------- Density plot of individual reefs----

p2.1 <- songs_count %>% filter(reef_code%in% c("BK")) %>% 
  mutate(year = as_factor(year)) %>% 
  ggplot()+   #specify x & solor by year
  geom_density(
    aes(x = total_length, color = year, group = year),linewidth = 1.5)+   #use density plot
  facet_wrap(~species_code, ncol=1, scales = "free")+    #separate plots by species
  scale_x_continuous(breaks = seq(0,max(songs$total_length), 10),
                     limits = c(0,NA))+  #scale x to be max lenght
  labs(x = "Total Length (mm)",    #edit axis & legend labels
       y = "Density", 
       color = "Year",
       title = element_text("BK"))+
  # scale_color_viridis_d()+  #color palatte
  scale_fill_manual( values = c("#440154","#481f70","#443983",
                                         "#3b528b","#31688e","#287c8e",
                                         "#21918c","#20a486", "#35b779",
                                         "#5ec962", "#90d743","#c8e020",
                                         "#fde725"),
                                         aesthetics = c("color", "fill"),
                     name = "Year")+
  theme_classic()+
  theme(panel.background = element_rect(fill = "grey90", color = "black"),
        plot.title = element_text(hjust = 0.5, size = 40),
        axis.text = element_text(size = 30),
        axis.title = element_text(size = 17),
        strip.text = element_text(size = 30),
        legend.text = element_text(size = 30),
        legend.key.size = unit(1.5,"cm"),
        legend.title = element_text(size = 50))

p2.2 <- songs_count %>% filter(reef_code%in% c("SMK")) %>% 
  mutate(year = as_factor(year)) %>% 
  ggplot()+   #specify x & solor by year
  geom_density(
    aes(x = total_length, color = year, group = year),linewidth = 1.5)+   #use density plot
  facet_wrap(~species_code, ncol=1, scales = "free")+    #separate plots by species
  scale_x_continuous(breaks = seq(0,max(songs$total_length), 10),
                     limits = c(0,NA))+  #scale x to be max lenght
  labs(x = "Total Length (mm)",    #edit axis & legend labels
       y = "Density", 
       color = "Year",
       title = element_text("SMK"))+
  # scale_color_viridis_d()+  #color palatte
  scale_fill_manual( values = c("#440154","#481f70","#443983",
                                         "#3b528b","#31688e","#287c8e",
                                         "#21918c","#20a486", "#35b779",
                                         "#5ec962", "#90d743","#c8e020",
                                         "#fde725"),
                                         aesthetics = c("color", "fill"),
                     name = "Year")+
  theme_classic()+
  theme(panel.background = element_rect(fill = "grey90", color = "black"),
        plot.title = element_text(hjust = 0.5, size = 40),
        axis.text = element_text(size = 30),
        axis.title = element_text(size = 17),
        strip.text = element_text(size = 30),
        legend.text = element_text(size = 30),
        legend.key.size = unit(1.5,"cm"),
        legend.title = element_text(size = 50))



arrange <- ggarrange((p1+rremove("xlab")+rremove("ylab")+rremove("legend"))+
                       (p2.1+rremove("ylab")+rremove("xlab")+rremove("legend"))+
                       (p2.2+rremove("ylab")+rremove("xlab")),
                     labels = NULL,
                     common.legend = TRUE,
                     legend = "right")
annotate_figure(arrange, 
                left = text_grob("Density", #add common axis
                                 rot = 90, 
                                 size = 45),
                bottom = text_grob("Total Length (mm)",
                                   size = 45))+
  theme(plot.margin = margin(1,1,1,1, "cm"))
ggsave(here("output", "AR_BK_SMK_density_sizedistribution.pdf"),
       width = 25, height = 33)



#####----------------- Density plot of individual species across AR or NR-------

p1 <- ggplot(songs_count %>% filter(reef_code%in% "WNR",
                                    species_code%in% "SEPU")%>% 
               mutate(year = as_factor(year)),    #data 
             aes(x = total_length, fill = year))+   #specify x & solor by year
  geom_density()+  #use density plot
  facet_wrap(~year, ncol = 1, scales = "free_y")+    #separate plots by species
  scale_x_continuous(breaks = seq(0,75, 10),
                     limits = c(0,NA))+  #scale x to be max lenght at increment of 5cm
  scale_y_continuous(breaks = seq(0,0.07, 0.05),
                     limits = c(0,0.07))+ #scale y tick mark
  labs(x = "Total Length (mm)",    #edit axis & legend labels
       y = "Density", 
       fill = "Year",
       title = element_text("Artificial Reef"))+
  scale_fill_viridis_d()+  #color palatte
  theme_classic()+
  theme(panel.background = element_rect(fill = "grey96", color = "black"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        panel.spacing = unit(1, "mm"),
        strip.text = element_blank())

p2 <- ggplot(songs_count %>%  #both natural reefs
                 filter(reef_code%in% c("BK","SMK"),
                        species_code%in%"SEPU") %>% 
                 mutate(year = as_factor(year)),    #data 
               aes(x = total_length, fill = year))+   #specify x & solor by year
  geom_density()+   #use densiity plot
  facet_wrap(~year, ncol = 1, scales = "free_y")+    #separate plots by species
  scale_x_continuous(breaks = seq(0,max(songs$total_length), 10),
                     limits = c(0,NA))+  #scale x to be max length
  scale_y_continuous(breaks = seq(0,0.07, 0.05),
                     limits = c(0,0.07))+ #scale y tick mark
  labs(x = "Total Length (mm)",    #edit axis & legend labels
       y = "Density", 
       fill = "Year",
       title = element_text("Natural Reef"))+
  scale_fill_viridis_d()+  #color palatte
  theme_classic()+
  theme(panel.background = element_rect(fill = "grey96", color = "black"),
        plot.title = element_text(hjust = 0.5),
        panel.spacing = unit(1, "mm"),
        strip.text = element_blank(),
        legend.position = "none")


p2.1 <- ggplot(songs_count %>% 
                 filter(reef_code%in% "BK",
                        species_code%in%"SEPU") %>% 
                 mutate(year = as_factor(year)),    #data 
             aes(x = total_length, fill = year))+   #specify x & solor by year
  geom_density()+   #use densiity plot
  facet_wrap(~year, ncol = 1, scales = "free_y")+    #separate plots by species
  scale_x_continuous(breaks = seq(0,max(songs$total_length), 10),
                     limits = c(0,NA))+  #scale x to be max length
  scale_y_continuous(breaks = seq(0,0.12, 0.1),
                     limits = c(0,0.12))+ #scale y tick mark
  labs(x = "Total Length (mm)",    #edit axis & legend labels
       y = "Density", 
       fill = "Year",
       title = element_text("BK"))+
  scale_fill_viridis_d()+  #color palatte
  theme_classic()+
  theme(panel.background = element_rect(fill = "grey96", color = "black"),
        plot.title = element_text(hjust = 0.5),
        panel.spacing = unit(1, "mm"),
        strip.text = element_blank(),
        legend.position = "none")

p2.2 <- ggplot(songs_count %>% filter(reef_code%in% "SMK",
                                      species_code%in%"SEPU")%>% 
                 mutate(year = as_factor(year)),    #data 
             aes(x = total_length, fill = year))+   #specify x & solor by year
  geom_density()+   #use densiity plot
  facet_wrap(~year, ncol = 1, scales = "free_y")+    #separate plots by species
  scale_x_continuous(breaks = seq(0,max(songs$total_length), 10),
                     limits = c(0,NA))+  #scale x to be max length
  scale_y_continuous(breaks = seq(0,0.12, 0.1),
                     limits = c(0,0.12))+ #scale y tick mark
  labs(x = "Total Length (mm)",    #edit axis & legend labels
       y = "Density", 
       fill = "Year",
       title = element_text("SMK"))+
  scale_fill_viridis_d()+  #color palatte
  theme_classic()+
  theme(panel.background = element_rect(fill = "grey96", color = "black"),
        plot.title = element_text(hjust = 0.5),
        panel.spacing = unit(1, "mm"),
        strip.text = element_blank(),
        legend.position = "none")

#combine both plots
combined <- ggarrange(p1+
                      p2+ rremove("ylab"),
                        # p2.2+rremove("ylab")+
                        # p2.1+rremove("ylab"),
                      common.legend = TRUE, 
                      legend = "right")
#add title at the top of the graph
annotate_figure(combined,
                top = text_grob("Size Distrbution of California Sheephead", 
                                face = "bold",
                                size = 14))

ggsave(here("output", "SEPU_AR_NR_density_distribution.pdf"),
       width = 9, height = 6)

#####-------Size simmary with before-after-heatwave-----

size_summary %>% 
  ggplot(aes(x = species_code,y = mean))+
  geom_bar(aes(group = blob_case,
               fill = blob_case),
           position = "dodge",
           stat = "identity")+
  geom_errorbar(aes(x = species_code,ymin = mean-se, ymax = mean+se,
                  group = blob_case ), 
                position = position_dodge(0.95)
)+
  labs(y = "mean Total Length (mm)",
       x = element_blank(),
       fill = "Blob stage")+
  scale_fill_manual(values = c("coral", "darkred","lightblue"))+
  theme_bw()

