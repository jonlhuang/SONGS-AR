#### Data exploration of SONGS size structured data


####-----load libraries-----
library(tidyverse)
library(here)
library(patchwork)
library(ggpubr)
library(ggh4x) #additional axis 

####-------functions-----

se <- function(x) sd(x)/sqrt(length(x))

#--- YOY data
fishYOY <- function(data,species,length) {
  data %>% 
    filter(species_code == {{species}},
           total_length<=length) %>% 
    uncount(count)%>% 
    mutate(n = 1) %>% 
    group_by(year, species_code, reef_code, transect_code, total_length) %>% 
    summarise(n = sum(n))
} #number of fish at size at site at year at transect

fishYOY_sum <- function(data,species,length,reef) { 
  data %>% 
    filter(species_code == {{species}},
           total_length<=length,
           reef_code%in%{{reef}}) %>% 
    uncount(count) %>% 
    mutate(count = 1) %>% 
    group_by(year) %>% 
    summarize(count = sum(count))
}#summarize the abundance of YOY by size 

yoy <- function(data,species,size){
  wnr <- fishYOY_sum(data, species, size, "WNR")#take YOY aboundance from every site 
  # %>% 
  #  mutate( count = case_when(.$year %in% c(2000:2006) == NA ~ 0, 
  #             TRUE ~ fishYOY_sum(data, species, size, "WNR")))

  bk <- fishYOY_sum(data, species, size, "BK")
  
  smk <- fishYOY_sum(data, species, size, "SMK")
  year <- data_frame(year = as_factor(c(2000:2021))) #make a list with all years to show every year
  YOY <- list(wnr,smk,bk) #make all sites into one list
  
df <-   left_join(year, #join year to have all years, with combined list of all reefs
            YOY %>% 
    reduce(full_join, #combine list using reduce with function "full_join" to have all reefs on one df
           by = "year") %>% 
    rename("WNR" = count.x, #rename columns to site names
           "SMK" = count.y ,
           "BK" = count))

#edit df to have 0 on counts and NA on data without data
df2 <-  df %>% 
  pivot_longer(cols = WNR:BK, #make longer to do it all at once
               names_to = "reef",
               values_to = "count") %>% 
  mutate(count = if_else(year %in% c(2000:2006) & is.na(count), 0, 
                 if_else(
                   year %in% c(2009:2021) & is.na(count), 0,
                   if_else(
                     year %in% c(2007:2008) & is.na(count), NA,
                     count
                   )))) %>% 
  pivot_wider(names_from = reef,
              values_from = count)
  

} #summarize the total abundundance of YOY by year


#--- Resident (Juvenile + Adult data)
fishRes <- function(data,species,length) {
  data %>% 
    filter(species_code == {{species}},
           total_length>=length) %>% 
    uncount(count)%>% 
    mutate(n = 1) %>% 
    group_by(year, species_code, reef_code, transect_code, total_length) %>% 
    summarise(n = sum(n))
} #number of fish at size at site at year at transect

fishRes_sum <- function(data,species,length,reef) { 
  data %>% 
    filter(species_code == {{species}},
           total_length >=length,
           reef_code%in%{{reef}}) %>% 
    uncount(count) %>% 
    mutate(count = 1) %>% 
    group_by(year) %>% 
    summarize(count = sum(count))
}#summarize the abundance of YOY by size 

#--- graph of all species on one
ar_nr <- function(data,reefar,xlim,reefnr, title){
  p1 <- ggplot(data %>% filter(reef_code%in% {{reefar}}) %>% 
                 mutate(year = as_factor(year)),    #data 
               aes(x = total_length, color = year))+   #specify x & solor by year
    geom_density(linewidth = 1.5)+   #use density plot
    facet_wrap(~species_code,ncol=1, scales = "free")+    #separate plots by species
    ggh4x::facetted_pos_scales(x = NULL, y = list( #custom scales for each facet
      SEPU = scale_y_continuous(limits = c(0,0.075)),
      PACL = scale_y_continuous(limits = c(0,0.15)),
      PANE = scale_y_continuous(limits = c(0,0.24)),
      EMJA = scale_y_continuous(limits = c(0,0.15)),
      CHPU = scale_y_continuous(limits = c(0,1.0)),
      OXCA = scale_y_continuous(limits = c(0,1.0))
    ))+
    scale_x_continuous(breaks = seq(0,xlim, 10),
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

  p2 <- data %>% filter(reef_code%in% reefnr) %>% 
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
      CHPU = scale_y_continuous(limits = c(0,1.0)),
      OXCA = scale_y_continuous(limits = c(0,1.0))
    ))+
    scale_x_continuous(breaks = seq(0,xlim, 10),
                       limits = c(0,NA))+  #scale x to be max lenght
    labs(x = "Total Length (mm)",    #edit axis & legend labels
         y = "Density", 
         color = "Year",
         title = element_text("Size distribution on Natural Reefs (BK & SMK)"))+
    # scale_color_viridis_d()+  #color palatte
    scale_fill_manual( values = c("#440154","#481467", "#482576","#453781","#404688","#39558c", #manually fill in legend
                                           "#33638d","#2d718e","#287d8e","#238a8d","#1f968b",
                                           "#20a386","#29af7f","#3dbc74","#56c667","#75d054","#95d840",
                                           "#bade28","#dde318","#fde725"),
                                           aesthetics = c("color", "fill"),
                       name = "Year")+
    theme_classic()+ #edit theme
    theme(panel.background = element_rect(fill = "grey90", color = "black"),
          plot.title = element_text(hjust = 0.5, size = 40),
          axis.text = element_text(size = 30),
          axis.title = element_text(size = 17),
          strip.text = element_text(size = 30), #facet font size 
          legend.text = element_text(size = 30), #legend font size
          legend.key.size = unit(1.5,"cm"), #legend key size
          legend.title = element_text(size = 50))
  p2
  
  arrange <- ggarrange((p1+rremove("xlab")+rremove("ylab")+rremove("legend"))+(p2+rremove("ylab")+rremove("xlab")), #edit which axis labels to keep
                       labels = NULL, 
                       common.legend = TRUE, #have one single legend for both garphs
                       legend = "right")
  annotate_figure(arrange, 
                  left = text_grob("Density", #add common axis
                                   rot = 90, 
                                   size = 45),
                  bottom = text_grob("Total Length (mm)",
                                     size = 45))+
    theme(plot.margin = margin(1,1,1,1, "cm")) #add margin space
  ggsave(here("output", title),
         width = 25, height = 33)
}

#---- graph of individual species
species_reefs <- function(data,species, reefar,xlim,ylim, yinterval,
                          reefbk,reefsmk, reefnr, title, arnr_filename, reefs_filename){
  p1 <- ggplot(songs_count %>% filter(reef_code%in% reefar,
                                      species_code%in% {{species}})%>% 
                 mutate(year = as_factor(year)),    #data 
               aes(x = total_length, fill = year))+   #specify x & solor by year
    geom_density()+  #use density plot
    facet_wrap(~year, ncol = 1, scales = "free_y")+    #separate plots by species
    scale_x_continuous(breaks = seq(0,xlim, 10),
                       limits = c(0,NA))+  #scale x to be max lenght at increment of 5cm
    scale_y_continuous(breaks = seq(0,ylim, yinterval),
                       limits = c(0,ylim))+ #scale y tick mark
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
  p1
  
  p2 <- ggplot(data %>%  #both natural reefs
                 filter(reef_code%in% c({{reefnr}}),
                        species_code%in% {{species}}) %>% 
                 mutate(year = as_factor(year)),    #data 
               aes(x = total_length, fill = year))+   #specify x & solor by year
    geom_density()+   #use densiity plot
    facet_wrap(~year, ncol = 1, scales = "free_y")+    #separate plots by species
    scale_x_continuous(breaks = seq(0,xlim, 10),
                       limits = c(0,NA))+  #scale x to be max length
    scale_y_continuous(breaks = seq(0,ylim, yinterval),
                       limits = c(0,ylim))+ #scale y tick mark
    labs(x = "Total Length (mm)",    #edit axis & legend labels
         y = "Density", 
         fill = "Year",
         title = element_text("Natural Reefs"))+
    scale_fill_viridis_d()+  #color palatte
    theme_classic()+
    theme(panel.background = element_rect(fill = "grey96", color = "black"),
          plot.title = element_text(hjust = 0.5),
          panel.spacing = unit(1, "mm"),
          strip.text = element_blank(),
          legend.position = "none")
  
  p2.1 <- ggplot(songs_count %>% 
                   filter(reef_code%in% reefbk,
                          species_code%in%species) %>% 
                   mutate(year = as_factor(year)),    #data 
                 aes(x = total_length, fill = year))+   #specify x & solor by year
    geom_density()+   #use densiity plot
    facet_wrap(~year, ncol = 1, scales = "free_y")+    #separate plots by species
    scale_x_continuous(breaks = seq(0,xlim, 10),
                       limits = c(0,NA))+  #scale x to be max length
    scale_y_continuous(breaks = seq(0,ylim, yinterval),
                       limits = c(0,ylim))+ #scale y tick mark
    labs(x = "Total Length (cm)",    #edit axis & legend labels
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
  
  p2.2 <- ggplot(songs_count %>% filter(reef_code%in% reefsmk,
                                        species_code%in%species)%>% 
                   mutate(year = as_factor(year)),    #data 
                 aes(x = total_length, fill = year))+   #specify x & solor by year
    geom_density()+   #use densiity plot
    facet_wrap(~year, ncol = 1, scales = "free_y")+    #separate plots by species
    scale_x_continuous(breaks = seq(0,xlim, 10),
                       limits = c(0,NA))+  #scale x to be max length
    scale_y_continuous(breaks = seq(0,ylim, yinterval),
                       limits = c(0,ylim))+ #scale y tick mark
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
  
  #combine both plots - by ar/nr
  combined <- ggarrange(p1+
                          p2+ rremove("ylab"),
                        # p2.2+rremove("ylab")+
                        # p2.1+rremove("ylab"),
                        common.legend = TRUE, 
                        legend = "right")
  #add title at the top of the graph
  annotate_figure(combined,
                  top = text_grob(title, 
                                  face = "bold",
                                  size = 14))
  
  ggsave(here("output", arnr_filename),
         width = 9, height = 6)

#Individual reefs
  combined <- ggarrange(p1+
                        p2.2+rremove("ylab")+
                        p2.1+rremove("ylab"),
                        common.legend = TRUE, 
                        legend = "right")
  #add title at the top of the graph
  annotate_figure(combined,
                  top = text_grob(title, 
                                  face = "bold",
                                  size = 14))
  
  ggsave(here("output", reefs_filename),
         width = 9, height = 6)
}



####------------import data ----------####
songs <- read.csv(here("data","songs_clean.csv"))
glimpse(songs)
  
#Data wrangle to have individual fish per row
songs_count <- songs %>% 
  mutate(species_code = as_factor(factor(species_code,   #reorder the factors
                                         levels = c( "SEPU","PACL","PANE","EMJA","CHPU","OXCA"))),
         total_length = as.numeric(total_length), #change length to numeric
         reef_code = as_factor(reef_code),
         year = as.factor(year)) %>%   
  summarise(count = sum(count), 
            .by = c(year,total_length, species_code, reef_code, transect_code)) %>%     #sum the count through the years
  uncount(count) %>%                   #reorder so each row = 1 count
  relocate(year, species_code,reef_code,transect_code, total_length) %>%  #reorder df order
  mutate(count = 1) #make a column so each row has one value

#summarizing the data by year, length, species, reef, and transect
songs_count_sum <- songs_count %>%  
  summarise(count = sum(count), #sum the count through the years
            .by = c(year, species_code, reef_code,transect_code,total_length)) 



#######-----------Exploration for environemnt----------##########

songs_year <- songs_count %>%  #add labels to re or post-blob by size
  drop_na() %>% 
  mutate( blob_case =  case_when(year<2013 ~"pre-blob",
                    year>=2013 & year<=2016 ~"blob",
                    year>=2017 ~"post-blob",
                    TRUE ~NA),
          blob_case = as_factor(factor(blob_case,
                                       levels = c("pre-blob","blob","post-blob")))
  )
    
size_summary <- songs_year %>%  #summarize size by category
  group_by(blob_case,species_code) %>% 
  summarize(mean = mean(total_length),
            sd = sd(total_length),
            se = se(total_length))


####-----Number of YOY-----####
#summary by year
yoy_emja <- yoy(songs_count_sum, "EMJA", 12.7)
yoy_pacl <- yoy(songs_count_sum, "PACL", 7.62)
yoy_chpu <- yoy(songs_count_sum, "CHPU", 7.62)
yoy_sepu <- yoy(songs_count_sum, "SEPU", 7.62)
yoy_pane <- yoy(songs_count_sum, "PANE", 7.62)
yoy_oxca<- yoy(songs_count_sum, "OXCA", 7.62) 

#export to one excel sheet
# yoy_tables <- list("sepu" = yoy_sepu, "pane" = yoy_pane,  "pacl" = yoy_pacl, 
#                    "emja" = yoy_emja,"chpu" = yoy_chpu, "oxca" = yoy_oxca)
# openxlsx::write.xlsx(yoy_tables, file = here("output","yoy.xlsx"))

#abundance and size at each transect
yoy_emja_abd <- fishYOY(songs, "EMJA", 12.7) 
yoy_pacl_abd <- fishYOY(songs, "PACL", 7.62)
yoy_chpu_abd <- fishYOY(songs, "CHPU", 7.62)
yoy_sepu_abd <- fishYOY(songs, "SEPU", 7.62)
yoy_pane_abd <- fishYOY(songs, "PANE", 7.62)
yoy_oxca_abd <- fishYOY(songs, "OXCA", 7.62) 


#######-----------Plot----------##########

#all species and plots on an ar_nr
ar_nr(songs_count,"WNR",75,c("BK","SMK"), "AR_NR_density_sizedistribution.pdf")


#SEPU
species_reefs(songs_count, "SEPU", "WNR", 75, 0.1, 0.08, "BK","SMK",c("BK","SMK"),
              "Size distribution of California Sheephead",
              arnr_filename =  "Test_AR_NR_density_distribution.pdf",
              reefs_filename = "Test_site_density_distribution.pdf")

#PACL
species_reefs(songs_count, "PACL", "WNR", 75, 0.2, 0.15, "BK","SMK",c("BK","SMK"),
              "Size distribution of Kelp Bass",
              arnr_filename =  "PACL_AR_NR_density_distribution.pdf",
              reefs_filename = "PACL_site_density_distribution.pdf")

#CHPU
species_reefs(songs_count, "CHPU", "WNR", 75, 1.4, 0.5, "BK","SMK",c("BK","SMK"),
              "Size distribution of Blacksmith",
              arnr_filename =  "CHPU_AR_NR_density_distribution.pdf",
              reefs_filename = "CHPU_site_density_distribution.pdf")

#EMJA
species_reefs(songs_count, "EMJA", "WNR", 75, 0.2, 0.15, "BK","SMK",c("BK","SMK"),
              "Size distribution of Black Perch",
              arnr_filename =  "EMJA_AR_NR_density_distribution.pdf",
              reefs_filename = "EMJA_site_density_distribution.pdf")

#OXCA
species_reefs(songs_count, "OXCA", "WNR", 75, 1.0, 0.4, "BK","SMK",c("BK","SMK"),
              "Size distribution of Señorita",
              arnr_filename =  "OXCA_AR_NR_density_distribution.pdf",
              reefs_filename = "OXCA_site_density_distribution.pdf")

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

