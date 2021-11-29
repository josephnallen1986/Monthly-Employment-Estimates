library(RSocrata)     # Download or Upload 'Socrata' Data Sets
library(tidyverse)    # Collection of R packages designed for data science
library(scales)       # Additional graphics functions
library(RColorBrewer) # Color ramps for graphs and maps
library(gridExtra)    # Functions for arranging multiple plots on a page
library(Cairo)        # Create high-quality vector (PDF, PostScript and SVG) and bitmap output
library(zoo)          # Moving averages     
library(lubridate)    # Makes it easier to work with dates and times.
library(ggpubr)       # Functions for creating and customizing 'ggplot2'
library(ggrepel)      # Provides text and label geoms for 'ggplot2' that help to avoid overlapping text labels. 

ces <- read.socrata(
  "https://data.edd.ca.gov/resource/r4zm-kdcg.json?$where=seasonally_adjusted = 'N' AND year >= '2020'") %>%
  filter(
    area_type == "Metropolitan Area" &
      (series_code == "10000000" | series_code == "20000000" | series_code == "30000000" |
         series_code == "40000000" | series_code == "50000000" | series_code == "55000000" |
         series_code == "60000000" | series_code == "65000000" | series_code == "70000000" |
         series_code == "80000000" | series_code == "90000000")) %>%
  select(msa = area_name, month = date, industry_title, estimated_employment = current_employment) %>%
  group_by(msa, industry_title) %>%
  mutate(
    estimated_employment = as.numeric(estimated_employment),
    month = as.Date(month),    
    previous_month = lag(estimated_employment),
    previous_year = lag(estimated_employment, 12),
    mom = estimated_employment - previous_month,
    yoy = estimated_employment - previous_year,
    mom_pct = round((mom/previous_month)* 100, 2),
    yoy_pct = round((yoy/previous_year)* 100, 2)) %>%
  filter(month == max(month)) 

current_month <- paste(
  months(as.Date(first(ces$month)))," ",
  year(first(ces$month)),sep="")

df <- ces %>%
  pivot_longer(
    cols=c("mom_pct", "yoy_pct"), 
    names_to="variable",
    values_to="pct_change") %>%
  select(msa, industry_title, variable, pct_change)

d <- paste(getwd(),"/Output/",format(first(ces$month), "%y-%m")," ",month.abb[month(first(ces$month))],sep="")
dir.create(d, showWarnings = FALSE)

msa_list <- unique(ces$msa)

for (selected_msa in msa_list) {
  
  df1 <- filter(df, msa == selected_msa)
  df2 <- filter(ces, msa == selected_msa)
  
  pct_change_graph <- ggplot(
    data = df1, 
    aes(
      str_wrap(industry_title, width = 25), 
      pct_change, 
      fill = variable)) +
    geom_col(position = "dodge") +
    geom_text(
      aes(
        label = paste(round(pct_change,1),"%",sep=""),
        hjust = 1,
        vjust = 0.5,
        colour = ifelse(pct_change >=5, "white", "black")),
      position = position_dodge(width = .9),
      show.legend = FALSE) +
    scale_y_continuous(      
      limits = c(
        round(min(df1$pct_change, na.rm = TRUE))-5, 
        round(max(df1$pct_change, na.rm = TRUE))+5)) +
    scale_x_discrete(limits = rev) +
    scale_fill_manual(
      labels = c("Month-over % Change", "Year-Over % Change"),
      values = c("#005f7c","#c67f07"))+
    coord_flip() +
    scale_color_manual(
      values = c("black" = "black", "white" = "white"), 
      guide = "none") +
    labs(y="Perecent Change") +
    theme(text = element_text(colour = "#000000", size=14),
          title = element_text(color = "#00587C"),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.background = element_rect(fill = NA),
          plot.background = element_rect(fill = "#FFFFFF"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          legend.background = element_blank(),
          legend.key = element_blank(),
          legend.position = "top",
          plot.title = element_blank(),
          plot.subtitle = element_blank(),
          axis.title=element_text(size=12,face="bold"),
          plot.margin=unit(c(0,0,0,0.5),"in")) 
  
  industry_emp_graph <- ggplot(
    data = df2,
    aes(
      industry_title, 
      estimated_employment)) + 
    geom_col(fill="#b9b8b8", position = "dodge") +
    geom_text(
      color = "#383E40",
      aes(
        label = comma(estimated_employment)), 
      vjust = 0.5, 
      hjust = ifelse(
        df2$estimated_employment > quantile(df2$estimated_employment, probs = 0.1),1.5,0.75))+
    coord_flip() +
    scale_y_continuous(labels = comma) +
    scale_x_discrete(limits=rev)+
    labs(y="Estimated Employment") +
    theme(text = element_text(colour = "#000000", size=14),
          title = element_text(color = "#00587C"),
          axis.title.y = element_blank(),
          axis.text.y=element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.background = element_rect(fill = NA),
          plot.background = element_rect(fill = "#FFFFFF"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          legend.background = element_blank(),
          legend.key = element_blank(),
          legend.position = "none",
          plot.title = element_blank(),
          plot.subtitle = element_blank(),
          axis.title=element_text(size=12,face="bold"),
          plot.margin=unit(c(0.55,0.5,0,0),"in"))
  
  g <- ggarrange(pct_change_graph, industry_emp_graph,
                 ncol = 2, nrow = 1, widths = c(2, 2))
  g_annotate <- 
    annotate_figure(g,
                    top = text_grob(
                      paste(
                        selected_msa, "\n",current_month, " - Industry Employment",sep=""),
                      color = "#00587C", face = "bold", size = 18),
                    bottom = text_grob("Data source: Current Employment Statistics (CES)", color = "#00587C",
                                       hjust = 1, x = 1, face = "italic", size = 10))
  
  file_name <- paste(d,"/",current_month," ",selected_msa,".png",sep="")
  
  ggsave(g_annotate, filename = file_name, dpi = 300, type = 'cairo',
         width = 14, height = 8.5, units = 'in', bg="#ffffff")
  
  print(file_name)
  
}




