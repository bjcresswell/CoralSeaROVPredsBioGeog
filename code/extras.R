# Aesthetics

# Suggested palette for more than 8 cats together:

#safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                 #           "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")
#scales::show_col(safe_colorblind_palette)


  

cb_palette <- c(#"#88CCEE",
                             #"#CC6677", 
                             #"#DDCC77",
                             "#117733", 
                             "#332288", 
                             "#AA4499", 
                             "#44AA99", 
                             "#999933")


slab_palette <- c("#1c134b", "#332288", "#4a31c5",
                  "#732e68", "#aa4499", "#c773b9",
                  "#2e7368", "#44aa99", "#73c7b9",
                  "#606020", "#999933", "#c6c653")


hab_pal <- c("#073416", "#117733", "#1bba50")
                   


# Pals for tones for 80%, 95%, 100 density plotting
Reef_100_pal <- c("#1c134b", "#332288", "#4a31c5")   # %
Reef_1_Area_pal <- c("#732e68", "#aa4499", "#c773b9")   #
dCT_pal <- c("#2e7368", "#44aa99", "#73c7b9")   # 
dGBR_pal <- c("#606020", "#999933", "#c6c653")    #



theme_bjc <- function (base_size = 11, base_family = "Arial") {
  theme_minimal() %+replace% 
    theme(
      plot.background = element_rect(fill = "white", colour = "transparent"),
      panel.background = element_rect(fill = "white", colour = "transparent"),
      legend.title = element_text(color = "black", size = 10),
      legend.text = element_text(color = "black", size = 9),
      axis.title = element_text(color = "black", size = 10),
      axis.text = element_text(color = "black", size = 9)
    )
}

