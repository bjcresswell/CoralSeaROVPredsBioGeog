# Aesthetics

# Suggested palette for more than 8 cats together:

#safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                 #           "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")
#scales::show_col(safe_colorblind_palette)


  

cb_palette <- c(#"#88CCEE",
                             #"#CC6677", 
                             #"#DDCC77",
                             #"#117733", 
                             "#332288", 
                             "#AA4499", 
                             "#44AA99", 
                             "#999933" 
                             #"#882255", 
                             #"#661100", 
                             #"#6699CC", 
                             #"#888888",
                             
)

theme_bjc <- function (base_size = 11, base_family = "Arial") {
  theme_minimal() %+replace% 
    theme(
      plot.background = element_rect(fill = "white", colour = "transparent"),
      legend.title = element_text(color = "black", size = 10),
      legend.text = element_text(color = "black", size = 9),
      axis.title = element_text(color = "black", size = 10),
      axis.text = element_text(color = "black", size = 9)
    )
}

