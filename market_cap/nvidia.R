# Sample data
company_data <- data.frame(
  company = c("Volkswagen", "Hilton Worldwide", "Under Armour", "Quest Diagnostics", "Hershey", "Kroger",
              "News Corp.", "Foot Locker", "Xerox", "Western Union", "Ralph Lauren", "H&R Block",
              "Bank of America", "Boeing", "Ford", "E*TRADE", "Goodyear", "Whirlpool",
              "Walt Disney", "Gap", "Hasbro", "Dr Pepper Snapple", "21st Century Fox",
              "Tiffany & Co.", "Harley-Davidson", "eBay", "General Mills"),
  market_value = c(83, 23, NA, NA, 20, 24, NA, NA, NA, NA, NA, NA, 312, 201, 40, 16, NA, NA,
                   188, NA, NA, NA, 83, NA, NA, 33, 27) # Filled in known values, others are NA for simplicity
)

# You'd want to fill in all market values accurately from the image if recreating precisely.
# For the purpose of demonstration, we'll impute some values or use the ones given.
# Let's assume some values for the smaller ones for demonstration purposes if they are not explicitly labelled
company_data$market_value[is.na(company_data$market_value)] <- 5 # Example small value for unlabelled ones

#install.packages("treemap")
library(treemap)

treemap(company_data,
        index="company",
        vSize="market_value",
        type="index", # or "value", "comp", "dens", "depth", "linked", "index"
        title="Company Market Stock Value (August 2)",
        palette = "Set3", # A color palette
        fontsize.labels=c(12, 8), # Font size for main and sub-labels
        fontcolor.labels=c("black", "black"),
        border.col=c("white"), # Border color for the tiles
        border.lwds=1, # Line width of the borders
        vColor="market_value", # Color based on market value
        # Arguments to control text appearance
        overlap.labels=0.5, # Controls label overlap. 0=no overlap. 1=complete overlap.
        bg.labels=c("transparent"), # Background color for labels
        align.labels=list(c("left", "top"), c("right", "bottom")), # Label alignment
        # Some aesthetic improvements to match the image's "stepped" look would require more custom plotting,
        # perhaps by manually arranging rectangles or using a package designed for that specific visual style.
        # A standard treemap will typically fill a single rectangular area.
        # The image's layout suggests a custom arrangement rather than a pure algorithmic treemap fill.
        # However, the core concept of size mapping is the same.
)

# install.packages("highcharter", binary = TRUE)

library(highcharter)

hchart(company_data,
       type = "treemap",
       hcaes(x = company, value = market_value, color = market_value)) %>%
  hc_title(text = "Company Market Stock Value (August 2)") %>%
  hc_tooltip(pointFormat = "<b>{point.name}</b>: ${point.value} billion") %>%
  hc_colorAxis(minColor = "#FFFFFF", maxColor = "#0000FF") # Example color axis



# install.packages("treemapify", binary = TRUE)

library(ggplot2)
library(treemapify)

ggplot(company_data, aes(area = market_value, fill = market_value, label = company)) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "centre", grow = TRUE) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") + # Customize colors
  labs(title = "Company Market Stock Value (August 2)") +
  theme_void() + # Remove axis and background elements
  theme(legend.position = "none") # Remove legend if not needed

