library(ggplot2)  # All Plots
library(scales)
library(corrplot) # Plot 2 Correlations
library(dplyr)
library(maps)     # Plot 3
library(ggthemes) # Plot 3

#_____________________________________________________________________________#
#__________________________                         __________________________#
#__________________________   PLOT 1: Scatter Plot  __________________________#
#__________________________                         __________________________#
#_____________________________________________________________________________#

cereal = read.csv('/Users/anviol/Desktop/Content/Datasets/cereal.csv')

# Find some Correlated Variables for a Plot
cereal_corr = cor(cereal[,4:16])
corrplot(cereal_corr, method = "circle", addCoef.col = "gray")

# Higer Resolution Image
tiff('ggplotFormatMagic.tiff', units="in", width=7, height=4, res=300)

# Let the Formating Fun Begin
ggplot(cereal, aes(x=rating, y=sugars)) +                                        # Call Plot
  geom_point(colour="gray") +                                                    # Set as a point plot
  geom_point(data=cereal[1:5,], pch=16, colour="blueviolet") +                   # Color Specific Points
  geom_point(data=cereal[9:13,], pch=16, colour="darksalmon") +
  geom_smooth(method = loess,                                                    # Apply Smoothing Line
              color = "deepskyblue4",                                            #   Line Color
              fill = "lightblue1",                                               #   Standard Error Fill
              linetype = "dashed",                                               #   Line Type
              se = TRUE) +                                                       #   Standard Error Add
  ggtitle(expression(atop(bold("Scatter Plot: Cereal"),                          # Title w/ Sub-Title
                          atop(italic("Sugar & Rating Correlation"),"")))) +
  xlab("Rating") +                                                               # X Axis Label
  ylab("Sugars") +                                                               # Y Axis Label
  theme_bw() +                                                                   # Black and White Theme
  theme(plot.title = element_text(hjust=0.5, vjust=-0.4, size=10),               # Centers Plot Title
        panel.grid.major = element_blank(),                                      # Remove Grid Lines
        panel.grid.minor = element_blank(),                                      # Remove Grid Lines
        panel.border = element_rect(colour="lightgoldenrod"),                    # Change Panel Border
        axis.title.x = element_text(colour="firebrick", size=8, face="bold"),    # X Axis Title
        axis.title.y = element_text(colour="firebrick", size=8, face="bold"),    # Y Axis Title
        axis.ticks.x = element_line(colour="black", size=1),                     # X Axis Tick Marks
        axis.ticks.y = element_line(colour="black", size=1),                     # Y Axis Tick Marks
        axis.text.x = element_text(colour="darkorange", size=8, face="bold"),    # X Axis Labels
        axis.text.y = element_text(colour="darkorange", size=8, face="bold"))    # Y Axis Labels

dev.off()

#_____________________________________________________________________________#
#__________________________                         __________________________#
#__________________________ PLOT 2: Multi-Line Plot __________________________#
#__________________________                         __________________________#
#_____________________________________________________________________________#

# Load New Data (https://archive.ics.uci.edu/ml/datasets/Dow+Jones+Index)
dow_df = read.table('/Users/anviol/Desktop/Content/Datasets/dow_jones_index/dow_jones_index.data', sep = ",", header = TRUE)
dow_df_sub = filter(dow_df, stock == c('DIS','WMT'))

# Convert Character Date to Date and 'Better' Format
dow_df_sub$date = format(as.Date(dow_df_sub$date, format = "%m/%d/%Y"),"%m-%d-%Y")

# Remove $ Symbol and COnvert to Numeric
dow_df_sub$open = as.numeric(gsub("\\$", "", dow_df_sub$open))
dow_df_sub$high = as.numeric(gsub("\\$", "", dow_df_sub$high))
dow_df_sub$low = as.numeric(gsub("\\$", "", dow_df_sub$low))

# Higer Resolution Image
tiff('ggplotFormatMagic2.tiff', units="in", width=7, height=4, res=300)

ggplot(dow_df_sub, aes(x = date, y = open, group = stock, colour=stock)) +
  geom_line() +
  geom_line(mapping = aes(y = low), lty = "dashed", colour="gray") +                          # Adding Multiple Lines
  geom_line(mapping = aes(y = high), lty = "dashed", colour = "gray") +
  labs(color='', x="Date", y="Stock Price", title="Dow Jones: Disney and Walmart") +          # All-in-One Labels
  scale_color_manual(labels = c("Disney", "Walmart"),                                         # Manually Change Label Names and Colors
                     values = c("#520099", "#ffc220")) +
  scale_y_continuous(labels = dollar) +                                                       # Y Label $ Format
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5, vjust=-0.4, size=10, face="bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour="gray"),
        axis.title.x = element_text(colour="firebrick", size=8, face="bold"),
        axis.title.y = element_text(colour="firebrick", size=8, face="bold"),
        axis.ticks.x = element_line(colour="gray", size=.5),
        axis.ticks.y = element_line(colour="gray", size=.5),
        axis.text.x = element_text(colour="gray48", size=8, face="bold", angle = 45, hjust=1), # Add x-axis Angle and Horizontal Adjustment
        axis.text.y = element_text(colour="gray48", size=8, face="bold"),
        legend.text = element_text(colour="black", size=6, face ="bold"),                      # Legend Design Options
        #legend.title = element_text(colour=, size=, face = ),
        legend.position = "bottom")

dev.off()

#_____________________________________________________________________________#
#_____________________                                  ______________________#
#_____________________  PLOT 3: Simple Map w/ Lat Long  ______________________#
#_____________________                                  ______________________#
#_____________________________________________________________________________#

# Load Mobile Usage Data
mobile_df = read.csv('/Users/anviol/Desktop/Content/Datasets/mobile_act_3mon.csv')

# Set up Clean Map Background
usa_map = ggplot() +
  borders("state", colour = "gray85", fill = "gray80") +
  theme_map() 

# High Res Image
tiff('ggplotFormatMagic3.tiff', units="in", width=7, height=4, res=300)

# Plot
usa_map +
  geom_point(data=mobile_df,
             aes(x = lon, y = lat),
             colour = 'dodgerblue4',
             alpha = .3) +
  labs(title="Mobile Usage in United States: 3 Months") +
  theme(plot.title = element_text(hjust=0.5, vjust=-0.4, size=8, face="bold"))

dev.off()
