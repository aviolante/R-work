# Fields in the cereal dataset:

# Name: Name of cereal
# mfr: Manufacturer of cereal
#  A = American Home Food Products;
#  G = General Mills
#  K = Kelloggs
#  N = Nabisco
#  P = Post
#  Q = Quaker Oats
#  R = Ralston Purina
# type:
#  cold
#  hot
# calories: calories per serving
# protein: grams of protein
# fat: grams of fat
# sodium: milligrams of sodium
# fiber: grams of dietary fiber
# carbo: grams of complex carbohydrates
# sugars: grams of sugars
# potass: milligrams of potassium
# vitamins: vitamins and minerals - 0, 25, or 100, indicating the typical percentage of FDA recommended
# shelf: display shelf (1, 2, or 3, counting from the floor)
# weight: weight in ounces of one serving
# cups: number of cups in one serving
# rating: a rating of the cereals (Possibly from Consumer Reports?)

library(ggplot2)
library(corrplot)

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



ggplot(cereal, aes(x=rating, y=sugars)) +
  geom_point()
