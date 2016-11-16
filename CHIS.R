library(ggplot2)
library(ggthemes)
library(haven)
library(dplyr)
library(reshape2)

# Import the CHIS Data Set in R

chis.all <- read_sas("data/adult.sas7bdat")
str(chis.all)
# Filter Data Set as per Instructions in Video.

# RBMI (BMI Category Description), BMI_P (BMI Value), RACEHPR2 (Race), SRSEX (Sex), SRAGE_P (Age)
# MARIT2 (Marital Status), AB1 (General Health Condition), ASTCUR (Current Asthma Status),
# AB51 (Type I or Type II Diabetes),  POVLL (Poverty Level)
Sel_Var <- c("RBMI", "BMI_P", "RACEHPR2",
             "SRSEX", "SRAGE_P", "MARIT2",
             "AB1", "ASTCUR", "AB51", "POVLL")
# Select Only Latino[1], Asian[4], African American[5] and White[6]

adult <- chis.all %>% select(one_of(Sel_Var)) %>% filter(RACEHPR2 %in% c(1,4,5,6))
rm(chis.all)

# 1: Exploring Data
# Explore the dataset with summary and str
summary(adult)
str(adult)

# Histogram: Age colored by BMI, default binwidth
ggplot(adult, aes(x = SRAGE_P, fill = factor(RBMI) ) ) +
  geom_histogram(binwidth = 1)

# 2: Data Cleaning

# You should have noticed in the age distribution that there is an unusual spike of individuals at 85, 
# which seems like an artifact of data collection and storage. Solve this by only keeping observations 
# for which adult$SRAGE_P is smaller than or equal to 84.
# Remove individual aboves 84

adult <- adult[adult$SRAGE_P <= 84, ] 

# There is a long positive tail on the BMIs that we'd like to remove. Only keep observations for which 
# adult$BMI_P is bigger than or equal to 16 and adult$BMI_P is strictly smaller than 52.
# Remove individuals with a BMI below 16 and above or equal to 52

adult <- adult[adult$BMI_P >= 16 & adult$BMI_P < 52, ]

# Relabel the race variable:
adult$RACEHPR2 <- factor(adult$RACEHPR2, labels = c("Latino", "Asian", "African American", "White"))

# Relabel the BMI categories variable:
adult$RBMI <- factor(adult$RBMI, labels = c("Under-weight", "Normal-weight", "Over-weight", "Obese"))

#3: Multiple Histograms

# The color scale used in the plot
BMI_fill <- scale_fill_brewer("BMI Category", palette = "Reds")

# Theme to fix category display in faceted plot
fix_strips <- theme(strip.text.y = element_text(angle = 0, hjust = 0, vjust = 0.1, size = 14),
                    strip.background = element_blank(), 
                    legend.position = "none")

# Histogram with BMI_fill and customizations
ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) + 
  geom_histogram(binwidth = 1) +
  fix_strips +
  BMI_fill +
  facet_grid(RBMI~.) +
  theme_classic()

# 4: Alternatives to Above

# Plot 1 - Count histogram
ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) + 
  geom_histogram(binwidth = 1) +
  BMI_fill

# Plot 2 - Density histogram
ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) + 
  geom_histogram(aes(y = ..density..), binwidth = 1) +
  BMI_fill

# Plot 3 - Faceted count histogram
ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) + 
  geom_histogram(binwidth = 1) +
  BMI_fill +
  facet_grid(RBMI ~ .)

# Plot 4 - Faceted density histogram
ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) + 
  geom_histogram(aes(y = ..density..), binwidth = 1) +
  BMI_fill +
  facet_grid(RBMI ~ .)

# Plot 5 - Density histogram with position = "fill"
ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) + 
  geom_histogram(aes(y = ..density..), binwidth = 1, position = "fill") +
  BMI_fill

# Plot 6 - The accurate histogram
ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) + 
  geom_histogram(aes(y = ..count../sum(..count..) ), binwidth = 1, position = "fill") +
  BMI_fill

# 5: Do Things Manually

# In the previous exercise we looked at how to produce a frequency histogram when we have many 
# sub-categories. The problem here is that this can't be facetted because the calculations occur on 
# the fly inside ggplot2. To overcome this we're going to calculate the proportions outside ggplot2. 
# This is the beginning of our flexible script for a mosaic plot.

# Create DF with table()
DF <- table(adult$RBMI, adult$SRAGE_P)

# Use apply on DF to get frequency of each group
DF_freq <- apply(DF, 2, function(x) x/sum(x) )

# Use melt on DF to create DF_melted
DF_melted <- melt(DF_freq)

# Change names of DF_melted
names(DF_melted) <- c("FILL", "X", "value")

# Make this a faceted plot
ggplot(DF_melted, aes(x = X, y = value, fill = FILL)) +
  geom_bar(stat = "identity", position = "stack") +
  BMI_fill + 
  facet_grid(FILL ~ .)

# 6: Merimeko/Mosaic Plot

# In the previous exercise we looked at different ways of showing the frequency distribution within each
# BMI category. This is all well and good, but the absolute number of each age group also has an 
# influence on if we will consider something as over-represented or not. Here, we will proceed to change
# the widths of the bars to show us something about the n in each group.
# This will get a bit more involved, because the aim is not to draw bars, but rather rectangles, 
# for which we can control the widths. You may have already realized that bars are simply rectangles, 
# but we don't have easy access to the xmin and xmax aesthetics, but in geom_rect() we do! Likewise, 
# we also have access to ymin and ymax. So we're going to draw a box for every one of our 268 distinct 
# groups of BMI category and age.

# The initial contingency table
DF <- as.data.frame.matrix(table(adult$SRAGE_P, adult$RBMI))

# Add the columns groupsSum, xmax and xmin. Remove groupSum again.
DF$groupSum <- rowSums(DF)
DF$xmax <- cumsum(DF$groupSum)
DF$xmin <- DF$xmax - DF$groupSum

# The groupSum column needs to be removed.
DF$groupSum <- NULL

# Copy row names to variable X

DF$X <- row.names(DF)

# Melt the dataset

DF_melted <- melt(DF, id.vars = c("X", "xmin", "xmax"), 
                  variable.name = "FILL")

# dplyr call to calculate ymin and ymax - don't change

DF_melted <- DF_melted %>% 
  group_by(X) %>% 
  mutate(ymax = cumsum(value/sum(value)),
         ymin = ymax - value/sum(value))

# Plot rectangles - don't change.

ggplot(DF_melted, aes(ymin = ymin, 
                      ymax = ymax,
                      xmin = xmin, 
                      xmax = xmax, 
                      fill = FILL)) + 
  geom_rect(colour = "white") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  BMI_fill +
  theme_tufte()

# 7: Adding statistics

# In the previous exercise we generated a plot where each individual bar was plotted separately using 
# rectangles (shown in the viewer). This means we have access to each piece and we can apply different 
# fill parameters. So let's make some new parameters. To get the Pearson residuals, we'll use the 
# chisq.test().

# Perform chi.sq test (RBMI and SRAGE_P)

results <- chisq.test(table(adult$RBMI, adult$SRAGE_P))

# Melt results$residuals and store as resid

resid <- melt(results$residuals)

# Change names of resid

names(resid) <- c("FILL", "X", "residual")

# merge the two datasets:

DF_all <- merge(DF_melted, resid)

# Update plot command

ggplot(DF_all, aes(ymin = ymin, 
                   ymax = ymax,
                   xmin = xmin, 
                   xmax = xmax, 
                   fill = residual)) + 
  geom_rect() +
  scale_fill_gradient2() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_tufte()

# 8: Adding text

# Now that we are not coloring according to BMI category, we have to add the group labels manually. 
# Also, we neglected to label the x-axis properly!

# Position for labels on x axis
DF_all$xtext <- DF_all$xmin + (DF_all$xmax - DF_all$xmin)/2

# Position for labels on y axis (don't change)
index <- DF_all$xmax == max(DF_all$xmax)
DF_all$ytext <- DF_all$ymin[index] + (DF_all$ymax[index] - DF_all$ymin[index])/2

# Plot

ggplot(DF_all, aes(ymin = ymin, ymax = ymax, xmin = xmin, 
                   xmax = xmax, fill = residual)) + 
  geom_rect(col = "white") +
  # geom_text for ages (i.e. the x axis)
  geom_text(aes(x = xtext, 
                label = X),
            y = 1,
            size = 3,
            angle = 90,
            hjust = 1,
            show.legend = FALSE) +
  # geom_text for BMI (i.e. the fill axis)
  geom_text(aes(x = max(xmax), 
                y = ytext,
                label = FILL),
            size = 3,
            hjust = 1,
            show.legend  = FALSE) +
  scale_fill_gradient2() +
  theme_tufte() +
  theme(legend.position = "bottom")

# 9: Generalizations - Creating a Function for Mosaic Plots

# Now that you've done all the steps necessary to make our mosaic plot, you can wrap all the steps into 
# a single function that we can use to examine any two variables of interest in our data frame 
# (or in any other data frame for that matter).

# Script generalized into a function
mosaicGG <- function(data, X, FILL) {
  
  # Proportions in raw data
  DF <- as.data.frame.matrix(table(data[[X]], data[[FILL]]))
  DF$groupSum <- rowSums(DF)
  DF$xmax <- cumsum(DF$groupSum)
  DF$xmin <- DF$xmax - DF$groupSum
  DF$X <- row.names(DF)
  DF$groupSum <- NULL
  DF_melted <- melt(DF, id = c("X", "xmin", "xmax"), variable.name = "FILL")
  library(dplyr)
  DF_melted <- DF_melted %>% 
    group_by(X) %>% 
    mutate(ymax = cumsum(value/sum(value)),
           ymin = ymax - value/sum(value))
  
  # Chi-sq test
  results <- chisq.test(table(data[[FILL]], data[[X]])) # fill and then x
  resid <- melt(results$residuals)
  names(resid) <- c("FILL", "X", "residual")
  
  # Merge data
  DF_all <- merge(DF_melted, resid)
  
  # Positions for labels
  DF_all$xtext <- DF_all$xmin + (DF_all$xmax - DF_all$xmin)/2
  index <- DF_all$xmax == max(DF_all$xmax)
  DF_all$ytext <- DF_all$ymin[index] + (DF_all$ymax[index] - DF_all$ymin[index])/2
  
  # plot:
  g <- ggplot(DF_all, aes(ymin = ymin,  ymax = ymax, xmin = xmin, 
                          xmax = xmax, fill = residual)) + 
    geom_rect(col = "white") +
    geom_text(aes(x = xtext, label = X),
              y = 1, size = 3, angle = 90, hjust = 1, show.legend = FALSE) +
    geom_text(aes(x = max(xmax),  y = ytext, label = FILL),
              size = 3, hjust = 1, show.legend = FALSE) +
    scale_fill_gradient2("Residuals") +
    scale_x_continuous("Individuals", expand = c(0,0)) +
    scale_y_continuous("Proportion", expand = c(0,0)) +
    theme_tufte() +
    theme(legend.position = "bottom")
  print(g)
}

# BMI described by age
mosaicGG(adult, "SRAGE_P","RBMI")

# Poverty described by age
mosaicGG(adult, "SRAGE_P","POVLL")

# mtcars: am described by cyl
library(mtcars)
mosaicGG(mtcars, "cyl","am")

# Vocab: vocabulary described by education
install.packages("car")
library(car)
mosaicGG(Vocab, "education","vocabulary")