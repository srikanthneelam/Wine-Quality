library(readr)
wine_dataset <- read_csv("C:/Users/srikanth/Desktop/Analytics/Data Sets/wine_dataset.csv")
attach(wine_dataset)
#Load all the required libraries.
library("ggplot2")
library("dplyr")
library("gridExtra")
install.packages("Simpsons")
library(Simpsons)
library(GGally)
install.packages("memisc")
library(memisc)
install.packages("pander")
library(pander)
library(corrplot)
#Transforming Quality from an Integer to a Factor
wine_dataset$quality <- factor(wine_dataset$quality, ordered = T)

#Creating a new Factored Variable called 'Rating'

wine_dataset$rating <- ifelse(wine_dataset$quality < 5, 'bad', ifelse(
  wine_dataset$quality < 7, 'average', 'good'))

wine_dataset$rating <- ordered(wine_dataset$rating,
                       levels = c('bad', 'average', 'good'))

View(wine_dataset)

## Structure and summary of the Dataframe

str(wine_dataset)
summary

## Univariate Plots

#First, before doing any analysis between the variables, I am going to plot the distribution of each of the variable as I would like to get a feel of the variables first. Based on the distribution shape, i.e. Normal, Positive Skew or Negative Skew, this will also help me to get some sense what to expect when I plot different variables against each other. Also for many variables, there are extreme outliers present in this dataset. For those, I will remove the extreme outliers for a more robust analysis

ggplot(data = wine_dataset, aes(x = quality)) +
  geom_bar(width = 1, color = 'black',fill = I('orange'))


ggplot(data = wine_dataset, aes(x = rating)) +
  geom_bar(width = 1, color = 'black',fill = I('blue'))

#As per the above plots the good quality and the poor quality wines are almost like outliers here, it might be difficult to get an accurate model of the Wine Quality. Let's look at the other plots.


grid.arrange(ggplot(wine_dataset, aes( x = 1, y = fixed_acidity ) ) + 
               geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ) +
               scale_y_continuous(lim = c(4,14)),
             ggplot(data = wine_dataset, aes(x = fixed_acidity)) +
               geom_histogram(binwidth = 1, color = 'black',fill = I('orange')) + 
               scale_x_continuous(lim = c(4,14)),ncol = 2)
#The distribution of Fixed Acidity is positively skewed. The median is around 8 with high concentration of wines with Fixed Acidity but due to some outliers, the mean has been dragged to around 9.4. The image has been rescaled to get rid of the high outliers.


grid.arrange(ggplot(wine_dataset, aes( x = 1, y = volatile_acidity ) ) + 
               geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ) +
               scale_y_continuous(lim = c(0,1)),
             ggplot(data = wine_dataset, aes(x = volatile_acidity)) +
               geom_histogram(binwidth = 0.05, color = 'black',fill = I('orange')) +
               scale_x_continuous(lim = c(0,1)), ncol = 2)
#The distribution of Volatile acidity looks like Bimodal with two peaks around 0.4 and 0.6



grid.arrange(ggplot(wine_dataset, aes( x = 1, y = citric_acid )) + 
               geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ),
             ggplot(data = wine_dataset, aes(x = citric_acid)) +
               geom_histogram(binwidth = 0.08, color = 'black',fill = I('orange')) +
               scale_x_continuous(breaks = seq(0,1,0.1), lim = c(0,1)), ncol = 2)
#Apart from some outliers, the distribution of Citric acid looks strange. Some higher values have no data at all and apart from them, the distribution looks almost rectangular. Maybe there was some error in the data or maybe the data collected was incomplete?

grid.arrange(ggplot(wine_dataset, aes( x = 1, y = residual_sugar )) + 
               geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ) +
               scale_y_continuous(lim = c(1,8)),
             ggplot(data = wine_dataset, aes(x = residual_sugar)) +
               geom_histogram(binwidth = 0.1, color = 'black',fill = I('orange')) +
               scale_x_continuous(lim = c(1,8)), ncol = 2)
# The distribution of Residual Sugar is again positively skewed with high peaks at around 2.3 with many outliers present at the higher ranges.

grid.arrange(ggplot(wine_dataset, aes( x = 1, y = chlorides )) + 
               geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ) +
               scale_y_continuous(lim = c(0,0.25)),
             ggplot(data = wine_dataset, aes(x = chlorides)) +
               geom_histogram(binwidth = 0.01, color = 'black',fill = I('orange')) +
               scale_x_continuous(lim = c(0,0.25)), ncol = 2)
# For Chlorides also, we see a similar distribution like Residual Sugar. We have got rid of extreme outliers in this image.

grid.arrange(ggplot(wine_dataset, aes( x = 1, y = free_sulfur_dioxide )) + 
               geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ) +
               scale_y_continuous(lim = c(0,45)),
             ggplot(data = wine_dataset, aes(x = free_sulfur_dioxide)) +
               geom_histogram(binwidth = 1, color = 'black',fill = I('orange')) +
               scale_x_continuous(breaks = seq(0,80,5), lim = c(0,45)), ncol = 2)

# For Free Sulphur Dioxide, there is a high peak at 7 but then it again follows the same positively skewed long tailed patterns with some outliers in the high range.

grid.arrange(ggplot(wine_dataset, aes( x = 1, y = total_sulfur_dioxide )) + 
               geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ) +
               scale_y_continuous(lim = c(0,180)),
             ggplot(data = wine_dataset, aes(x = total_sulfur_dioxide)) +
               geom_histogram(binwidth = 5, color = 'black',fill = I('orange')) +
               scale_x_continuous(lim = c(0,180)), ncol = 2)
# Being a superset of the previous variable, Total Sulphur Dioxide also follows a similar pattern.


grid.arrange(ggplot(wine_dataset, aes( x = 1, y = density)) + 
               geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ),
             ggplot(data = wine_dataset, aes(x = density)) +
               geom_histogram(binwidth = 0.001, color = 'black',fill = I('orange')), ncol = 2)

#For the Density variable, we see something new for the first time. This Variable has almost a perfect Normal Distribution

grid.arrange(ggplot(wine_dataset, aes( x = 1, y = pH)) + 
               geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ),
             ggplot(data = wine_dataset, aes(x = pH)) +
               geom_histogram(binwidth = 0.1, color = 'black',fill = I('orange')), ncol = 2)

# pH also has a very Normally distributed shape

grid.arrange(ggplot(wine_dataset, aes( x = 1, y = sulphates)) + 
               geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ) +
               scale_y_continuous(lim = c(0.3,1.6)),
             ggplot(data = wine_dataset, aes(x = sulphates)) +
               geom_histogram(binwidth = 0.1, color = 'black',fill = I('orange')) +
               scale_x_continuous(lim = c(0.3,1.6)), ncol = 2)
# Sulphates also exhibit a similar long tailed distribution like Chlorides or Free/Total Sulphur Dioxide. It has relatively less outliers.

grid.arrange(ggplot(wine_dataset, aes( x = 1, y = alcohol)) + 
               geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ) +
               scale_y_continuous(lim = c(8,14)),
             ggplot(data = wine_dataset, aes(x = alcohol)) +
               geom_histogram(binwidth = 0.1, color = 'black',fill = I('orange')) +
               scale_x_continuous(lim = c(8,14)), ncol = 2)
# Alcohol also follows a skewed distribution but here the skewness is less than that of Chlorides or Residual Sugars

# I also see that in this dataset, most of the wines belong to the 'average' quality with very few 'bad' and 'good' ones. Now this again raises my doubt if this dataset is a complete one or not. For the lack of these data, it might be challenging to build a predictive model as I don't have enough data for the Good Quality and the Bad Quality wines.

# Without analyzing the data, I think maybe the acidity(fixed, volatile or citric) will change the quality of wine based on their values. Also pH as related to acidity may have some effect on the quality. Also this would be an interesting thing to see how the pH is affected by the different acids present in the wine and if the overall pH affects the quality of the wine. I also think the residual sugar will have an effect on the wine quality as sugar determines how sweet the wine will be and may adversely affect the taste of the wine.
# Citric acid has a unique distribution as compared to the other numeric variables. It almost has a rectangular shape apart from a few outliers. Now if we compare the wine quality distribution, this distribution of Citric Acid is very unexpected and maybe there is even a possibility of incomplete data collection.


## Bivariate Plots

#First I will create a correlation table between the variables present in this dataset so that I get some initial guidance about which ones may be correlated to each other.


wine_dataset$quality <- as.numeric(wine_dataset$quality)
library(ggplot2)
wine_dataset1 <- wine_dataset[-13]
wine_dataset2 <-wine_dataset1[-13]
View(wine_dataset2)

install.packages("corpcor")
library(corpcor)
panel.cor<-function(x,y,digits=2,prefix="",cex.cor)
{
  usr<-par("usr");on.exit(par(usr))
  Par(usr=c(0,1,0,1))
  r=(cor(x,y))
  txt<-format(c(r,0.123456789),digits = digits)[1]
  txt<-paste(prefix,txt,sep = "")
}
pairs(wine_dataset2,upper.panel = panel.cor,main="scatter Plot matrix with Correlation Coefficients")
pairs(wine_dataset2)
library(psych)
pairs.panels(wine_dataset2)

c <- cor(
  wine_dataset2 %>%
        mutate(
      # now we translate quality to a number
      quality = as.numeric(quality)
    )
)
emphasize.strong.cells(which(abs(c) > .3 & c != 1, arr.ind = TRUE))
pandoc.table(c)

# The very first thing that caught my eye in this table is that Volatile acidity has a positive correlation with pH. But how can that be possible! We know that with the decrease in pH, acidity increases. So is it possible that a Simpson's Paradox is at play here? I will further investigate this peculiar characteristic.
# Density has a very strong correlation with Fixed Acidity.
# The variables most strongly correlated to quality are Volatile Acidity and Alcohol
# Alcohol has negative correlation with density. This is evident from the fact that the density of water is greater than the density of alcohol.

# created some Box plots between these variables to see if I have missed anything from the correlation table.

ggplot(data = wine_dataset2, aes(x = quality, y = fixed_acidity)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue') +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4)
#As we can see, Fixed Acidity has almost no effect on the Quality. The mean and median values of fixed acidity remains almost unchanged with increase in quality

  ggplot(data=wine_dataset2, aes(x = quality, y = volatile_acidity)) +
    geom_jitter( alpha = .3) +
    geom_boxplot(alpha = .5,color = 'blue') +
    stat_summary(fun.y = "mean", 
                 geom = "point", 
                 color = "red", 
                 shape = 8, 
                 size = 4)

  # Volatile acid seems to have a negative impact on the quality of the wine. As volatile acid level goes up, the quality of the wine degrades
  
  ggplot(data=wine_dataset2, aes(x=quality, y=citric_acid)) +
    geom_jitter( alpha = .3) +
    geom_boxplot(alpha = .5,color = 'blue') +
    stat_summary(fun.y = "mean", 
                 geom = "point", 
                 color = "red", 
                 shape = 8, 
                 size = 4)
  
  # Citric acid seems to have a positive correlation with Wine Quality. Better wines have higher Citric Acid.
  
  ggplot(data=wine_dataset2, aes(x=quality, y=residual_sugar)) +
    geom_jitter( alpha = .3) +
    geom_boxplot(alpha = .5,color = 'blue') +
    scale_y_continuous(lim = c(0,5)) +
    stat_summary(fun.y = "mean", 
                 geom = "point", 
                 color = "red", 
                 shape = 8, 
                 size = 4)
  
  # Previously I thought that Residual Sugar may have an effect on the wine quality. But this plot contradicts that assumption and shows that Residual Sugar almost has no effect on the Quality of the Wine. The mean values for the residual sugar is almost the same for every quality of wine.
  
  ggplot(data=wine_dataset2, aes(x=quality, y=chlorides)) +
    geom_jitter( alpha = .3) +
    geom_boxplot(alpha = .5,color = 'blue') +
    scale_y_continuous(lim = c(0,0.2)) +
    stat_summary(fun.y = "mean", 
                 geom = "point", 
                 color = "red", 
                 shape = 8, 
                 size = 4)
  # Even though weakly correlated, from the decrease in median values of the Chlorides with increase in quality, it seems that lower percent of Chloride seems to produce better wine
  
  ggplot(data=wine_dataset2, aes(x=quality, y=free_sulfur_dioxide)) +
    geom_jitter( alpha = .3) +
    geom_boxplot(alpha = .5,color = 'blue') +
    scale_y_continuous(lim = c(0,40)) +
    stat_summary(fun.y = "mean", 
                 geom = "point", 
                 color = "red", 
                 shape = 8, 
                 size = 4)
  # Now this is an interesting observation. We see that too low concentration of Free Sulphur Dioxide produces poor wine and too high concentration results in average wine
  # As this is a Subset of Free Sulphur Dioxide, we see a similar pattern here
  
  ggplot(data=wine_dataset2, aes(x=quality, y=total_sulfur_dioxide)) +
    geom_jitter( alpha = .3) +
    geom_boxplot(alpha = .5,color = 'blue') +
    scale_y_continuous(lim = c(0,150)) +
    stat_summary(fun.y = "mean", 
                 geom = "point", 
                 color = "red", 
                 shape = 8, 
                 size = 4)
  
  # 
  
  ggplot(data=wine_dataset2, aes(x=quality, y=density)) +
    geom_jitter( alpha = .3) +
    geom_boxplot(alpha = .5,color = 'blue') +
    stat_summary(fun.y = "mean", 
                 geom = "point", 
                 color = "red", 
                 shape = 8, 
                 size = 4)
  # Better wines seems to have lower densities. But may be it will be wise not to draw any conclusions here. Because there might be a possibility that the low density is due to higher alcohol content which actually is the driving factor for better wines
  
  ggplot(data=wine_dataset2, aes(x=quality, y=pH)) +
    geom_jitter( alpha = .3) +
    geom_boxplot(alpha = .5,color = 'blue') +
    stat_summary(fun.y = "mean", 
                 geom = "point", 
                 color = "red", 
                 shape = 8, 
                 size = 4)
  # Better wines seems to have less pH, i.e they are more acidic. But there are a quite a few outliers here. So maybe the next logical thing would be to see how the individual acids affects the pH.
  
  ggplot(data = wine_dataset2, aes(x = fixed_acidity, y = pH)) +
    geom_point(alpha = 0.3) +
    scale_x_log10(breaks=seq(5,15,1)) +
    xlab("Fixed Acidity in Log Scale") +
    geom_smooth(method="lm")
  
  ggplot(data = wine_dataset2, aes(x = volatile_acidity, y = pH)) +
    geom_point(alpha = 0.3) +
    scale_x_log10(breaks=seq(.1,1,.1)) +
    xlab("Volatile Acidity in Log Scale") +
    geom_smooth(method="lm")
  
  ggplot(data = subset(wine_dataset2, citric_acid > 0), aes(x = citric_acid, y = pH)) +
    geom_point(alpha = 0.3) +
    scale_x_log10() +
    xlab("Citric Acid in Log Scale") +
    geom_smooth(method="lm")
  
  ggplot(data=wine_dataset2, aes(x=quality, y=sulphates)) +
    geom_jitter( alpha = .3) +
    geom_boxplot(alpha = .5,color = 'blue') +
    scale_y_continuous(lim = c(0.25,1)) +
    stat_summary(fun.y = "mean", 
                 geom = "point", 
                 color = "red", 
                 shape = 8, 
                 size = 4)
  
  # Even though we see many outliers in the 'Average' quality wine, it seems that better wines have a stronger concentration of Sulphates.
  
  ggplot(data=wine_dataset2, aes(x=quality, y=alcohol)) +
    geom_jitter( alpha = .3) +
    geom_boxplot(alpha = .5,color = 'blue') +
    stat_summary(fun.y = "mean", 
                 geom = "point", 
                 color = "red", 
                 shape = 8, 
                 size = 4)
  
  #The correlation is really distinct here. It is pretty evident that better wines have higher Alcohol content in it. But we see a great number of outliers here. So it might be possible that alcohol alone does not contribute to a wine being a good quality one. Let's make a simple linear model and try to get the statistics here
  
  alcoholQualityLinearModel <- lm(as.numeric(quality) ~ alcohol,
                                  data = wine_dataset2)
  
  
  summary(alcoholQualityLinearModel)
  
  
  simple_cor_test <- function(x, y) {
    return(cor.test(x, as.numeric(y))$estimate)
    
  }
  
  correlations <- c(
    simple_cor_test(wine_dataset2$fixed_acidity, wine_dataset2$quality),
    simple_cor_test(wine_dataset2$volatile_acidity, wine_dataset2$quality),
    simple_cor_test(wine_dataset2$citric_acid, wine_dataset2$quality),
    simple_cor_test(log10(wine_dataset2$residual_sugar), wine_dataset2$quality),
    simple_cor_test(log10(wine_dataset2$chlorides), wine_dataset2$quality),
    simple_cor_test(wine_dataset2$free_sulfur_dioxide, wine_dataset2$quality),
    simple_cor_test(wine_dataset2$total_sulfur_dioxide, wine_dataset2$quality),
    simple_cor_test(wine_dataset2$density, wine_dataset2$quality),
    simple_cor_test(wine_dataset2$pH, wine_dataset2$quality),
    simple_cor_test(log10(wine_dataset2$sulphates), wine_dataset2$quality),
    simple_cor_test(wine_dataset2$alcohol, wine_dataset2$quality))
  names(correlations) <- c('fixed_acidity', 'volatile_acidity', 'citric_acid',
                           'log10_residual_sugar',
                           'log10_chlordies', 'free_sulfur_dioxide',
                           'total_sulfur_dioxide', 'density', 'pH',
                           'log10_sulphates', 'alcohol')
  correlations

  # From the correlation test, it seems that the following variables have a higher correlation to Wine Quality.

  #1. Alcohol
  #2. Sulphates(log10)
  #3. Volatile Acidity
  #4. Citric Acid
  ## Analysis of Bivariate Plots
  
  ### Observations
  
  #1. Fixed Acidity seems to have almost no effect on quality.
  #2. Volatile Acidity seems to have a negative correlation with the quality.
  #3. Better wines seem to have higher concentration of Citric Acid.
  #4. Better wines seem to have higher alcohol percentages. But when I created a linear model around it, I saw from the R squared value that alcohol by itself only contributes like 20% on the variance of the quality. So there may be some other factors at play here.
  #5. Even though it's a weak correlation, but lower percent of Chloride seems to produce better quality wines.
  #6. Better wines seem to have lower densities. But then again, this may be due to the higher alcohol content in them.
  #7. Better wines seem to be more acidic.
  #8. Residual sugar almost has no effect on the wine quality.
  
  # Linear Modelling
  #Now after all these analysis, I am going to take the variables which are most strongly correlated with the quality of the wine and generate a linear model with them.
  install.packages("caTools")
  library(caTools)
  set.seed(123)
  split=sample.split(wine_dataset2$quality,SplitRatio = 0.8)
  training_set=subset(wine_dataset2,split==TRUE)
  test_set=subset(wine_dataset2,split==FALSE)
  
  
  m1 <- lm(as.numeric(quality) ~ alcohol, data = training_set)
  m2 <- update(m1, ~ . + sulphates)
  m3 <- update(m2, ~ . + volatile_acidity)
  m4 <- update(m3, ~ . + citric_acid)
  m5 <- update(m4, ~ . + fixed_acidity)
  m6 <- update(m2, ~ . + pH)
  mtable(m1,m2,m3,m4,m5,m6)
  
  wine_predict <- data.frame(
    test_set$quality,
    predict(m5, test_set) - as.numeric(test_set$quality)
  )
  
  names(wine_predict) <- c("quality", "error")
  ggplot(data=wine_predict, aes(x=quality,y=error)) +
    geom_jitter(alpha = 0.3)
  
  
  ggplot(data=wine_dataset2, aes(y=alcohol, x=quality)) + 
    geom_jitter(alpha = .3)  +
    geom_boxplot(alpha = .5,color = 'blue') +
    stat_summary(fun.y = "mean", 
                 geom = "point", 
                 color = "red", 
                 shape = 8, 
                 size = 4) +
    xlab("Quality") +
    ggtitle("Influence of alcohol on wine quality")
  
  df <- data.frame(
    test_set$quality,
    predict(m5, test_set) - as.numeric(test_set$quality)
  )
  names(df) <- c("quality", "error")
  ggplot(data=df, aes(x=quality,y=error)) +
    geom_jitter(alpha = 0.3) +
    ggtitle("Linear model errors vs expected quality")
  
  ## Reflections
  
  #In this data, my main struggle was to get a higher confidence level when predicting factors that are responsible for the production of different quality of wines especially the 'Good' and the 'Bad' ones. As the data was very centralized towards the 'Average' quality, my training set did not have enough data on the extreme edges to accurately build a model which can predict the quality of a wine given the other variables with lesser margin of error. So maybe in future, I can get a dataset about Red Wines with more complete information so that I can build my models more effectively.
  
  #Initially when I was writing and developing for this project, I saw that some wines didn't have citric acid at all. Also the others showed almost a rectangular distribution. My first thought was maybe this was bad data or incomplete data. But then I researched further about wines. I saw that citric acid actually is added to some wines to increase the acidity. So it's evident that some wines would not have Citric Acid at all. So actually this was in parallel to my experimental findings.
  
  #The other variables showed either a Positively skewed or a Normal Distribution.
  
  #First I plotted different variables against the quality to see Univariate relationships between them and then one by one I threw in one or more external factors to see if they together have any effect on the categorical variable. I saw that the factors which affected the quality of the wine the most were Alcohol percentage, Sulphate and Acid concentrations.
  
  #I tried to figure out the effect of each individual acid on the overall pH of the wine. Here I found out a very peculiar phenomenon where I saw that for volatile acids, the pH was increasing with acidity which was against everything I learned in my Science classes.
  
  #But then to my utter surprise, for the first time in my life as a data analyst, I saw the legendary Simpson's Paradox at play where one lurking variable was reversing the sign of the correlation and in turn totally changing the trend in the opposite direction.
  
  #In the final part of my analysis, I plotted multivariate plots to see if there were some interesting combinations of variables which together affected the overall quality of the wine. It was in this section I found out that density did not play a part in improving wine quality.
  
  #For future analysis, I would love to have a dataset, where apart from the wine quality, a rank is given for that particular wine by 5 different wine tasters as we know when we include the human element, our opinion changes on so many different factors. So by including the human element in my analysis, I would be able to put in that perspective and see a lot of unseen factors which might result in a better or worse wine quality. Having these factors included inside the dataset would result in a different insight altogether in my analysis.
  pdf("wineQuality.pdf")
  ggplot(data = wine_dataset, aes(x = quality)) +
    geom_bar(width = 1, color = 'black',fill = I('orange'))
  
  
  ggplot(data = wine_dataset, aes(x = rating)) +
    geom_bar(width = 1, color = 'black',fill = I('blue'))
  
  grid.arrange(ggplot(wine_dataset, aes( x = 1, y = fixed_acidity ) ) + 
                 geom_jitter(alpha = 0.1 ) +
                 geom_boxplot(alpha = 0.2, color = 'red' ) +
                 scale_y_continuous(lim = c(4,14)),
               ggplot(data = wine_dataset, aes(x = fixed_acidity)) +
                 geom_histogram(binwidth = 1, color = 'black',fill = I('orange')) + 
                 scale_x_continuous(lim = c(4,14)),ncol = 2)
  grid.arrange(ggplot(wine_dataset, aes( x = 1, y = volatile_acidity ) ) + 
                 geom_jitter(alpha = 0.1 ) +
                 geom_boxplot(alpha = 0.2, color = 'red' ) +
                 scale_y_continuous(lim = c(0,1)),
               ggplot(data = wine_dataset, aes(x = volatile_acidity)) +
                 geom_histogram(binwidth = 0.05, color = 'black',fill = I('orange')) +
                 scale_x_continuous(lim = c(0,1)), ncol = 2)
  grid.arrange(ggplot(wine_dataset, aes( x = 1, y = citric_acid )) + 
                 geom_jitter(alpha = 0.1 ) +
                 geom_boxplot(alpha = 0.2, color = 'red' ),
               ggplot(data = wine_dataset, aes(x = citric_acid)) +
                 geom_histogram(binwidth = 0.08, color = 'black',fill = I('orange')) +
                 scale_x_continuous(breaks = seq(0,1,0.1), lim = c(0,1)), ncol = 2)
  grid.arrange(ggplot(wine_dataset, aes( x = 1, y = residual_sugar )) + 
                 geom_jitter(alpha = 0.1 ) +
                 geom_boxplot(alpha = 0.2, color = 'red' ) +
                 scale_y_continuous(lim = c(1,8)),
               ggplot(data = wine_dataset, aes(x = residual_sugar)) +
                 geom_histogram(binwidth = 0.1, color = 'black',fill = I('orange')) +
                 scale_x_continuous(lim = c(1,8)), ncol = 2)
  
  grid.arrange(ggplot(wine_dataset, aes( x = 1, y = chlorides )) + 
                 geom_jitter(alpha = 0.1 ) +
                 geom_boxplot(alpha = 0.2, color = 'red' ) +
                 scale_y_continuous(lim = c(0,0.25)),
               ggplot(data = wine_dataset, aes(x = chlorides)) +
                 geom_histogram(binwidth = 0.01, color = 'black',fill = I('orange')) +
                 scale_x_continuous(lim = c(0,0.25)), ncol = 2)
  grid.arrange(ggplot(wine_dataset, aes( x = 1, y = free_sulfur_dioxide )) + 
                 geom_jitter(alpha = 0.1 ) +
                 geom_boxplot(alpha = 0.2, color = 'red' ) +
                 scale_y_continuous(lim = c(0,45)),
               ggplot(data = wine_dataset, aes(x = free_sulfur_dioxide)) +
                 geom_histogram(binwidth = 1, color = 'black',fill = I('orange')) +
                 scale_x_continuous(breaks = seq(0,80,5), lim = c(0,45)), ncol = 2)
  
  grid.arrange(ggplot(wine_dataset, aes( x = 1, y = total_sulfur_dioxide )) + 
                 geom_jitter(alpha = 0.1 ) +
                 geom_boxplot(alpha = 0.2, color = 'red' ) +
                 scale_y_continuous(lim = c(0,180)),
               ggplot(data = wine_dataset, aes(x = total_sulfur_dioxide)) +
                 geom_histogram(binwidth = 5, color = 'black',fill = I('orange')) +
                 scale_x_continuous(lim = c(0,180)), ncol = 2)
  
  grid.arrange(ggplot(wine_dataset, aes( x = 1, y = density)) + 
                 geom_jitter(alpha = 0.1 ) +
                 geom_boxplot(alpha = 0.2, color = 'red' ),
               ggplot(data = wine_dataset, aes(x = density)) +
                 geom_histogram(binwidth = 0.001, color = 'black',fill = I('orange')), ncol = 2)
  
  grid.arrange(ggplot(wine_dataset, aes( x = 1, y = pH)) + 
                 geom_jitter(alpha = 0.1 ) +
                 geom_boxplot(alpha = 0.2, color = 'red' ),
               ggplot(data = wine_dataset, aes(x = pH)) +
                 geom_histogram(binwidth = 0.1, color = 'black',fill = I('orange')), ncol = 2)
  
  
  grid.arrange(ggplot(wine_dataset, aes( x = 1, y = sulphates)) + 
                 geom_jitter(alpha = 0.1 ) +
                 geom_boxplot(alpha = 0.2, color = 'red' ) +
                 scale_y_continuous(lim = c(0.3,1.6)),
               ggplot(data = wine_dataset, aes(x = sulphates)) +
                 geom_histogram(binwidth = 0.1, color = 'black',fill = I('orange')) +
                 scale_x_continuous(lim = c(0.3,1.6)), ncol = 2)
  
  grid.arrange(ggplot(wine_dataset, aes( x = 1, y = alcohol)) + 
                 geom_jitter(alpha = 0.1 ) +
                 geom_boxplot(alpha = 0.2, color = 'red' ) +
                 scale_y_continuous(lim = c(8,14)),
               ggplot(data = wine_dataset, aes(x = alcohol)) +
                 geom_histogram(binwidth = 0.1, color = 'black',fill = I('orange')) +
                 scale_x_continuous(lim = c(8,14)), ncol = 2)
  
  ggplot(data = wine_dataset2, aes(x = quality, y = fixed_acidity)) +
    geom_jitter( alpha = .3) +
    geom_boxplot(alpha = .5,color = 'blue') +
    stat_summary(fun.y = "mean", 
                 geom = "point", 
                 color = "red", 
                 shape = 8, 
                 size = 4)
  
  ggplot(data=wine_dataset2, aes(x = quality, y = volatile_acidity)) +
    geom_jitter( alpha = .3) +
    geom_boxplot(alpha = .5,color = 'blue') +
    stat_summary(fun.y = "mean", 
                 geom = "point", 
                 color = "red", 
                 shape = 8, 
                 size = 4)
  ggplot(data=wine_dataset2, aes(x=quality, y=citric_acid)) +
    geom_jitter( alpha = .3) +
    geom_boxplot(alpha = .5,color = 'blue') +
    stat_summary(fun.y = "mean", 
                 geom = "point", 
                 color = "red", 
                 shape = 8, 
                 size = 4)
  
  ggplot(data=wine_dataset2, aes(x=quality, y=residual_sugar)) +
    geom_jitter( alpha = .3) +
    geom_boxplot(alpha = .5,color = 'blue') +
    scale_y_continuous(lim = c(0,5)) +
    stat_summary(fun.y = "mean", 
                 geom = "point", 
                 color = "red", 
                 shape = 8, 
                 size = 4)
  
  ggplot(data=wine_dataset2, aes(x=quality, y=chlorides)) +
    geom_jitter( alpha = .3) +
    geom_boxplot(alpha = .5,color = 'blue') +
    scale_y_continuous(lim = c(0,0.2)) +
    stat_summary(fun.y = "mean", 
                 geom = "point", 
                 color = "red", 
                 shape = 8, 
                 size = 4)
  
  ggplot(data=wine_dataset2, aes(x=quality, y=free_sulfur_dioxide)) +
    geom_jitter( alpha = .3) +
    geom_boxplot(alpha = .5,color = 'blue') +
    scale_y_continuous(lim = c(0,40)) +
    stat_summary(fun.y = "mean", 
                 geom = "point", 
                 color = "red", 
                 shape = 8, 
                 size = 4)
  
  ggplot(data=wine_dataset2, aes(x=quality, y=total_sulfur_dioxide)) +
    geom_jitter( alpha = .3) +
    geom_boxplot(alpha = .5,color = 'blue') +
    scale_y_continuous(lim = c(0,150)) +
    stat_summary(fun.y = "mean", 
                 geom = "point", 
                 color = "red", 
                 shape = 8, 
                 size = 4)
  
  ggplot(data=wine_dataset2, aes(x=quality, y=density)) +
    geom_jitter( alpha = .3) +
    geom_boxplot(alpha = .5,color = 'blue') +
    stat_summary(fun.y = "mean", 
                 geom = "point", 
                 color = "red", 
                 shape = 8, 
                 size = 4)
  
  ggplot(data=wine_dataset2, aes(x=quality, y=pH)) +
    geom_jitter( alpha = .3) +
    geom_boxplot(alpha = .5,color = 'blue') +
    stat_summary(fun.y = "mean", 
                 geom = "point", 
                 color = "red", 
                 shape = 8, 
                 size = 4)
  
  ggplot(data = wine_dataset2, aes(x = fixed_acidity, y = pH)) +
    geom_point(alpha = 0.3) +
    scale_x_log10(breaks=seq(5,15,1)) +
    xlab("Fixed Acidity in Log Scale") +
    geom_smooth(method="lm")
  
  ggplot(data = wine_dataset2, aes(x = volatile_acidity, y = pH)) +
    geom_point(alpha = 0.3) +
    scale_x_log10(breaks=seq(.1,1,.1)) +
    xlab("Volatile Acidity in Log Scale") +
    geom_smooth(method="lm")
  
  ggplot(data = subset(wine_dataset2, citric_acid > 0), aes(x = citric_acid, y = pH)) +
    geom_point(alpha = 0.3) +
    scale_x_log10() +
    xlab("Citric Acid in Log Scale") +
    geom_smooth(method="lm")
  
  ggplot(data=wine_dataset2, aes(x=quality, y=sulphates)) +
    geom_jitter( alpha = .3) +
    geom_boxplot(alpha = .5,color = 'blue') +
    scale_y_continuous(lim = c(0.25,1)) +
    stat_summary(fun.y = "mean", 
                 geom = "point", 
                 color = "red", 
                 shape = 8, 
                 size = 4)
  ggplot(data=wine_dataset2, aes(x=quality, y=alcohol)) +
    geom_jitter( alpha = .3) +
    geom_boxplot(alpha = .5,color = 'blue') +
    stat_summary(fun.y = "mean", 
                 geom = "point", 
                 color = "red", 
                 shape = 8, 
                 size = 4)
  
  names(wine_predict) <- c("quality", "error")
  ggplot(data=wine_predict, aes(x=quality,y=error)) +
    geom_jitter(alpha = 0.3)
  
  
  ggplot(data=wine_dataset2, aes(y=alcohol, x=quality)) + 
    geom_jitter(alpha = .3)  +
    geom_boxplot(alpha = .5,color = 'blue') +
    stat_summary(fun.y = "mean", 
                 geom = "point", 
                 color = "red", 
                 shape = 8, 
                 size = 4) +
    xlab("Quality") +
    ggtitle("Influence of alcohol on wine quality")
  
  names(df) <- c("quality", "error")
  ggplot(data=df, aes(x=quality,y=error)) +
    geom_jitter(alpha = 0.3) +
    ggtitle("Linear model errors vs expected quality")
  dev.off()