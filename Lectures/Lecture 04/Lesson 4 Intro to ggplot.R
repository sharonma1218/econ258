#------------------------------------------------------------- #
# Lesson 4: Intro to ggplot                                   #
#                                                              #      
# Ardina Hasanbasri                                            #
# Econ 258                                                     #
#                                                              #
#--------------------------------------------------------------# 

# Best visualization reference! https://rkabacoff.github.io/datavis/

setwd("C:/Users/ardin/OneDrive/Desktop/Econ 258 Winter 2021/Lesson R Codes/Lesson 3 Code")

install.packages("ggplot2")

library(ggplot2)

data_clean <- read.csv("clean_internet_and_growth.csv") 

############### PART 1: GRAPHING ###############

#Recall from previous class, we drew a graph and saved it as png. W/ ggplot, we don't need to do this! 

data_clean <- filter(data_clean, adv_int>0 & adv_int<0.3 & wagegrowth<1)

adv_int_high <- data_clean$adv_int[data_clean$allhigh.x==1] 
adv_int_low  <- data_clean$adv_int[data_clean$allhigh.x==0]
wagegrowth_high <- data_clean$wagegrowth[data_clean$allhigh.x==1] 
wagegrowth_low  <- data_clean$wagegrowth[data_clean$allhigh.x==0]

# Compare plot versus qplot  
plot(adv_int_high, wagegrowth_high) 
qplot(adv_int_high, wagegrowth_high) # differences: aesthetics & can become an object 

qplot(adv_int_high, wagegrowth_high, 
      main = "Advance Internet Investment and Wage Growth by County Type",
      xlab = "Fraction Firms with Advance Internet", ylab = "Wage Growth 1995 to 2000", 
      xlim = range(c(0,0.3)), ylim = range(c(0,1)))

graph1 <-   plot(adv_int_high, wagegrowth_high) # outputs a NULL; i.e. plots cannot be saved (is not an object).
graph2 <-   qplot(adv_int_high, wagegrowth_high) # outputs the graph; i.e. qplots can be saved (is an object).

graph2_w_labels <- graph2 + geom_point(size=2) + 
  xlab("Fraction Firms with Advance Internet") +
  ylab("Wage Growth 1995 to 2000") # changes the size of the points & adds labels 

print(graph2_w_labels) # equivalent to "graph2_w_labels"

# For a more complicated graph use ggplot to specify in dataframe which one is x and y, arguments will be used later

data_high <- filter(data_clean, allhigh.x==1)
data_low <- filter(data_clean, allhigh.x==0)

ggplot(data_high) # will be empty 

ggplot(data_high, aes(adv_int, wagegrowth)) # the columns I care about are x and y 

ggplot(data_high, aes(adv_int, wagegrowth))+geom_point() # adds the points
ggplot(data_high, aes(adv_int, wagegrowth))+geom_point()+geom_smooth() # adds a smooth line that's like a linear fit 

ggplot(data_high, aes(adv_int, wagegrowth))+
  geom_point() + 
  geom_smooth()

high_graph <-  ggplot(data_high, aes(adv_int, wagegrowth)) +
  geom_point(color="darkblue") + 
  geom_smooth(method = "lm", se=FALSE) +
  xlab("Fraction Firms with Advance Internet") + 
  ylab("Wage Growth 1995 to 2000")

print(high_graph)

#-----------------------------------------------------------------
#
# *Creating a nicer graph from the paper!*
#
# To play around with the code, see if you can add each component 
# of the ggplot one by one.
# Change the components and see how your graph is changing. 
#
#-----------------------------------------------------------------

png("Graph_Output/wage_growth_and_internet_final.png", width = 600, height = 400)

myplot <- ggplot(data_clean, aes(adv_int, wagegrowth, group=factor(allhigh.x), color=factor(allhigh.x))) + # groups points; colors change based on group
  geom_point() + 
  geom_smooth(method = "lm", se=FALSE) +
  labs(title="Investment in Internet and Wage Growth", # title
       y="Wage Growth 1995 to 2000", x="Fraction Firms with Advance Internet", # y & x axes labels
       caption="Source: Forman, Goldfarb, and Greenstein (2012)") + # caption 
  xlim(range(c(0,0.3))) + ylim(range(c(0,0.6))) + # x and y limits 
  scale_colour_manual(values = c("#6EB5FF", "#FF6961"), 
                      name = "County Type",
                      labels =c("Low Characteristics", "High Characteristics")) +
  theme_classic() +  # specify a ready-made theme 
  theme(legend.title = element_text(size=12, face="bold"),
        legend.justification=c(0,1), 
        legend.position=c(0.05, 0.95),
        legend.background = element_blank(),
        plot.title = element_text(size=15, face="bold", hjust=0.5),
        axis.title.x = element_text(size=13, face="bold"), 
        axis.title.y = element_text(size=13, face="bold")) 

print(myplot)
dev.off()

# Find you png graph and look at it. It's not actually really nice. 
# Here is another way to save as a tiff file.
# Tiff files are much crispier and easier to save. 
# You just need to run the command below.

ggsave("Graph_Output/wage_growth_and_internet_final2.tiff" , myplot, height=4, width=6, units='in')

# Compare your tiff graphs and your png.

############### PART 2: Extra Code For Additional Graphs ###############

# Histogram of adv_int 
hist(data_clean$adv_int, col = "gray", breaks = 30, main = "Fraction of Firms with Advanced Internet", xlab = "Adv. Internet") 
abline(v=c(mean(data_clean$adv_int), median(data_clean$adv_int)), lty=c(2,3), lwd=2)
legend("topright", legend=c("mean Adv. Internet", "median Adv. Internet"), lty=c(2,3), lwd=2) 

qplot(data_clean$adv_int, main = "Fraction of Firms with Advanced Internet", xlab = "Adv. Internet", ylab="count") +
  geom_histogram(color = "black", fill = "white")  

# BarPlot of adv_int by county type
freq_table <- table(data_clean$allhigh.x) # bar plots are drawn from a table, thus we create a frequncy table first.
barplot(freq_table)
barplot(freq_table, horiz=TRUE, 
        main = "Frequency of High Category Counties", 
        xlim = range(0,1500),
        names.arg=c("Non-Top", "Top"), 
        arg.legends=list(x="topright"))

# BoxPlot of adv_int 
boxplot(data_clean$adv_int)

boxplot(data_clean$adv_int~data_clean$allhigh.x, 
        xlab="County Type", ylab="Advance Internet", col="gray")

#######################
#
# Practice by Yourself
# Two options
# 1) Make the internet wage graph much better (lines 75 to 101)
# 2) Using the code you have from the World Value Survey, 
# can you make a professional looking graph with ggplot?
# 
#######################

myplot <- ggplot(data_clean, aes(adv_int, wagegrowth, group=factor(allhigh.x), color=factor(allhigh.x))) + # groups points; colors change based on group
  geom_point() + 
  geom_smooth(method = "lm", se=FALSE) +
  labs(title="Investment in Internet and Wage Growth", # title
       y="Wage Growth 1995 to 2000", x="Fraction Firms with Advance Internet", # y & x axes labels
       caption="Source: Forman, Goldfarb, and Greenstein (2012)") + # caption 
  xlim(range(c(0,0.3))) + ylim(range(c(0,0.6))) + # x and y limits 
  scale_colour_manual(values = c("#6EB5FF", "#FF6961"), 
                      name = "County Type",
                      labels =c("Low Characteristics", "High Characteristics")) +
  theme_classic() +  # specify a ready-made theme 
  theme(legend.title = element_text(size=12, face="bold"),
        legend.justification=c(0,1), 
        legend.position=c(0.05, 0.95),
        legend.background = element_blank(),
        plot.title = element_text(size=15, face="bold", hjust=0.5),
        axis.title.x = element_text(size=13, face="bold"), 
        axis.title.y = element_text(size=13, face="bold")) 

ggsave("Graph_Output/wage_growth_and_internet_final2.tiff" , myplot, height=4, width=6, units='in')

myplot2 <- ggplot(data_clean, aes(adv_int, wagegrowth))+
           geom_bar(stat = "identity", fill = "Blue", color = "Blue") +
           scale_y_continuous(labels = percent) +
           scale_x_continuous(labels = percent) +
           theme_classic() +
           labs(x = "Fraction Firms with Advance Internet", 
                y = "Wage Growth 1995 to 2000", 
                title  = "Investment in Internet and Wage Growth")+
           theme(plot.title = element_text(size=15, face="bold", hjust=0.5),
                 axis.title.x = element_text(size=13, face="bold"), 
                 axis.title.y = element_text(size=13, face="bold")) 

ggsave("Internet_Wage_Chart.tiff" , myplot2, height=4, width=6, units='in')
