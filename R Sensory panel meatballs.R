#------------------------------------------------------------------------------------------------------------
#                        step 1) SESSION 3,4,5,6 
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
require(readxl)
require(tidyverse)


m1 <- read_excel("R/TRAINED PANEL/Panel_3steps.xlsx", 
                 sheet = "step 1")
m2<-m1 %>% pivot_longer(.,3:16, names_to="ATTRIBUTES", values_to="SCORES")
m2

# MEAN AND SD OF PANEL (STEP1)
mean(m1$ELASTICITY, na.rm = TRUE)
sd(m1$ELASTICITY)

# MEAN AND SD PER JUDGE (STEP1)
# ponerle un nombre a la tabla que vayas a crear. La nueva tabla irá por cada catador entonces, ahí le estoy diciendo que de la tabla "m1" (que es la que hemos creado arriba "VALIDACION", ) que me extraiga las filas del 1 al 4 ( si fuera por ejemplo para sacar la media de laura pues pondriamos m1[5:8,])
JUDGE1=m1[1:4,]
JUDGE1
mean(JUDGE1$BRIGHTNESS, na.rm = TRUE)
sd(JUDGE1$BRIGHTNESS)

#ANOVA attribute/session (P-VALUES)
summary(lm(AFTERTASTE~SESSION, data =m1))
anova(lm(AFTERTASTE~SESSION, data =m1))

#BOXPLOT STEP1

m2 %>%
  ggplot(aes(SESSION, SCORES))+
  geom_boxplot()+
  facet_wrap(~ATTRIBUTES, ncol= 5)

#---------------------------------------------------------------------------------------
#                    STEP 2) RE-TRAIN ATTRIBUTES: cohesiveness, juiciness, taste
#----------------------------------------------------------------------------------------

require(readxl)
require(tidyverse)

m3<-read_excel("R/TRAINED PANEL/Panel_3steps.xlsx", 
    sheet = "step 2")
m3

m4<-m3 %>% pivot_longer(.,3:16, names_to="ATTRIBUTES", values_to="SCORES")
m4

# MEAN AND SD OF PANEL (STEP2)
mean(m3$ELASTICITY, na.rm = TRUE)
sd(m3$ELASTICITY)

# MEAN AND SD PER JUDGE (STEP2)
# ponerle un nombre a la tabla que vayas a crear. La nueva tabla irá por cada catador entonces, ahí le estoy diciendo que de la tabla "m1" (que es la que hemos creado arriba "VALIDACION", ) que me extraiga las filas del 1 al 4 ( si fuera por ejemplo para sacar la media de laura pues pondriamos m1[5:8,])
JUDGE1=m3[1:4,]
JUDGE1
mean(JUDGE1$ELASTICITY, na.rm = TRUE)
sd(JUDGE1$ELASTICITY)

#ANOVA AMONG SESSIONS
summary(lm(TASTE~SESSION, data =m3))
anova(lm(TASTE~SESSION, data =m3))


#BOXPLOT STEP2

m5<-read_excel("R/TRAINED PANEL/Panel_3steps.xlsx", 
               sheet = "QDA")
m5

Step2<-left_join(m4,m5,by="ATTRIBUTES")
Step2

Step2 %>%
  ggplot(aes(SESSION, SCORES))+
  geom_boxplot()+
  facet_wrap(~ATTRIBUTES, ncol= 3)+
  stat_summary(fun=mean, geom="point", shape=18, size=4, color="red", fill="red")

#---------------------------------------------------------------------------------------

#3) AZTI meatballs 

require(readxl)
require(tidyverse)

# ANOVA JUDGE

STEP3 <-read_excel("R/TRAINED PANEL/Panel_3steps.xlsx", 
                   sheet = "step 3")
View(STEP3)


x1=STEP3$FIRMNESS
y1=STEP3$PRODUCT

tabla1=data.frame(y1,x1)
tabla1

ANOVA1=aov(y1 ~ x1, tabla1)
ANOVA1
summary(ANOVA1)

#PLOT STEP3

m6<-read_excel("R/TRAINED PANEL/Panel_3steps.xlsx", 
               sheet = "step 3", n_max = 24)
m6

m7<-m6 %>% pivot_longer(.,4:17, names_to="ATTRIBUTES", values_to="SCORES")
m7

#PARA PONER LA RAYITA ROJA DEL QDA
m8<-read_excel("R/TRAINED PANEL/Panel_3steps.xlsx", 
               sheet = "QDA")
m8
Step3<-left_join(m7,m8,by="ATTRIBUTES")
Step3



#plot general CON QDA
Step3 %>%
  ggplot(aes(SESSION, SCORES))+
  geom_boxplot()+
  geom_hline(aes(yintercept=QDA), size=1, color="red")+
  geom_rect(aes(xmin=0, xmax=4,ymin=QDA-1, ymax=QDA+1), alpha=0.01)+
  facet_wrap(~ATTRIBUTES, ncol= 5)+
  stat_summary(fun=mean, geom="point", shape=18, size=4, color="red", fill="red")

#---------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------
#spider chart
average_scores <- data.frame(
  row.names = c("REFERENCE", "PROTOTYPE"),
  OUTER_COLOUR = c(7.5, 7.7),
  INNER_COLOUR = c(8.5, 7.6),
  ELASTICITY = c(2,0.6),
  CHARACTERISTIC_ODOUR = c(0.9, 6.9),
  FIRMNESS = c(1.8,2.4),
  FRAGILITY = c(7.6, 6.8),
  JUICINESS = c(1.7, 5.7),
  CHEWINESS = c(3.1,4.5),
  TASTE = c(2, 6.8),
  AFTERTASTE = c(1.3, 6.5),
  ADHERENCE = c (2.5, 1.5),
  GRAININESS = c(1.4, 5.5),
  PASTINESS = c(6.5, 1.4),
  FAT_PERCEPTION = c(2.5, 1.5))
average_scores 
  
install.packages("fmsb")
library(fmsb)
# Define the variable ranges: maximum and minimum
max_min <- data.frame(
  OUTER_COLOUR = c(0, 9),
  INNER_COLOUR = c(0, 9),
  ELASTICITY = c(0, 9),
  CHARACTERISTIC_ODOUR = c(0, 9),
  FIRMNESS = c(0, 9),
  FRAGILITY = c(0, 9),
  JUICINESS = c(0, 9),
  CHEWINESS = c(0, 9),
  TASTE = c(0, 9),
  AFTERTASTE = c(0, 9),
  ADHERENCE = c (0, 9),
  GRAININESS = c(0, 9),
  PASTINESS = c(0, 9),
  FAT_PERCEPTION = c(0, 9))
  
rownames(max_min) <- c("Max", "Min")

# Bind the variable ranges to the data
df <- rbind(max_min, average_scores)
df

# Plot the data for REFERENCE
library(fmsb)
REFERENCE_data <- df[c("Max", "Min", "REFERENCE"), ]
radarchart(REFERENCE_data)

ALL_data <- df[c("Max", "Min", "REFERENCE", "PROTOTYPE"), ]
radarchart(ALL_data)

op <- par(mar = c(1, 2, 2, 1))
radarchart(ALL_data, caxislabels = c(0, 3, 6, 9))
par(op)

create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}

create_beautiful_radarchart(
  data = df, caxislabels = c(0, 2, 4, 6, 9),
  color = c("#00AFBB", "#E7B800", "#FC4E07")
)

# Add an horizontal legend
legend(
  x = "topleft", legend = rownames(df[-c(1,2),]), horiz = FALSE,
  bty = "n", pch = 20 , col = c("#00AFBB", "#E7B800", "#FC4E07"),
  text.col = "black", cex = 0.75, pt.cex = 1.5
)

citation()


  
  

