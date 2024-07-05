#title: "Project 2" 
#Writer: Adedotun Teminiola Inaolaji


#importation of the data 
crimedata = read.csv("AssessmentCrimeData.csv") 
View(crimedata) 


#decriptive statistics  
#Compare mean and median  
summary(crimedata)  

install.packages("psych")  
library(psych) 
describe(crimedata) 

install.packages("rcompanion") 
library(rcompanion) 

attach(crimedata) 

install.packages("gridExtra") 
library(gridExtra) 

#Visualizing the 14 crimes indicator with Boxplot- Anti.Social.Behaviour/Burglary/Robbery/Vehicle Crime/shoplifting/criminal.damage.arson/other.theft/drugs/other.crimes/Bike.Theft/Possession.of.weapons/Public.Order/Theft.from.the.person 
#Simple Plots to Visual the data  
par(mfrow = c (2,7)) 
boxplot(log10(Anti.Social.Behaviour/Land.Area.in.Hectares), col = "lightblue",  main ="Anti.Social.Behaviour") 
boxplot(log10(Burglary/Land.Area.in.Hectares), col = "lightblue", main ="Burglary") 
boxplot(log10(Robbery/Land.Area.in.Hectares), col = "lightblue", main ="Robbery") 
boxplot(log10(Vehicle.Crimes/Land.Area.in.Hectares), col = "lightblue", main ="Vehicle.Crimes") 
boxplot(log10(Violent.Crimes/Land.Area.in.Hectares), col = "lightblue", main ="Violent.Crimes") 
boxplot(log10(Shoplifting/Land.Area.in.Hectares), col = "lightblue", main ="Shoplifting") 
boxplot(log10(Criminal.Damage...Arson/Land.Area.in.Hectares),  col = "lightblue", main ="Criminal.Damage...Arsons") 
boxplot(log10(Other.Theft/Land.Area.in.Hectares), col = "lightblue", main ="Other.Theft") 
boxplot(log10(Drugs/Land.Area.in.Hectares),  col = "lightblue", main ="Drugs") 
boxplot(log10(Other.Crimes/Land.Area.in.Hectares),  col = "lightblue", main ="Other.Crimes") 
boxplot(log10(Bike.Theft/Land.Area.in.Hectares),  col = "lightblue", main ="Bike.Theft") 
boxplot(log10(Possession.of.Weapons/Land.Area.in.Hectares),  col = "lightblue", main ="Possession.of.Weapons") 
boxplot(log10(Public.Order/Land.Area.in.Hectares),  col = "lightblue", main ="Public.Order") 
boxplot(log10(Theft.From.the.Person/Land.Area.in.Hectares), col = "lightblue", main ="Theft.From.the.Person") 

#Visualizing the 14 crimes indicator with Histrogram - Anti.Social.Behaviour/Burglary/Robbery/Vehicle Crime/shoplifting/criminal.damage.arson/other.theft/drugs/other.crimes/Bike.Theft/Possession.of.weapons/Public.Order/Theft.from.the.person 
#Simple Plots to Visual the data  

#par(mfrow = c (2,7)) 
par(mar = c(2, 2, 1, 1)) 
hist(log10(Anti.Social.Behaviour/Land.Area.in.Hectares), main ="Anti.Social.Behaviour", col = "lightblue", xlab = "log10(ASB Density)", cex.lab =1.2) 
hist(log10(Burglary/Land.Area.in.Hectares), main ="Burglary", col = "lightblue", xlab = "log10(Burglary Density)", cex.lab =1.2) 
hist(log10(Robbery/Land.Area.in.Hectares), main ="Robbery", col = "lightblue", xlab = "log10(Robbery Density)", cex.lab =1.2) 
hist(log10(Vehicle.Crimes/Land.Area.in.Hectares), main ="Vehicle.Crimes", col = "lightblue", xlab = "log10(Vehicle.Crimes Density)", cex.lab =1.2) 
hist(log10(Violent.Crimes/Land.Area.in.Hectares),  main ="Violent.Crimes", col = "lightblue", xlab = "log10(Violent.CrimesDensity)", cex.lab =1.2) 
hist(log10(Shoplifting/Land.Area.in.Hectares), main ="Shoplifting", col = "lightblue", xlab = "log10(Shoplifting Density)", cex.lab =1.2) 
hist(log10(Criminal.Damage...Arson/Land.Area.in.Hectares), main ="Criminal.Damage...Arson", col = "lightblue", xlab = "log10(Criminal.Damage...Arson Density)", cex.lab =1.2) 
hist(log10(Other.Theft/Land.Area.in.Hectares), main ="Other.Theft", col = "lightblue", xlab = "log10(Other.Theft Density)", cex.lab =1.2) 
hist(log10(Drugs/Land.Area.in.Hectares), main ="Drugs", col = "lightblue", xlab = "log10(Drugs Density)", cex.lab =1.2) 
hist(log10(Other.Crimes/Land.Area.in.Hectares), main ="Other.Crimes", col = "lightblue", xlab = "log10(Other.Crimes Density)", cex.lab =1.2) 
hist(log10(Bike.Theft/Land.Area.in.Hectares), main ="Bike.Thef", col = "lightblue", xlab = "log10(Bike.Theft Density)", cex.lab =1.2) 
hist(log10(Possession.of.Weapons/Land.Area.in.Hectares), main ="Possession.of.Weapon", col = "lightblue", xlab = "log10(Possession.of.WeaponsDensity)", cex.lab =1.2) 
hist(log10(Public.Order/Land.Area.in.Hectares), main ="Public.Order",col = "lightblue", xlab = "log10(Public.Order Density)", cex.lab =1.2) 
hist(log10(Theft.From.the.Person/Land.Area.in.Hectares), main ="Theft from the person", col = "lightblue", xlab = "log10(Theft.From.the.Person Density)", cex.lab =1.2) 

#Assessment Part 2 
############# Create a linear regression model 
model1<- lm(log10(Anti.Social.Behaviour/Land.Area.in.Hectares) ~ log10(Population/Land.Area.in.Hectares)) 
summary(model1) 
model2<- lm(log10(Burglary/Land.Area.in.Hectares) ~ log10(Population/Land.Area.in.Hectares)) 
summary(model2) 
model3<- lm(log10(Robbery/Land.Area.in.Hectares) ~ log10(Population/Land.Area.in.Hectares)) 
summary(model3) 
model4<- lm(log10(Vehicle.Crimes/Land.Area.in.Hectares) ~ log10(Population/Land.Area.in.Hectares))  
summary(model4) 
model5<- lm(log10(Violent.Crimes/Land.Area.in.Hectares) ~ log10(Population/Land.Area.in.Hectares)) 
summary(model5) 
model6<- lm(log10(Shoplifting/Land.Area.in.Hectares) ~ log10(Population/Land.Area.in.Hectares)) 
summary(model6) 
model7<- lm(log10(Criminal.Damage...Arson/Land.Area.in.Hectares) ~ log10(Population/Land.Area.in.Hectares)) 
summary(model7) 
model8<- lm(log10(Other.Theft/Land.Area.in.Hectares) ~ log10(Population/Land.Area.in.Hectares)) 
summary(model8) 
model9<- lm(log10(Drugs/Land.Area.in.Hectares) ~ log10(Population/Land.Area.in.Hectares)) 
summary(model9) 
model10<-lm(log10(Other.Crimes/Land.Area.in.Hectares) ~ log10(Population/Land.Area.in.Hectares)) 
summary(model10) 
model11<-lm(log10(Bike.Theft/Land.Area.in.Hectares) ~ log10(Population/Land.Area.in.Hectares)) 
summary(model11) 
model12<-lm(log10(Possession.of.Weapons/Land.Area.in.Hectares) ~ log10(Population/Land.Area.in.Hectares)) 
summary(model12) 
model13<-lm(log10(Public.Order/Land.Area.in.Hectares) ~ log10(Population/Land.Area.in.Hectares)) 
summary(model13) 
model14<-lm(log10(Theft.From.the.Person/Land.Area.in.Hectares) ~ log10(Population/Land.Area.in.Hectares)) 
summary(model14) 

#############Visual for linearity 
#par(mfrow = c (2,7)) 
par(mar = c(2, 2, 1, 1)) 
plot(log10(Population/Land.Area.in.Hectares),log10(Anti.Social.Behaviour/Land.Area.in.Hectares), xlab = "log10(Population Density)", ylab = "log10(Anti Social Behaviour Density)", main = "Anti.Social.Behaviour", cex.lab =1.5) 
abline(model1, col = "red", Lwd = 2) 

plot(log10(Population/Land.Area.in.Hectares),log10(Burglary/Land.Area.in.Hectares), xlab = "log10(Population Density)", ylab = "log10(Burglary Density)", main = "Burglary", cex.lab =1.5) 
abline(model2, col = "red", Lwd = 2) 

plot(log10(Population/Land.Area.in.Hectares),log10(Robbery/Land.Area.in.Hectares), xlab = "log10(Population Density)", ylab = "log10(Robbery Density)", main = "Robbery", cex.lab =1.5) 
abline(model3, col = "red", Lwd = 2) 

plot(log10(Crimedata$Population/Land.Area.in.Hectares),log10(Vehicle.Crimes/Land.Area.in.Hectares), xlab = "log10(Population Density)", ylab = "log10(Vehicle.Crimes Density)", main = "Vehicle.Crimes", cex.lab =1.5) 
abline(model4, col = "red", Lwd = 2) 

plot(log10(Population/Land.Area.in.Hectares),log10(Violent.Crimes/Land.Area.in.Hectares), xlab = "log10(Population Density)", ylab = "log10(Violent.Crimes Density)", main = "Violent.Crimes", cex.lab =1.5) 
abline(model5, col = "red", Lwd = 2) 

plot(log10(Population/Land.Area.in.Hectares),log10(Shoplifting/Land.Area.in.Hectares), xlab = "log10(Population Density)", ylab = "log10(Shoplifting Density)", main = "Robbery", cex.lab =1.5) 
abline(model6, col = "red", Lwd = 2) 

plot(log10(Population/Land.Area.in.Hectares),log10(Criminal.Damage...Arson/Land.Area.in.Hectares), xlab = "log10(Population Density)", ylab = "log10(Criminal.Damage.Arson Density)", main = "Criminal.Damage.Arson", cex.lab =1.5) 
abline(model7, col = "red", Lwd = 2) 

plot(log10(Population/Land.Area.in.Hectares),log10(Other.Theft/Land.Area.in.Hectares), xlab = "log10(Population Density)", ylab = "log10(Other.Theft Density)", main = "Other.Theft", cex.lab =1.5) 
abline(model8, col = "red", Lwd = 2) 

plot(log10(Population/Land.Area.in.Hectares),log10(Drugs/Land.Area.in.Hectares), xlab = "log10(Population Density)", ylab = "log10(Drugs Density)", main = "Drugs", cex.lab =1.5) 
abline(model9, col = "red", Lwd = 2) 

plot(log10(Population/Land.Area.in.Hectares),log10(Other.Crimes/Land.Area.in.Hectares), xlab = "log10(Population Density)", ylab = "log10(Other.Crimes Density)", main = "Other.Crimes", cex.lab =1.5) 
abline(model10, col = "red", Lwd = 2) 

plot(log10(Population/Land.Area.in.Hectares),log10(Bike.Theft/Land.Area.in.Hectares), xlab = "log10(Population Density)", ylab = "log10(Bike.Theft Density)", main = "Bike.Theft", cex.lab =1.5) 
abline(model11, col = "red", Lwd = 2) 

plot(log10(Population/Land.Area.in.Hectares),log10(Possession.of.Weapons/Land.Area.in.Hectares), xlab = "log10(Population Density)", ylab = "log10(Possession.of.Weapons Density)", main = "Possession.of.Weapons", cex.lab =1.5) 
abline(model12, col = "red", Lwd = 2) 

plot(log10(Population/Land.Area.in.Hectares),log10(Public.Order/Land.Area.in.Hectares), xlab = "log10(Population Density)", ylab = "log10(Public.Order Density)", main = "Public.Order", cex.lab =1.5) 
abline(model13, col = "red", Lwd = 2) 

plot(log10(Population/Land.Area.in.Hectares),log10(Theft.From.the.Person/Land.Area.in.Hectares), xlab = "log10(Population Density)", ylab = "log10(Theft.From.the.Person Density)", main = "Theft.From.the.Person", cex.lab =1.5) 
abline(model14, col = "red") 



#############visual for normality 
par(mar = c(2, 2, 1, 1)) 
hist(model1$residuals,main = "Histogram - Anti.Social.Behaviour", xlab = "Residuals",col = "lightblue",cex.lab = 1.5) 
hist(model2$residuals,main = "Histogram - Burglary", xlab = "Residuals", col = "lightblue",cex.lab = 1.5) 
hist(model3$residuals,main = "Histogram - Robbery", xlab = "Residuals",col = "lightblue",cex.lab = 1.5) 
hist(model4$residuals,main = "Histogram - Vehicle Crimes", xlab = "Residuals",col = "lightblue",cex.lab = 1.5) 
hist(model5$residuals,main = "Histogram - Voilent Crimes", xlab = "Residuals",col = "lightblue",cex.lab = 1.5) 
hist(model6$residuals,main = "Histogram - Shoplifting", xlab = "Residuals",col = "lightblue",cex.lab = 1.5) 
hist(model7$residuals,main = "Histogram - Criminal.Damage.Arson", xlab = "Residuals",col = "lightblue",cex.lab = 1.5) 
hist(model8$residuals,main = "Histrogram - Other Theft", xlab = "Residuals",col = "lightblue",cex.lab = 1.5) 
hist(model9$residuals,main = "Histrogram - Drugs", xlab = "Residuals",col = "lightblue",cex.lab = 1.5) 
hist(model10$residuals,main = "Histrogram - Other Crimes", xlab = "Residuals",col = "lightblue",cex.lab = 1.5) 
hist(model11$residuals,main = "Histrogram - Bike Theft", xlab = "Residuals",col = "lightblue",cex.lab = 1.5) 
hist(model12$residuals,main = "Histrogram - Possession.of.Weapons", xlab = "Residuals",col = "lightblue",cex.lab = 1.5) 
hist(model13$residuals,main = "Histrogram - Public Order", xlab = "Residuals",col = "lightblue",cex.lab = 1.5) 
hist(model14$residuals,main = "Histrogram - Theft.From.the.Person", xlab = "Residuals",col = "lightblue",cex.lab = 1.5) 

#############Visual for independence 
#par(mfrow= c(2,7)) 
par(mar = c(2, 2, 1, 1)) 
residual1 <- residuals(model1) 
plot(residual1[-length(residual1)],residual1[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]), pch = 19, cex = 0.2, main = "Anti.Social.Behaviour", cex.lab = 1.5) 

residual2 <- residuals(model2) 
plot(residual2[-length(residual2)],residual2[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]), pch = 19, cex = 0.2, main = "Burglary", cex.Lab= 1.5) 

residual3 <- residuals(model3) 
plot(residual3[-length(residual3)],residual3[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]), pch = 19, cex = 0.2, main = "Robbery", cex.Lab= 1.5) 

residual4 <- residuals(model4) 
plot(residual4[-length(residual4)],residual4[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]), pch = 19, cex = 0.2, main = "Vehicle Crimes", cex.Lab= 1.5) 

residual5 <- residuals(model5) 
plot(residual5[-length(residual5)],residual5[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]), pch = 19, cex = 0.2, main = "Voilent Crimes", cex.Lab= 1.5) 

residual6 <- residuals(model6) 
plot(residual6[-length(residual6)],residual6[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]), pch = 19, cex = 0.2, main = "Shoplifting", cex.Lab= 1.5) 

residual7 <- residuals(model7) 
plot(residual7[-length(residual7)],residual7[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]), pch = 19, cex = 0.2, main = "Criminal Damage Arson", cex.Lab= 1.5) 

residual8 <- residuals(model8) 
plot(residual8[-length(residual8)],residual8[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]), pch = 19, cex = 0.2, main = "Other Theft", cex.Lab= 1.5) 

residual9 <- residuals(model9) 
plot(residual9[-length(residual9)],residual9[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]), pch = 19, cex = 0.2, main = "Drugs", cex.Lab= 1.5) 

residual10 <- residuals(model10) 
plot(residual10[-length(residual10)],residual10[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]), pch = 19, cex = 0.2, main = "Other Crimes", cex.Lab= 1.5) 

residual11 <- residuals(model11) 
plot(residual11[-length(residual11)],residual11[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]), pch = 19, cex = 0.2, main = "Bike Theft", cex.Lab= 1.5) 

residual12 <- residuals(model12) 
plot(residual12[-length(residual12)],residual12[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]), pch = 19, cex = 0.2, main = "Possession of Weapons", cex.Lab= 1.5) 

residual13 <- residuals(model13) 
plot(residual13[-length(residual13)],residual13[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]), pch = 19, cex = 0.2, main = "Public Order", cex.Lab= 1.5) 

residual14 <- residuals(model14) 
plot(residual14[-length(residual14)],residual14[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]), pch = 19, cex = 0.2, main = "Theft From the Person", cex.Lab= 1.5) 


#############check for constant variance 
#is the system homoscedastic? 
par(mfrow= c(2,7))
plot(fitted(model1),residuals(model1), xlab = "Fitted", ylab = "Residuals", main = "Anti.Social.Behaviour", cex.lab= 1.5) 
abline(h = 0, lwd =2, col = 2) 
plot(fitted(model2),residuals(model2), xlab = "Fitted", ylab = "Residuals", main = "Burglary", cex.lab= 1.5) 
abline(h = 0, lwd =2, col = 2) 
plot(fitted(model3),residuals(model3), xlab = "Fitted", ylab = "Residuals", main = "Robbery", cex.lab= 1.5) 
abline(h = 0, lwd =2, col = 2) 
plot(fitted(model4),residuals(model4), xlab = "Fitted", ylab = "Residuals", main = "Vehicle Crimes", cex.lab= 1.5) 
abline(h = 0, lwd =2, col = 2) 
plot(fitted(model5),residuals(model5), xlab = "Fitted", ylab = "Residuals", main = "Voilent Crimes", cex.lab= 1.5) 
abline(h = 0, lwd =2, col = 2) 
plot(fitted(model6),residuals(model6), xlab = "Fitted", ylab = "Residuals", main = "Shoplifting", cex.lab= 1.5) 
abline(h = 0, lwd =2, col = 2) 
plot(fitted(model7),residuals(model7), xlab = "Fitted", ylab = "Residuals", main = "Criminal Damage Arson", cex.lab= 1.5) 
abline(h = 0, lwd =2, col = 2) 
plot(fitted(model8),residuals(model8), xlab = "Fitted", ylab = "Residuals", main = "Other Theft", cex.lab= 1.5) 
abline(h = 0, lwd =2, col = 2) 
plot(fitted(model9),residuals(model9), xlab = "Fitted", ylab = "Residuals", main = "Drugs", cex.lab= 1.5) 
abline(h = 0, lwd =2, col = 2) 
plot(fitted(model10),residuals(model10), xlab = "Fitted", ylab = "Residuals", main = "Other Crimes", cex.lab= 1.5) 
abline(h = 0, lwd =2, col = 2) 
plot(fitted(model11),residuals(model11), xlab = "Fitted", ylab = "Residuals", main = "Bike Theft", cex.lab= 1.5) 
abline(h = 0, lwd =2, col = 2) 
plot(fitted(model12),residuals(model12), xlab = "Fitted", ylab = "Residuals", main = "Possession of Weapons", cex.lab= 1.5) 
abline(h = 0, lwd =2, col = 2) 
plot(fitted(model13),residuals(model13), xlab = "Fitted", ylab = "Residuals", main = "Public Order", cex.lab= 1.5) 
abline(h = 0, lwd =2, col = 2) 
plot(fitted(model14),residuals(model14), xlab = "Fitted", ylab = "Residuals", main = "Theft From the Person", cex.lab= 1.5) 
abline(h = 0, lwd =2, col = 2) 




#b) 
#############Create the crimedataresidual data frame/scatterplot
crimedataresidual <- data.frame(model1$residuals, model2$residuals, model3$residuals, model4$residuals, model5$residuals, model6$residuals, model7$residuals,model8$residuals, model9$residuals, model10$residuals, model11$residuals, model12$residuals, model13$residuals, model14$residuals) 

# Assign separate colors to each column in the data frame 
colnames(crimedataresidual) <- c("Anti.Social.Behaviour","Burglary","Robbery","Vehicle Crimes","Voilent Crimes","Shoplifting","Criminal Damage Arson","Other Theft","Drugs","Other Crimes","Bike Theft", "Possession of Weapons","Public Order","Theft From the Person") 
colors <- c("blue", "red", "green", "orange", "purple", "cyan", "magenta", "yellow", "gray", "brown", "pink", "darkgreen", "darkblue", "black") 
plot(crimedataresidual, col = colors, cex = 0.2) 


#############Dendrogram 

dist <- dist(crimedataresidual)
a <- hclust(dist, method = "complete")
par(cex = 0.4, cex.lab = 0.5)
plot(a, col= "darkred", cex.axis = 0.2)


############# HeatMAP 
install.packages("ggcorrplot") 
install.packages("ggplot") 
library(ggcorrplot) 

# calulate the correlations 
htmap <- cor(crimedataresidual, use="complete.obs") 
round(htmap,2) 
ggcorrplot(htmap, 
           hc.order = TRUE, 
           type = "full", 
           lab = TRUE) + 
  scale_fill_gradient(low = "white", 
                      high = "red", 
                      guide = "colorbar") + 
  theme( 
    text = element_text(size = 14), # adjust font size 
    plot.title = element_text(size = 16), # adjust title font size 
    plot.subtitle = element_text(size = 14), # adjust subtitle font size 
  ) + 
  theme( 
    plot.margin = unit(c(1, 1, 1, 1), "cm") # adjust margins 
  ) 



############ k-means Clustering
# To perform k-means clustering on this dataset, first need to preprocess the data.then we have to remove the columns 'LSOA' and 'Name', as they are not relevant for clustering. can also normalize the remaining columns using the scale() function:

#crimedata_processed <- scale(crimedata[, -c(1, 2)])

# I need to specify the number of clusters I want to create. For this, I can use the elbow method to determine the optimal number of clusters. The elbow method involves plotting the within-cluster sum of squares (WSS) against the number of clusters. The WSS is the sum of the squared distances between each point and its assigned cluster center.

library(factoextra)

wss <- c()
for (i in 1:10) {
  k <- kmeans(crimedataresidual, centers = i)
  wss[i] <- k$tot.withinss
}

fviz_nbclust(crimedataresidual, kmeans, method = "wss") + 
  geom_vline(xintercept = 3, linetype = 2)


#  testing 1 to 10 clusters and storing the WSS for each number of clusters. Then, plotting the WSS values against the number of clusters, and adding a vertical line at 3 (which is the number of clusters we'll choose).

#The resulting elbow plot shows that the optimal number of clusters is 3. To confirm this by examining the plot and noting where the "elbow" or bend in the curve occurs.

#Next, perform k-means clustering with 3 clusters using the kmeans() function:

k <- kmeans(crimedataresidual, centers = 3)

# Finally,  visualize the clustering results using a scatter plot.  color each point according to its cluster membership:
library(ggplot2)
crimedata_clustered <- data.frame(crimedataresidual, cluster = as.factor(k$cluster))
ggplot(crimedata_clustered, aes(x = Anti.Social.Behaviour, y = Burglary, color = cluster)) + 
  geom_point() + 
  labs(x = "Anti-Social Behaviour", y = "Burglary")

# This creates a scatter plot of Anti-Social Behaviour versus Burglary, with each point colored according to its cluster membership. The resulting plot shows that the clusters are well separated.
fviz_cluster(k, data = crimedataresidual, geom = "point", 
             ellipse.type = "convex", palette = "jco", ggtheme = theme_minimal(),
             main = "K-means clustering of crimedata (k = 3)")



#PART 3 
#############################CRIME RATE
regio = gsub("0.*$", "", Name)

crimedata = cbind(crimedata, regio)
#summing the entire crimes
Total_Crime = (crimedata$Anti.Social.Behaviour+crimedata$Burglary	+ crimedata$Robbery	+ crimedata$Vehicle.Crimes+crimedata$Violent.Crimes + crimedata$Shoplifting+ crimedata$Criminal.Damage...Arson+ crimedata$Other.Theft	+
                 crimedata$Drugs	+ crimedata$Other.Crimes	+ crimedata$Bike.Theft	+ crimedata$Possession.of.Weapons	+ crimedata$Public.Order	+ 
                 crimedata$Theft.From.the.Person)

#finding the crime density
Total_Crime_Rate = round((Total_Crime/(crimedata$Land.Area.in.Hectares * 10000)), 3)

#joining the new columns to the dataframe
crimedata = cbind(crimedata, Total_Crime, Total_Crime_Rate)

# Create the bar plot
ggplot(crimedata, aes(x = regio, y = Total_Crime_Rate, fill = regio)) +
  geom_bar(stat = "identity") +
  labs(title = "Crime Rate by Region", x = "Region", y = "Count")


##############
# Simple Horizontal Bar Plot with Added Labels
counts <- table(crimedata$regio)
barplot(counts, main="Region Count Plot", horiz=F, col = "Green")

# Distribution of Population
ggplot(crimedata, aes(x= log10(Population)))+
  geom_histogram(color="pink", fill="red") +
  xlab("Population") + ylab("Count") + ggtitle("Population Distribution")



#Geo mapping  
#instructions 
#1) download shapefiles 
#2) install packages 
#3) run libraries 
#4) change directory location in this code. There are two places where you need to do this. 
#5) explore how to make maps (e.g. add colour etc.) 



#install packages 
install.packages("sf") 
install.packages("raster") 
install.packages("dplyr") 
install.packages("spData") 
install.packages("tmap") 
install.packages("ggplot2") 
install.packages("rgdal") 
install.packages("png") 
install.packages("rgeos") 
install.packages("rio") 

#libraries 
library(sf) 
library(raster) 
library(dplyr) 
library(spData) 
library(tmap)    # for static and interactive maps 
library(ggplot2) # tidyverse data visualization package 
library(rgdal) 
library(png) 
library(rgeos) 
library(rio) 

#download shape files to directory 
#change the directory location below 
shp2<- shapefile("/Users/inaolajiteminiola/Desktop/Information Visualisation/Assessment VI/Lower_Layer_Super_Output_Areas_(December_2011)_Boundaries_Super_Generalised_Clipped_(BSC)_EW_V3.shx") 


#import your data 
regions<- rio::import("AssessmentCrimeData.csv") 

#make sure that the column name for LSOA codes  match with the name in the shapefiles. In this case the columns name is "LSOA11CD". 
shp_main<-subset(shp2, LSOA11CD %in% regions$LSOA) 

#add variables to the shapefile. for instance crime data 
shp_main$"LSOA Code"<-regions$LSOA11CD 
shp_main$"Region Name"<-regions$Name 
shp_main$"Land Area"<- regions$`Land Area (Hectares)` 
shp_main$"Population"<-regions$Population 
shp_main$"Population Density"<- regions$Population/regions$`Land Area in Hectares` 
shp_main$"log10(ASB Density)"<- log10(regions$`Anti-Social Behaviour`/regions$`Land Area in Hectares`) 
shp_main$"log10(Burglary Density)"<- log10(regions$Burglary/regions$`Land Area in Hectares`) 
shp_main$"log10(Robbery Density)"<- log10(regions$Robbery/regions$`Land Area in Hectares`) 
shp_main$"log10(Vehicle.Crimes Density)"<- log10(regions$Vehicle.Crimes /regions$`Land Area in Hectares`) 
shp_main$"log10(Violent.Crimes Density)"<- log10(regions$Violent.Crimes/regions$`Land Area in Hectares`) 
shp_main$"log10(Shoplifting Density)"<- log10(regions$Shoplifting/regions$`Land Area in Hectares`) 
shp_main$"log10(C.D.A Density)"<- log10(regions$Criminal.Damage...Arson/regions$`Land Area in Hectares`) 
shp_main$"log10(Other.Theft Density)"<- log10(regions$Other.Theft/regions$`Land Area in Hectares`) 
shp_main$"log10(Drugs Density)"<- log10(regions$Drugs/regions$`Land Area in Hectares`) 
shp_main$"log10(Other.Crimes Density)"<- log10(regions$Other.Crimes/regions$`Land Area in Hectares`) 
shp_main$"log10(Bike Thefts Density)"<- log10(regions$Bike.Thefts/regions$`Land Area in Hectares`) 
shp_main$"log10(P.O.W Density)"<- log10(regions$Possession.of.Weapons/regions$`Land Area in Hectares`) 
shp_main$"log10(Public.Order Density)"<- log10(regions$Public.Order/regions$`Land Area in Hectares`) 
shp_main$"log10(Theft.From.the.Person Density)"<- log10(regions$Theft.From.the.Person/regions$`Land Area in Hectares`) 

#example 1 
#this is the code to make a map. Currently this will make a map of Derbyshire colored to population density 
tm_shape(shp_main) + 
  
  tm_polygons ("Population Density", palette = "YlGn") + 
  tm_layout + 
  tm_borders(lwd=1) 


#example 2 
#this is the code to make a map. Currently this will make a map of Derbyshire colored to log10(ASB Density) 
#ASB 
tm_shape(shp_main) + 
  
  tm_polygons ("log10(ASB Density)", palette = "YlGn") + 
  tm_layout + 
  tm_borders(lwd=1) 

#Burglary  
tm_shape(shp_main) + 
  
  tm_polygons ("log10(Burglary Density)") + 
  tm_layout + 
  tm_borders(lwd=1) 

#Robbery 
tm_shape(shp_main) + 
  
  tm_polygons ("log10(Robbery Density)") + 
  tm_layout + 
  tm_borders(lwd=1) 


#Vehicle.Crimes  
tm_shape(shp_main) + 
  
  tm_polygons ("log10(Vehicle.Crimes Density)") + 
  tm_layout + 
  tm_borders(lwd=1) 

#Violent.Crimes  
tm_shape(shp_main) + 
  
  tm_polygons ("log10(Violent.Crimes Density)") + 
  tm_layout + 
  tm_borders(lwd=1) 

#Shoplifing  
tm_shape(shp_main) + 
  
  tm_polygons ("log10(Shop4lifting Density)") + 
  tm_layout + 
  tm_borders(lwd=1) 

#Criminal.Damage...Arson 
tm_shape(shp_main) + 
  
  tm_polygons ("log10(Criminal.Damage...Arson.Crimes Density)") + 
  tm_layout + 
  tm_borders(lwd=1) 

#Other.Theft 
tm_shape(shp_main) + 
  
  tm_polygons ("log10(Other.Theft Density)") + 
  tm_layout + 
  tm_borders(lwd=1) 


#Drugs 
tm_shape(shp_main) + 
  
  tm_polygons ("log10(Drugs Density)") + 
  tm_layout + 
  tm_borders(lwd=1) 


#Other.Crimes 
tm_shape(shp_main) + 
  
  tm_polygons ("log10(Other.Crimes Density)") + 
  tm_layout + 
  tm_borders(lwd=1) 

#Bike.Thefts  
tm_shape(shp_main) + 
  
  tm_polygons ("log10(Bike.Thefts Density)") + 
  tm_layout + 
  tm_borders(lwd=1) 


#Possession.of.Weapons  
tm_shape(shp_main) + 
  
  tm_polygons ("log10(Possession.of.Weapons Density)") + 
  tm_layout + 
  tm_borders(lwd=1) 

#Public.Order 
tm_shape(shp_main) + 
  
  tm_polygons ("log10(Public.Order Density)") + 
  tm_layout + 
  tm_borders(lwd=1) 

#Theft.From.the.Person 
tm_shape(shp_main) + 
  
  tm_polygons ("log10(Theft.From.the.Person Density)") + 
  tm_layout + 
  tm_borders(lwd=1) 





