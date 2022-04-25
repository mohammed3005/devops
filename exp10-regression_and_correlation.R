data <- read.csv("regression_dataset.csv")
summary(data)
plot(data)
library(ggplot2)
dat <- data
#Linear Regression
income.happiness.lm <- lm(happiness ~ income, data )
summary(income.happiness.lm)
#Plot of Linear Regression
income.graph<-ggplot(dat, aes(x=income, y=happiness))+
  geom_point() + geom_smooth(method="lm", col="black")+
  labs(title = "Reported happiness as a function of income",
       x = "Income (x$10,000)",
       y = "Happiness score (0 to 10)")
income.graph
#Predicting happiness when income is 7.11
a <- data.frame(income = 9.22)
result <- predict(income.happiness.lm ,a)
result
#Multiple Regression
heart.disease.lm<-lm(heart.disease ~ biking + smoking, data )
summary(heart.disease.lm)
plotting.data<-expand.grid(
  biking = seq(min(data$biking), max(data$biking), length.out=30),
  smoking=c(min(data$smoking), mean(data$smoking),
            max(data$smoking)))
plotting.data$predicted.y <- predict.lm(heart.disease.lm,
                                        newdata=plotting.data)
plotting.data$smoking <- round(plotting.data$smoking, digits = 2)
plotting.data$smoking <- as.factor(plotting.data$smoking)
#Plot of Multiple Regression
ggplot(data, aes(x=biking, y=heart.disease)) +
  geom_point() +
  geom_line(data=plotting.data, aes(x=biking, y=predicted.y,
                                    color=smoking), size=1.25)
#Plot of Correlation

