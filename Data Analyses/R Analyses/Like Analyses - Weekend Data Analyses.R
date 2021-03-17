setwd("~/Documents/EAR/Dysfluency Project")
data=read.csv("weekend-t-test.csv")


# Variables
weekday=data$weekday
weekend=data$weekend
weekdaylike=data$weekdaylike
weekendlike=data$weekendlike

#Weekend vs Weekday
t.test(weekday,weekend)
#p=0.184

#Weekend Like* vs Weekday Like*
t.test(weekdaylike, weekendlike)
#p=0.091
#t=1.70


#Histograms

hist(weekday,
     main="Amount of speech on Weekdays",
     xlab="Percentage",
     xlim=c(0,100),
     ylim=c(0, 30),
     col="magenta",
     
)

hist(weekend,
     main="Amount of speech on Weekends",
     xlab="Percentage",
     xlim=c(0,100),
     ylim=c(0, 30),
     col="darkmagenta",
     
)


hist(weekdaylike,
     main="Usage of Like* on Weekdays",
     xlab="Percentage",
     xlim=c(0,100),
     ylim=c(0,25),
     col="lightgreen",
     
)

hist(weekendlike,
     main="Usage of Like* on Weekends",
     xlab="Percentage",
     xlim=c(0,100),
     
     col="darkgreen",
     
)