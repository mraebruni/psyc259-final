setwd("~/Documents/EAR/Dysfluency Project")
data=read.csv("histograms.csv")


#Variables
female=data$femtofem
male=data$maletomale
femlike=data$femalelike
malelike=data$malelike
femaletomale=data$femaletomale
maletofemale=data$maletofemale

#Female to Female
hist(female,
     main="Female participants talking to female partners",
     xlab="Percentage",
     xlim=c(0,100),
     ylim=c(0,12),
     col="darkmagenta",
    
)

hist(female,
     main="Female participants talking to female partners",
     xlab="Percentage",
     xlim=c(0,100),
     ylim=c(0,30),
     col="darkmagenta",
     
)


#Male to Male
hist(male,
     main="Male participants talking to male partners",
     xlab="Percentage",
     xlim=c(0,100),
     ylim=c(0,12),
     col="darkblue",
     nclass=5
)


hist(male,
     main="Male participants talking to male partners",
     xlab="Percentage",
     xlim=c(0,100),
     ylim=c(0,30),
     col="darkblue",
     nclass=5
)

#Female to Male
hist(femaletomale,
     main="Female participants talking to male partners",
     xlab="Percentage",
     xlim=c(0,100),
     ylim=c(0,30),
     col="lightpink",
     nclass=5
     
)


#Male to Female
hist(maletofemale,
     main="Male participants talking to female partners",
     xlab="Percentage",
     xlim=c(0,100),
     ylim=c(0,30),
     col="plum",
     nclass=5
)



#Female Like
hist(femlike,
     main="Female participants saying Like* to female partners",
     xlab="Percentage",
     xlim=c(0,100),
     ylim=c(0,20),
     col="darkmagenta",
     nclass=5
     
)

#Male Like
hist(malelike,
     main="Male participants saying Like* to male partners",
     xlab="Percentage",
     xlim=c(0,100),
     ylim=c(0,15),
     col="darkblue",
     nclass=4
     
)


