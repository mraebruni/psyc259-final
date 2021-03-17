Like* Scores Correlations - Phase Two
10-12-2020

setwd("~/Documents/EAR/Dysfluency Project")

data=read.csv("like-analyses-phase-two.csv")

# Variables
subject=data$subject
englishwords=data$english.words
total=data$total
one=data$one
two=data$two
three=data$three
four=data$four
eescore=data$ee.score
ctopp=data$ctopp
minteng=data$mint.eng
vfeng=data$vf.eng


# No relationship between Total Like* Scores and Behavioral tasks
cor.test(total, eescore)
# p = 0.3707

cor.test(total, ctopp)
# p = 0.156

cor.test(total, minteng)
# p = 0.327

cor.test(total, vfeng)
# p = .966




# One

cor.test(one, eescore)
# p = 0.656

cor.test(one, ctopp)
# p = 0.169

cor.test(one, minteng)
# p = 0.778

cor.test(total, vfeng)
# p = .966



# Two

cor.test(two, eescore)
# p = 0.523

cor.test(two, ctopp)
# p = 0.069

cor.test(two, minteng)
# p = 0.286

cor.test(two, vfeng)
# p = .081



# Three

cor.test(three, eescore)
# p = 0.375

cor.test(three, ctopp)
# p = 0.738

cor.test(three, minteng)
# p = 0.200

cor.test(three, vfeng)
# p = .794



# Four

cor.test(four, eescore)
# p = 0.205

cor.test(four, ctopp)
# p = 0.135

cor.test(four, minteng)
# p = 0.502

cor.test(four, vfeng)
# p = .977
