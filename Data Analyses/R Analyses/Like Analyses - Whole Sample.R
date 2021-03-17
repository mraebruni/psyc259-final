Like* Scores Correlations - Whole Sample
10-16-2020


library(ggplot2)

setwd("~/Documents/EAR/Dysfluency Project")

data=read.csv("like-analyses-whole-sample.csv")

# Variables
subject=data$subject
englishwords=data$english.words
total=data$total
one=data$one
two=data$two
three=data$three
four=data$four
otherspeak=data$other.speak
otherread=data$other.read
otherwrite=data$other.write
otherunder=data$other.under
ageexposedeng=data$age.exposed.eng
ageexposedother=data$age.exposed.other
currentexposother=data$current.expos.other
currentuseother=data$current.use.other
timespeakengfamily=data$time.speak.eng.family
timespeakengfriends=data$time.speak.eng.friends
timespeakotherfamily=data$time.speak.other.family
timespeakotherfriends=data$time.speak.other.friends
cs1=data$cs.1
cs2=data$cs.2
cs3=data$cs.3
cs4=data$cs.4
numberengfiles=data$number.eng.files
propengonly=data$propengonly
apt=data$apt
classroom=data$classroom
intransit=data$intransit
intransitother=data$intransitother
resto=data$resto
shopping=data$shopping
otherpublic=data$otherpublic



# Correlations

cor.test(englishwords, total)
plot(englishwords, total)

reg1 = lm(total~englishwords, data=data)

abline(reg1)

#p = 0.004
#r = 0.324

cor.test(englishwords, one)
#p = 0.059
#r = 0.217

cor.test(englishwords, two)
#p = 0.065
#r = 0.212

cor.test(englishwords, three)
plot(englishwords, three)
#p = 0.005
#r = 0.318

cor.test(englishwords, four)
plot(englishwords, four)
#p = 0.023
#r = 0.259

-----------------------------------------------------------

# Other Speak, Read, Write, Understand
  
cor.test(total, otherspeak)
cor.test(total, otherread)

reg2=lm(total~otherread)
plot(total, otherread)
abline(reg2)

#p = 0.0081
#r = -0.307
  
cor.test(total, otherwrite)
plot(total, otherwrite)
#p = 0.0014
#r = -0.367

cor.test(total, otherunder)

### Other Speak
cor.test(one, otherspeak)
cor.test(two, otherspeak)

lm(otherspeak ~ two)
summary(lm(otherspeak ~ two))
## THIS WORKED vv ??? not sure why
plot(two, otherspeak)
abline(lm(otherspeak ~ two))

#p = 0.01
#r = -0.297
cor.test(three, otherspeak)
cor.test(four, otherspeak)

### Other Read
cor.test(one, otherread)
cor.test(two, otherread)
plot(two, otherread)

lm(otherread ~ two)
summary(lm(otherread ~ two))
## THIS WORKED
plot(two, otherread)
abline(lm(otherread ~ two))

#p = 0.006
#r = -0.315

cor.test(three, otherread)
#p = 0.012
#r = -0.291
cor.test(four, otherread)
#p = 0.013
#r = -0.289

### Other Write
cor.test(one, otherwrite)
cor.test(two, otherwrite)

lm(otherwrite ~ two)
summary(lm(otherwrite ~ two))
## THIS WORKED
plot(two, otherwrite)
abline(lm(otherwrite ~ two))
#p = 0.002
#r = -0.349

cor.test(three, otherwrite)
#p = 0.004
#r = -0.326
cor.test(four, otherwrite)
plot(four, otherwrite)
#p = 0.002
#r = -0.345

### Other Understand
cor.test(one, otherunder)
cor.test(two, otherunder)
plot(two, otherunder)

lm(otherunder ~ two)
summary(lm(otherunder ~ two))
## THIS WORKED
plot(two, otherunder)
abline(lm(otherunder ~ two))

#p = 0.004
#r = -0.329
cor.test(three, otherunder)
cor.test(four, otherunder)


-----------------------------------------------------------
  
# Age Exposed to English and Other
  
cor.test(total, ageexposedeng)
cor.test(total, ageexposedother)
  
cor.test(one, ageexposedeng) 
cor.test(two, ageexposedeng)

## THIS WORKED
plot(two, ageexposedeng)
abline(lm(ageexposedeng ~ two))

#p = 0.010
#r = -0.294

cor.test(three, ageexposedeng)
cor.test(four, ageexposedeng)


cor.test(one, ageexposedother)
cor.test(two, ageexposedother)
plot(two, ageexposedother)

plot(two, ageexposedother)
abline(lm(ageexposedother ~ two))

#p = 0.012
#r = 0.288
cor.test(three, ageexposedother)
cor.test(four, ageexposedother)


-----------------------------------------------------------
  
# Current Exposure and Use of Other
  
cor.test(total, currentexposother)
cor.test(total, currentuseother)

cor.test(one, currentexposother)
cor.test(two, currentexposother)
plot(two, currentexposother)
abline(lm(currentexposother ~ two))

#p < .0001
#r = -0.441

cor.test(three, currentexposother)
cor.test(four, currentexposother)

cor.test(one, currentuseother)
cor.test(two, currentuseother)
plot(two, currentuseother)

plot(two, currentuseother)
abline(lm(currentuseother ~ two))

#p = 0.0004
#r = -0.398
cor.test(three, currentuseother)
cor.test(four, currentuseother)


-----------------------------------------------------------

 # Time Speaking English to Family and Friends
  
cor.test(total, timespeakengfamily)
cor.test(total, timespeakengfriends)
plot(total, timespeakengfriends)
#p = 0.07

cor.test(one, timespeakengfamily)
cor.test(two, timespeakengfamily)
cor.test(three, timespeakengfamily)
cor.test(four, timespeakengfamily)

cor.test(one, timespeakengfriends)
cor.test(two, timespeakengfriends)

plot(two, timespeakengfriends)
abline(lm(timespeakengfriends ~ two))

#p = 0.01
#r = 0.293
cor.test(three, timespeakengfriends)
cor.test(four, timespeakengfriends)


-----------------------------------------------------------
  
# Time Speaking Other to Family and Friends
  
cor.test(total, timespeakotherfamily)
cor.test(total, timespeakotherfriends)
#p = 0.0004
#r = -0.400

cor.test(one, timespeakotherfamily)
cor.test(two, timespeakotherfamily)
cor.test(three, timespeakotherfamily)
cor.test(four, timespeakotherfamily)

cor.test(one, timespeakotherfriends)
#p = 0.005
#r = -0.317
cor.test(two, timespeakotherfriends)
#p = 0.001
#r = -0.366
cor.test(three, timespeakotherfriends)
#p = 0.004
#r = -0.325
cor.test(four, timespeakotherfriends)
#p = 0.003
#r = -0.336


-----------------------------------------------------------
  
# Codeswitching  

cor.test(total, cs1)
cor.test(total, cs2)
cor.test(total, cs3)  
cor.test(total, cs4)  

cor.test(one, cs1)  
cor.test(two, cs1)
cor.test(three, cs1)
cor.test(four, cs1)

cor.test(one, cs2)
cor.test(two, cs2)
#p = 0.016
#r = -0.274
cor.test(three, cs2)
cor.test(four, cs2)

cor.test(one, cs3)
cor.test(two, cs3)
#p = 0.015
#r = -0.276
cor.test(three, cs3)
cor.test(four, cs3)

cor.test(one, cs4)
cor.test(two, cs4)
cor.test(three, cs4)
cor.test(four, cs4)

-----------------------------------------------------------
  
# Number of English Only Files

cor.test(total, propengonly)
#p = 0.015
#r = 0.279

cor.test(one, propengonly)
#p=0.087
#r=0.197

cor.test(two, propengonly)

#p = 0.003
#r=0.334
plot(two, propengonly)
abline(lm(propengonly ~ two))

cor.test(three, propengonly)
#p=0.035
#r=0.241
cor.test(four, propengonly)
#p=0.056
#r=0.219


-----------------------------------------------------------
  
# Qualtrics Contexts
  
cor.test(total, apt)
#p<0.0001
#r=0.679
plot(total, apt)
abline(lm(apt ~ total))

cor.test(one, apt)
#p<0.0001
#r=0.435

cor.test(two, apt)
#p<0.0001
#r=0.448
cor.test(three, apt)
#p<0.0001
#r=0.614
cor.test(four, apt)
#p<0.0001
#r=0.602

cor.test(classroom, total)
cor.test(one, classroom)
#p=0.0425
#r=0.233
plot(one, classroom)
abline(lm(classroom ~ one))
cor.test(two, classroom)
cor.test(three, classroom)
cor.test(four, classroom)

cor.test(intransit, total)
#p=0.03
#r=0.246
plot(total, intransit)
abline(lm(intransit ~ total))

cor.test(intransit, one)
cor.test(intransit, two)
#p=0.098
#r=0.19
cor.test(intransit, three)
#p=0.042
#r=0.233
cor.test(intransit, four)
#p=0.09
#r=0.19

cor.test(intransitother, total)
cor.test(intransitother, one)
cor.test(intransitother, two)
cor.test(intransitother, three)
cor.test(intransitother, four)

cor.test(resto, total)
#p=0.002
#r=0.339
cor.test(resto, one)
#p=0.09
#r=0.19
cor.test(resto, two)
#p=0.07
#r=0.20
cor.test(resto, three)
#p=0.028
#r=0.251
cor.test(resto, four)
#p=0.001
#r=0.369

cor.test(shopping, total)
cor.test(shopping, one)
cor.test(shopping, two)
cor.test(shopping, three)
cor.test(shopping, four)

cor.test(otherpublic, total)
#p<0.0001
#r=0.417
cor.test(otherpublic, one)
#p=0.002
#r=0.348
cor.test(otherpublic, two)
#p=0.002
#r=0.349
cor.test(otherpublic, three)
#p=0.016
#r=0.274
cor.test(otherpublic, four)
#p<.001
#r=0.408