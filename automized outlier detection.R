hist(r$assault)
r[r$assault>mean(r$assault)+2.5*IQR(r$assault),]$assault
nrow(r[r$assault>mean(r$assault)+2.5*IQR(r$assault),]) #4

hist(r$auto.theft)
r[r$auto.theft>mean(r$auto.theft)+2.5*IQR(r$auto.theft),]$auto.theft
nrow(r[r$auto.theft>mean(r$auto.theft)+2.5*IQR(r$auto.theft),]) #5

hist(r$break.and.enter)
r[r$break.and.enter>mean(r$break.and.enter)+2.5*IQR(r$break.and.enter),]$break.and.enter
nrow(r[r$break.and.enter>mean(r$break.and.enter)+2.5*IQR(r$break.and.enter),]) #4

hist(r$robbery)
r[r$robbery>mean(r$robbery)+2.5*IQR(r$robbery),]$robbery
nrow(r[r$robbery>mean(r$robbery)+2.5*IQR(r$robbery),]) #4

hist(r$theft.over)
r[r$theft.over>mean(r$theft.over)+2.5*IQR(r$theft.over),]$theft.over
nrow(r[r$theft.over>mean(r$theft.over)+2.5*IQR(r$theft.over),]) #7

hist(r$drug.arrests)
r[r$drug.arrests>mean(r$drug.arrests)+2.5*IQR(r$drug.arrests),]$drug.arrests
nrow(r[r$drug.arrests>mean(r$drug.arrests)+2.5*IQR(r$drug.arrests),]) #5

hist(r$total.crime)
r[r$total.crime>mean(r$total.crime)+2.5*IQR(r$total.crime),]$total.crime
nrow(r[r$total.crime>mean(r$total.crime)+2.5*IQR(r$total.crime),]) #5
