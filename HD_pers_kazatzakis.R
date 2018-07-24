#Step 1: Sort all the rows of the matrix theta_1, independently of each other
s_theta_pers_il<-apply(theta_pers_il, 1, sort)

#Step 2: Similarly for theta_2
s_theta_kazatzakis_il<-apply(theta_kazatzakis_il, 1, sort) 

#Step 3: Compute the Hellinger distance
Diff_pers_kazatzakis<-(sqrt(s_theta_pers_il)-sqrt(s_theta_kazatzakis_il))**2 
#Distance per document
HD_pers_kazatzakis0<-rowSums(Diff_pers_kazatzakis) 
#Mean for all documents
HD_pers_kazatzakis<-mean(HD_pers_kazatzakis0)


