#Step 1: Sort all the rows of the matrix theta_1, independently of each other
s_theta_pers_wout13_il<-t(apply(theta_pers_wout13_il, 1, sort, decreasing = TRUE))

#Step 2: Similarly for theta_2
s_theta_pallis_il<-t(apply(theta_pallis_il, 1, sort, decreasing = TRUE)) 

#Step 3: Compute the Hellinger distance
Diff_pers_wout13_pallis<-(sqrt(s_theta_pers_wout13_il)-sqrt(s_theta_pallis_il))**2 
#Distance per document
HD_pers_wout13_pallis0<-rowSums(Diff_pers_wout13_pallis) 
#Mean for all documents
HD_pers_wout13_pallis<-mean(HD_pers_wout13_pallis0)

