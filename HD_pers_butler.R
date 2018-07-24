#Step 1: Sort all the rows of the matrix theta_1, independently of each other
s_theta_pers_il<-t(apply(theta_pers_il, 1, sort, decreasing = TRUE))

#Step 2: Similarly for theta_2
s_theta_vos_il<-t(apply(theta_vos_il, 1, sort, decreasing = TRUE)) 

#Step 3: Compute the Hellinger distance
Diff_pers_vos<-(sqrt(s_theta_pers_il)-sqrt(s_theta_vos_il))**2 
#Distance per document
HD_pers_vos0<-rowSums(Diff_pers_vos) 
#Mean for all documents
HD_pers_vos<-mean(HD_pers_vos0)

