#Your name: Siyuan Pan
# Course: 44-149 Scientific Computing
# Project3# (e.g.robot vaccum )
# Due Date: 4/20/2018
# Brief: us census and population
# By submitting this, I pledge that the code in this file was written by the author indicated above,
#  and that all assistance from outside sources was correctly attributed in comments.  Additionally, I 
#  agree to abide by the rules expressed in the CSIS Academic Honesty Policy
census <- read.csv('us_census.csv')
contiguous <- census[!census$state %in% c('AK', 'HI', 'PR'),]
plot(contiguous$longitude, contiguous$latitude, col=contiguous$state)
N<-12
chosen_counties<-sample(1:nrow(contiguous),N)
print(chosen_counties)

centers<-matrix(0,nrow=N,ncol=2)

centers[,1]=contiguous$latitude[chosen_counties]
centers[,2]=contiguous$longitude[chosen_counties]

#print(centers)
centers_df=contiguous[chosen_counties,3:4]



dist_sq<-function(county,center){
  
  deltax<-county[1,'latitude']-center[1]
  deltay<-county[1,'longitude']-center[2]
  deltax^2+deltay^2
}

#deltax<-contiguous[1,'latitude']-centers[1,1]
#deltay<-contiguous[1,'longitude']-center[1,2]
#print(deltax^2+deltay^2)

belongs_to<-rep(0,nrow(contiguous))
for(county in 1:nrow(contiguous)){
  closest_center<-1
  closest_distance <- dist_sq(contiguous[county,],centers[1,])
  for(cluster in 2:N){
    d <- dist_sq(contiguous[county,],centers[cluster,])
    if(d < closest_distance){
      closest_distance <- d
      closest_center <- cluster
    }
  }
  belongs_to[county]<-closest_center
} 

plot(contiguous$longitude,contiguous$latitude,type='p',col=belongs_to)
for (i in 1:N){
  clust_of_interest<-contiguous[belongs_to==i,]
  total_pop<-sum(clust_of_interest$population)
  
  print(nrow(clust_of_interest))
  print(sum(belongs_to==1))
  print(sum(clust_of_interest$population))
  
  new_latitude<-sum(clust_of_interest$latitude*clust_of_interest$population)/total_pop
  new_longitude<-sum(clust_of_interest$longitude*clust_of_interest$population)/total_pop
  centers[i,1]<-new_latitude
  centers[i,2]<-new_longitude
  
}
plot(contiguous$longitude,contiguous$latitude,type='p',col=belongs_to)
