setwd('S:/Shared Folders/MH/Data Science KC/Tobi')

checkin=read.csv(file.choose(), header=T)
business=read.csv(file.choose(), header=T)
review=read.csv(file.choose(), header=T)
user=read.csv(file.choose(), header=T)

#looking at a sample of data
head(checkin, 2)
head(business, 3)
head(review, 2)
head(user, 3)

#finding out the number of rows in each dataset
nrow(checkin)
nrow(business)
nrow(review)
nrow(user)

colnames(checkin)
colnames(business)
colnames(review)
colnames(user)

#changing column names so they are not duplicated
colnames(checkin)[170]="type.Checkin"
colnames(business)[10]="review_count.Business"
colnames(business)[7]="name.Business"
colnames(business)[13]="type.Business" 
colnames(business)[11]="stars.Business"
colnames(review)[6]="type.Review" 
colnames(review)[4]="stars.Review"
colnames(review)[8]="votes_cool.Review"
colnames(review)[9]="votes_funny.Review"
colnames(review)[10]="votes_useful.Review"
colnames(user)[4]="type.User"
colnames(user)[3]="review_count.User"
colnames(user)[6]="votes_cool.User"
colnames(user)[7]="votes_funny.User"
colnames(user)[8]="votes_useful.User"

#finding overlap and joining datasets (probably a more efficient way to do this)
intersect(colnames(business),colnames(review))
combined1=merge(business,review, "business_id")
intersect(colnames(combined1),colnames(user))
combined2=merge(combined1,user, "user_id")
intersect(colnames(combined2),colnames(checkin))
combined3=merge(combined2,checkin, "business_id")

#checking changes were properly done
head(combined3, 2)
nrow(combined1) # 226180
nrow(combined2) #209982
nrow(combined3) #194982
# => 229907-194982=34925 missing?
nrow(na.omit(review)) #229907 still
sum(is.na(review)) #0
nrow(na.omit(business)) #0 all rows have N/A?
sum(is.na(business)) #11,537
nrow(na.omit(user))
sum(is.na(user)) #0
nrow(na.omit(checkin)) #0 all rows have N/A
sum(is.na(checkin)) #112,8612 N/As
length(names(combined3)) #198
unique(combined3$categories) #2061

#creating correlation matrix
cor_frame=data.frame(combined3$latitude, combined3$longitude, combined3$review_count.Business, combined3$stars.Business, combined3$stars.Review, combined3$votes_cool.Review, combined3$votes_funny.Review, combined3$votes_useful.Review, combined3$average_stars, combined3$review_count.User, combined3$votes_cool.User, combined3$votes_funny.User, combined3$votes_useful.User)
head(cor_frame,2)
cor(cor_frame) #review stars are best predicted by business stars (.4309920973) and average stars (.4628016494). business review count is next with .0950607272, useful??
#votes_cool.Review 0.0474422013, votes_funny.Review -0.0587699639, votes_useful.Review -0.0324664238
nrow(cor_frame)


mean(review$stars.Review) # 3.766723
mean(combined3$stars.Review) # 3.776605, use for N/As

nrow(user1) #43841, only accounts for 1 N/A, review or business does not have matching user id?? or just blank, no N/As?
checkin1=na.omit(read.csv("checkin.csv", sep="|", header=T))
nrow(checkin1) #no N/As, still 8282
review2=read.csv("review1.csv", sep="|", header=T)
colnames(review2)[1]="id.Review"
colnames(review2)[9]="type.Review" 
colnames(review2)[7]="stars.Review"
colnames(review2)[2]="votes.Review"

#this did not do what I wanted :(, R anomalies
for (i in review2)
    {if (is.na(i[7])==T)
      {i[7]=3.866772}
     }
nrow(review2)    
review2[7=="NA",]=3.866772

#running some regressions
pearson=cor.test(combined3$stars.Review,combined3$stars.Business) #significant
pearson.pvalue #?
plot(combined3$stars.Review,combined3$stars.Business, pch=20)
line_business=lm(combined3$stars.Review~ combined3$stars.Business) # y=0.1992x +3.0275
abline(line_business)

cor.test(combined3$stars.Review,combined3$average_stars) #significant
plot(combined3$stars.Review,combined3$average_stars,cex=.8)
line_average=lm(combined3$stars.Review~ combined3$average_stars) #y=0.223x +2.914
abline(line_average) 

plot(combined3$review_count.User, combined3$average_stars, cex=.4)
plot(combined3$review_count.User, combined3$average_stars, xlim=c(0,30), cex=.4)
model=lm(formula = combined3$stars.Review ~ combined3$average_stars + combined3$stars.Business)
model #y=0.8334average_stars+ 0.7861*stars.Business -2.3249
