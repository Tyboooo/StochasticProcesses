#######Calculation for the expected annual profit from country A once
stability has been reached under the discount scheme with 3 discount levels
lambda<- 0.873 # from 0.01*(87+(3*0.1))
number_of_customers<-10000 #number of customers
n<-30 # number of steps to simulate
P<- matrix(c(0.5823, 0.4177, 0, 0.5823, 0, 0.4177, 0, 0.5823, 0.4177),
           nrow=3, ncol=3, byrow=TRUE)
#vectors to store proportion of simulations in each state
levels<-matrix(0, nrow=3, ncol=n+1)
for(j in 1:number_of_customers) {
  X<-1 # initial state of the Markov chain
  levels[X,1]=levels[X,1]+1 #record the initial state
  for(i in 1:n) {
    p<-P[X[i],] #to get p values
    
    X[i+1] <-sample(x=3,size=1, prob=p) #this updates the chain
    levels[X[i+1],i+1]= levels[X[i+1],i+1] + 1
  }
}
levels<-levels/number_of_customers
#calculating the stationary distribution now
finalproportion=levels[,n+1]
finalproportion
#calculating the total premium paid once stability has been reached
premium1<-200
discount_levels<-c(0, 0.15, 0.3)
premium_paid1<-numeric(length=3)
for(i in 1:3){
  premium_paid1[i]<-(finalproportion[i]*number_of_customers*(1-
                                                               discount_levels[i])*premium1)
}

total_premium_paid1<-sum(premium_paid1)

#calculating total claims now
number_of_accidents<- rpois(number_of_customers,lambda) # number of
accidents per customer
cost_of_a_claim1 <- runif(number_of_customers,0,400)
mean(cost_of_a_claim1) # expected amount of a claim
total_claimed_amount1<-
  (sum(number_of_accidents))*mean(cost_of_a_claim1) #expected total of the
claimed amount
annual_profit_A1=total_premium_paid1 - total_claimed_amount1 # expected
annual profit once stability has been reached for Country A under discount
scheme 1

######Calculation for the expected annual profit from country B once stability
has been reached under the discount scheme with 3 discount levels
premium2<-300
discount_levels<-c(0, 0.15, 0.3)
premium_paid2<-numeric(length=3)
for(i in 1:3){
  premium_paid2[i]<-(finalproportion[i]*number_of_customers*(1-
                                                               discount_levels[i])*premium2)
}

total_premium_paid2<-sum(premium_paid2)
#calculating total claims now
number_of_accidents<- rpois(number_of_customers,lambda) # number of
accidents per customer
cost_of_a_claim2 <- runif(number_of_customers,0,600)
mean(cost_of_a_claim2) # expected amount of a claim
total_claimed_amount2<-
  (sum(number_of_accidents))*mean(cost_of_a_claim2) #expected total of the
claimed amount
annual_profit_B1=total_premium_paid2 - total_claimed_amount2 # expected
annual profit once stability has been reached for Country B under discount
scheme 1
#plotting the progression of the proportion at each state
plot(levels[1,], type='l', xlim=c(0,n+1), ylim=c(0,1), xlab='Number of years',
     ylab='Proportion', col='orange',lwd=2)
lines(levels[2,], col='green', lwd=2)
lines(levels[3,], col='blue',lwd=2)
legend(15,1, c('Discount level 1 (0%)', 'Discount level 2 (15%)', 'Discount level 3
(30%)'),
       
       col=c('orange','green','blue'), lwd=c(2,2,2))
##Early years' profits for country A under the discount scheme with 3 levels
prop_in_the_early_years_of_operation1<-c(levels[,4],levels[,5],levels[,6])
premium1<-200
premium_paid_in_early_yearsA<-numeric(length=3)
for(i in 1:3){
  premium_paid_in_early_yearsA[i]<-
    (prop_in_the_early_years_of_operation1[i]*number_of_customers*(1-
                                                                     discount_levels[i])*premium1)
}
annual_profit_for_early_yearsA<-numeric(length=3)
for(i in 1:3){
  annual_profit_for_early_yearsA[i]<-premium_paid_in_early_yearsA[i]-
    total_claimed_amount1
}

##Early years' profits for country B under the discount scheme with 3 levels
prop_in_the_early_years_of_operation2<-c(levels[,4],levels[,5],levels[,6])
premium2<-300
premium_paid_in_early_yearsB<-numeric(length=3)
for(i in 1:3){
  premium_paid_in_early_yearsB[i]<-
    (prop_in_the_early_years_of_operation2[i]*number_of_customers*(1-
                                                                     discount_levels[i])*premium2)
}
annual_profit_for_early_yearsB<-numeric(length=3)
for(i in 1:3){
  annual_profit_for_early_yearsB[i]<-premium_paid_in_early_yearsB[i]-
    total_claimed_amount2
}

#######Calculation for the expected annual profit from country A once
stability has been reached under the discount scheme with 4 discount levels
lambda<- 0.873 # from 0.01*(87+(3*0.1))
number_of_customers<-10000 #number of customers
n<-30 # number of steps to simulate
P<- matrix(c(0.5823, 0.4177, 0, 0, 0.5823, 0, 0.4177, 0, 0, 0.5823, 0, 0.4177, 0,
             0, 0.5823, 0.4177),
           nrow=4, ncol=4, byrow=TRUE)
#vectors to store proportion of simulations in each state
levels<-matrix(0, nrow=4, ncol=n+1)
for(j in 1:number_of_customers) {
  X<-1 # initial state of the Markov chain
  levels[X,1]=levels[X,1]+1 #record the initial state
  for(i in 1:n) {
    p<-P[X[i],] #to get p values
    X[i+1] <-sample(x=4,size=1, prob=p) #this updates the chain
    levels[X[i+1],i+1]= levels[X[i+1],i+1] + 1
  }
}
levels<-levels/number_of_customers
#calculating the stationary distribution now
finalproportion=levels[,n+1]
finalproportion
#calculating the total premium paid once stability has been reached
premium1<-200
discount_levels<-c(0, 0.1, 0.2, 0.3)
premium_paid1<-numeric(length=4)
for(i in 1:4){
  premium_paid1[i]<-(finalproportion[i]*number_of_customers*(1-
                                                               discount_levels[i])*premium1)
}

total_premium_paid1<-sum(premium_paid1)

#calculating total claims now
number_of_accidents<- rpois(number_of_customers,lambda) # number of
accidents per customer
cost_of_a_claim1 <- runif(number_of_customers,0,400)
mean(cost_of_a_claim1) # expected amount of a claim
total_claimed_amount1<-
  sum(number_of_accidents)*mean(cost_of_a_claim1) #expected total of the
claimed amount
annual_profit_A2=total_premium_paid1 - total_claimed_amount1 # expected
annual profit once stability has been reached for Country A under 2nd discount
scheme

######Calculation for the expected annual profit from country B once stability
has been reached under the discount scheme with 4 discount levels
#calculating total premium paid
premium2<-300
discount_levels<-c(0, 0.1 , 0.2, 0.3)
premium_paid2<-numeric(length=4)
for(i in 1:4){
  premium_paid2[i]<-(finalproportion[i]*number_of_customers*(1-
                                                               discount_levels[i])*premium2)
}

total_premium_paid2<-sum(premium_paid2)
#calculating total claims now
number_of_accidents<- rpois(number_of_customers,lambda) # number of
accidents per customer
cost_of_a_claim2 <- runif(number_of_customers,0,600)

mean(cost_of_a_claim2) # expected amount of a claim
total_claimed_amount2<-
  sum(number_of_accidents)*mean(cost_of_a_claim2) #expected total of the
claimed amount
annual_profit_B2=total_premium_paid2 - total_claimed_amount2 # expected
annual profit once stability has been reached for Country B under 2nd discount
scheme
#plotting the progression of the proportion at each state
plot(levels[1,], type='l', xlim=c(0,n+1), ylim=c(0,1), xlab='Number of years',
     ylab='Proportion', col='orange',lwd=2)
lines(levels[2,], col='green', lwd=2)
lines(levels[3,], col='blue',lwd=2)
lines(levels[4,], col='red', lwd=2)
legend(15,1, c('Discount level 1 (0%)', 'Discount level 2 (10%)', 'Discount level 3
(20%)', 'Discount level 4 (30%)'),
       col=c('orange','green','blue','red'), lwd=c(2,2,2,2))

##Early years' profits for country A under the discount scheme with 4 levels
prop_in_the_early_years_of_operation1<-c(levels[,4],levels[,5],levels[,6])
premium1<-200
premium_paid_in_early_yearsA<-numeric(length=3)
for(i in 1:3){
  premium_paid_in_early_yearsA[i]<-
    (prop_in_the_early_years_of_operation1[i]*number_of_customers*(1-
                                                                     discount_levels[i])*premium1)
}
annual_profit_for_early_yearsA<-numeric(length=3)
for(i in 1:3){
  
  annual_profit_for_early_yearsA[i]<-premium_paid_in_early_yearsA[i]-
    total_claimed_amount1
}

##Early years' profits for country B under the discount scheme with 4 levels
prop_in_the_early_years_of_operation2<-c(levels[,4],levels[,5],levels[,6])
premium2<-300
premium_paid_in_early_yearsB<-numeric(length=3)
for(i in 1:3){
  premium_paid_in_early_yearsB[i]<-
    (prop_in_the_early_years_of_operation2[i]*number_of_customers*(1-
                                                                     discount_levels[i])*premium2)
}
annual_profit_for_early_yearsB<-numeric(length=3)
for(i in 1:3){
  annual_profit_for_early_yearsB[i]<-premium_paid_in_early_yearsB[i]-
    total_claimed_amount2
}