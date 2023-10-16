> n <- 10000
> n_acc <- rpois(10000, lambda = 0.879)
> sum(n_acc)
> cost_claim_a <- runif (8791, 0, 400)
> sum(cost_claim_a)
> P <- matrix(c(0.4152, 0.5848, 0, 0.4152,0, 0.5848, 0, 0.4152, 0.5848) , 
              + nrow=3 , ncol = 3 , byrow = TRUE)
> levels<-matrix(0, nrow=3, ncol=n+1)
> for(j in 1:trials){
  + X<-1 # initial state of the Markov Chain
  + levels[X, 1] = levels[X, 1] + 1 #Record the initial state
  + for( i in 1:n){
    + p<-P[X[i],] # Get the p values
    + X[i+1]<-sample(x=3, size=1, prob=p) # update the chain
    + levels[X[i+1], i+1] = levels[X[i+1], i+1] + 1
    + }
  + }
> levels<-levels/trials
> levels
> cost_claim_b <- runif (8791, 0, 600)
> sum(cost_claim_b)


