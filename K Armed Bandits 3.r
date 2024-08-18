#--------------------------    K Armed Bandits - optimistic Vs realistic   -----------------------#
# reward is the cumulative reward
# Q_a1[x] is the calculated expected reward from an action x (realistic)
# Q_a2[x] is the calculated expected reward from an action x (optimistic)
# Er1 , Er2 is the vector containing the expected reward at each iteration for Q_a1, Q_a2
# q_a[x] is the expected reward from an action x (real)
# alpha is the step size parameter
# n_a[x] is the number of times an action is encountered while exploring
q_a  <- rnorm(10,0,1)
Q_a1 <- rep(0,10)
Q_a2 <- rep(10,10)
Qa   <- rep(0,10)
reward1 <- 0
reward2 <- 0
Er1 <- 0
Er2 <- 0
alpha = 0.1
for(i in 1:10000){
	if(i%%10 == 1){
		action <- sample(1:10,1)
		r_action <- rnorm(1,q_a[action])
		Q_a1[action] <- (1-alpha)*Q_a1[action]+alpha*r_action
		reward1 <- reward1+r_action
		Er1 <- c(Er1,reward1/i)
		action1 <- which.max(Q_a1)
	}
	else{
		reward <- rnorm(1,q_a[action1])
		reward1 <- reward1 + reward
		Er1 <- c(Er1,reward1/i)
		Q_a1[action1] <- (1-alpha)*Q_a1[action1]+alpha*reward
	}
	action2 <- which.max(Q_a2)
	reward <- rnorm(1,q_a[action2])
	reward2 <- reward2 + reward
	Er2 <- c(Er2,reward2/i)
	Q_a2[action2] <- (1-alpha)*Q_a2[action2]+alpha*reward

}
plot(Er2, type = "l", ylab = "E1(r) Vs E2(r)", col = "blue")
points(Er1, type = "l", col = "red")