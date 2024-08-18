#--------------------------    K Armed Bandits with constant step  -----------------------#
# reward is the cumulative reward
# Q_a1[x] is the calculated expected reward from an action x (average)
# Q_a2[x] is the calculated expected reward from an action x with constant step size alpha
# Er1 , Er2 is the vector containing the expected reward at each iteration for Q_a1, Q_a2
# q_a[x] is the expected reward from an action x (real)
# alpha is the step size parameter
# n_a[x] is the number of times an action is encountered while exploring
q_a  <- rnorm(10,0,1)
Q_a1 <- rep(0,10)
Q_a2 <- rep(0,10)
Qa   <- rep(0,10)
n_a  <- rep(0,10)
reward1 <- 0
reward2 <- 0
Er1 <- 0
Er2 <- 0
alpha = 0.1
for(i in 1:10000){
	if(i%%10 == 1){
		action <- sample(1:10,1)
		r_action <- rnorm(1,q_a[action])
		n_a[action] <- n_a[action] + 1
		Q_a1[action] <- ((n_a[action]-1)*Q_a1[action]+r_action)/n_a[action]
		Q_a2[action] <- (1-alpha)*Q_a2[action]+alpha*r_action
		reward1 <- reward1+r_action
		reward2 <- reward2+r_action
		Er1 <- c(Er1,reward1/i)
		action1 <- which.max(Q_a1)
		action2 <- which.max(Q_a2)
	}
	else{
		reward1 <- reward1 + rnorm(1,q_a[action1])
		Er1 <- c(Er1,reward1/i)
		reward2 <- reward2 + rnorm(1,q_a[action2])
		Er2 <- c(Er2,reward2/i)
	}
	q_a = q_a + rnorm(10,0,0.01)
}
plot(Er2, type = "l", ylab = "E1(r) Vs E2(r)", col = "blue")
points(Er1, type = "l", col = "red")