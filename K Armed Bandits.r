#--------------------------    K Armed Bandits   -----------------------#
# reward is the cumulative reward
# Er is the vector containing the expected reward at each iteration
# Q_a[x] is the expected reward from an action x (calculated)
# q_a[x] is the expected reward from an action x (real)
# Qa[x] is the cumulative reward from an action x
# n_a[x] is the number of times an action is encountered while exploring
q_a <- rnorm(10,0,1)
Q_a <- rep(0,10)
Qa  <- rep(0,10)
n_a <- rep(0,10)
reward <- 0
Er <- 0
for(i in 1:2000){
	if(i%%10 == 1){
		action <- sample(1:10,1)
		r_action <- rnorm(1,q_a[action])
		Q_a[action] = (Qa[action]+r_action)/n_a[action]
		Qa[action] <- Qa[action] + r_action
		n_a[action] <- n_a[action] + 1
		reward <- reward+r_action
		Er <- c(Er,reward/i)
		action <- which.max(Q_a)
	}
	else{
		reward <- reward + rnorm(1,q_a[action])
		Er <- c(Er,reward/i)
	}
}
plot(Er, type = "l", ylab = "E(r)")