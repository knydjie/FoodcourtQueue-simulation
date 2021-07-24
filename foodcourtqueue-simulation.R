#---- arus pelanggan di foodcourt saat jam makan siang----
rm(list=ls())

library(simmer)
set.seed(1)
env <- simmer("Foodcourt")
env

order <- function() round(rexp(1, 0.2),3)
eat <- function() round(rexp(1, 0.05),3)
cashier <- function() round(rexp(1, 1),3)
arrival <- function() round(rexp(1,3),3)

pelanggan <- trajectory("antrian pelanggan") %>%
  log_("datang...") %>%
  seize("restoran") %>%
  timeout(order) %>%
  release("restoran") %>%
  
  log_("makan...") %>%
  seize("meja") %>%
  timeout(eat) %>%
  release("meja") %>%
  
  log_("ke kasir...") %>%
  seize("kasir") %>%
  timeout(cashier) %>%
  release("kasir") %>%
  
  log_("pulang...")

env %>%
  add_resource("restoran", 7) %>%
  add_resource("meja", 15) %>%
  add_resource("kasir", 2) %>%
  add_generator("pelanggan", pelanggan, arrival)

env %>%
  run(120) %>%
  now()

(output_data1 <- get_mon_arrivals(env))
(output_data2 <- get_mon_resources(env))
(output_data3 <- get_mon_attributes(env))
summary(output_data1)
summary(output_data2)

rata <- mean(output_data2$queue)

#dengan ringkasan statistik
rm(list=ls())

library(simmer)
library(simmer.plot)
library(ggplot2)
set.seed(123)
env <- simmer("Foodcourt")
env

order <- function() round(rexp(1, 0.5),3) #melayani 1 pelanggan dalam 2 menit 
eat <- function() round(rexp(1, 0.05),3) #melayani 1 pelanggan dalam 20 menit
cashier <- function() round(rexp(1, 1),3) #melayani 1 pelanggan dalam 1 menit
arrival <- function() round(rexp(1,3),3) #datang 3 pelanggan dalam 1 menit

pelanggan <- trajectory("antrian pelanggan") %>%
  # log_("datang...") %>%
  seize("restoran") %>%
  timeout(order) %>%
  release("restoran") %>%
  
  #log_("makan...") %>%
  seize("meja") %>%
  timeout(eat) %>%
  release("meja") %>%
  
  #log_("ke kasir...") %>%
  seize("kasir") %>%
  timeout(cashier) %>%
  release("kasir") #%>%

#log_("pulang...")

env2 <- lapply(1:100, function(i){
  simmer("Foodcourt") %>%
    add_resource("restoran", 7) %>% #ada 7 restoran
    add_resource("meja", 15) %>% #ada 15 meja
    add_resource("kasir", 2) %>% #ada 2 kasir
    add_generator("pelanggan", pelanggan, arrival) %>%
    run(120) %>% invisible #jam makan siang selama 2 jam
})

#melihat proses kedatangan pelanggan
output1 <- get_mon_arrivals(env2)
sub1 <- subset(output1, replication==1) 
sub2 <- subset(output1, replication==2)

#melihat proses pemakaian server
output2 <- get_mon_resources(env2)
sub3 <- subset(output2, replication==1)
sub4 <- subset(output2, replication==2)

#output1
k <- NULL; rata <- NULL
for(j in 1:100){
  dt <- subset(output1, replication == j)
  k <- c(k, nrow(dt))
  lsist <- dt$end_time - dt$start_time
  rata <- c(rata, mean(lsist))
}
k %>% head()
mean(k) #rata2 jmlh pelanggan yang selesai dilayani dlm 120 menit
hist(k)

rata %>% head() 
mean(rata) #rata2 lama org di dalam sistem
hist(rata)

#output2
rantri <- NULL
for(j in 1:100){
  dt <- subset(output2, replication == j)
  rantri <- c(rantri, mean(dt$queue))
}
rantri %>% head()
mean(rantri) #rata2 jml antrian
hist(rantri)

#plot
plot(output2, metric = "utilization")
plot(output2, metric = "usage", c("restoran", "meja", "kasir"), items = "server")
plot(output2, metric = "usage", c("restoran", "meja", "kasir"), items = "queue")
plot(output1, metric = "activity_time")
plot(output1, metric = "waiting_time")

#selang kepercayaan
SE1 <- sd(k)
xbar1 <- mean(k)
Z.97.5 <- qnorm(0.975) 
(SK1 <- xbar1+c(-1,1)*Z.97.5 * SE1)

SE2 <- sd(rata)
xbar2 <- mean(rata) 
Z.97.5 <- qnorm(0.975)
(SK2 <- xbar2+c(-1,1)*Z.97.5 * SE2)

SE3 <- sd(rantri)
xbar3 <- mean(rantri)
Z.97.5 <- qnorm(0.975)
(SK3 <- xbar3+c(-1,1)*Z.97.5 * SE3)

#SK1 <- quantile(k, probs=c(0.025, 0.975))
#SK1

#SK2 <- quantile(rata, probs=c(0.025, 0.975))
#SK2

#SK3 <- quantile(rantri, probs=c(0.025, 0.975))
#SK3

#ringkasan statistik
hasil <- env2 %>%
  get_mon_arrivals() %>%
  transform(waiting_time = end_time - start_time - activity_time)
paste("Rata-rata total", sum(hasil$finished), "orang pelanggan dalam menyelesaikan aktivitas di Foodcourt adalah",
      round(mean(hasil$waiting_time),3), "menit")