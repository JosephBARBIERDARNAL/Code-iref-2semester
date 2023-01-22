#1
x1 = c(5,4,1,0)
x2 = c(4,5,-2,-3)
data = data.frame(x1,x2)

#2
plot(data$x1, data$x2)

#3
distance = dist(data)
mon_arbre=hclust(distance)
mon_arbre$height

#4
distance = dist(data)
mon_arbre=hclust(distance)
plot(mon_arbre)

#5
distance = dist(data)
mon_arbre=hclust(distance)
cutree(mon_arbre,k = 2)

#6
distance = dist(data)
mon_arbre= hclust(distance^2/2, method="ward.D")
mon_arbre$height

#7
d = dist(data)
mon_arbre=hclust(d^2/2, method="ward.D")
plot(mon_arbre, ylim=c(-10, 100))

#8
d = dist(data)
mon_arbre=hclust(d^2/2, method="ward.D")
mon_arbre$height[3]/sum(mon_arbre$height)
1 - mon_arbre$height[3]/sum(mon_arbre$height)


