points = read_csv("C:/Users/ksnow2024/Downloads/clusters.csv")

ggplot(points, aes(x = X, y = Y, color = as.factor(Cluster))) + geom_point()
