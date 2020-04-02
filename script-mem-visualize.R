### Download libraries
library(data.table)
library(ggplot2)
library(sf)
library(tidyr)
library(dplyr)
library(mapdata)

### Open db-MEM file
dbmem <- read.table("DbMEM-GPS-444ind.txt", header=TRUE)

### Change wide to long
dbmem_gps_long <- gather(dbmem, MEM, Value, MEM1:ncol(dbmem))

### Extract one average value for each GPS point
dbmem_gps <- dbmem_gps_long %>% group_by(Latitude, Longitude, MEM)%>%
  summarise(mem_mean <- mean(Value))
setnames(dbmem_gps, "mem_mean <- mean(Value)", "Average_MEM")
dbmem_wide <- spread(dbmem_gps, MEM, Average_MEM)
#write.table(dbmem_wide, "Mean_MEM_28GPS_fasciatus.txt", quote=FALSE, sep="\t",row.names=FALSE)

# same map, but in ggplot
wH <- map_data("worldHires",  xlim=c(-100,100), ylim=c(20,100)) # subset polygons 

### Test of you can create the map
ggplot() +
  geom_polygon(data = wH, aes(x=long, y = lat, group = group), fill = "gray80", color = NA) +
  coord_fixed(xlim=c(-100,100), ylim=c(20,100), ratio = 1.3) +
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        axis.title = element_blank())

########## Fasciatus
### Select only MEM1 and MEM4
dbmem_gps_long_mem14 <- subset(dbmem_gps, subset=MEM=="MEM1"| MEM=="MEM4")

# Create a ggmap for each MEMs
pdf("MEM1_4_fas.pdf", width=5, height=5)
x_title="Longitude"
y_title="Latitude"
graph1 <- ggplot() +
  geom_polygon(data = wH, aes(x=long, y = lat, group = group), fill = "gray80", color = NA) +
  coord_fixed(xlim = c(-72,-45), ylim=c(35,70), ratio=1.2)+
  facet_wrap(~MEM)+
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        axis.title = element_blank())+
  geom_point(aes(x = Longitude, y = Latitude,fill=Average_MEM), data=dbmem_gps_long_mem14,shape = 21, size=1.5)+
  theme_bw()+theme(legend.position = "none",
                   panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(y=y_title)+  
  labs(x=x_title)+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y=element_text(colour="black"))+
  scale_fill_continuous(high="yellow",low="red")
graph1
dev.off()
