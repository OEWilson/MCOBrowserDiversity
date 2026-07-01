library(ggplot2)
library(ggpubr)
library(deeptime)
library(FSA)

europe <- read.csv("Europe brachydonts.csv")
europe.bins <- read.csv("Europe max bins.csv")
balkanotolia <- read.csv("Balkanotolia brachydonts.csv")
balkanotolia.bins <- read.csv("Balkanotolia max bins.csv")
centralasia <- read.csv("Central Asia brachydonts.csv")
centralasia.bins <- read.csv("Central Asia max bins.csv")
india <- read.csv("Indian Subcontinent brachydonts.csv")
india.bins <- read.csv("Indian subcontinent max bins.csv")
eastafrica <- read.csv("Eastern Africa brachydonts.csv")
eastafrica.bins <- read.csv("Eastern Africa max bins.csv")
northam <- read.csv("North America brachydonts.csv")
northam.bins <- read.csv("North America max bins.csv")
southam <- read.csv("South America brachydonts.csv")
southam.bins <- read.csv("South America max bins.csv")

#Plotting the maximum number of brachydonts through time
europe.plot <- ggplot() + scale_x_reverse("Age (Ma)",limits=c(0,40)) + scale_y_continuous(name="Number of species",limits=c(0,20)) +
  geom_pointrange(data = europe,aes(x=as.numeric(Age.midpoint.2026),xmin=Max.age, xmax=Min.age,y=Brachydonts),colour="black") +
  geom_smooth(data=europe.bins,aes(x=Age,y=Brachydonts),colour="black",linewidth=1,se=FALSE,span=0.1) + 
  geom_smooth(data=europe.bins,aes(x=Age,y=Hypsodonts),colour="darkred",linewidth=1,se=FALSE,span=0.1) + 
  theme_bw() + coord_geo(pos = list("bottom"), dat = list("epochs"),abbrv = FALSE, expand=TRUE) + theme(text=element_text(size=20))
europe.plot
balkanotolia.plot <- ggplot() + scale_x_reverse("Age (Ma)",limits=c(0,40)) + scale_y_continuous(name="Number of species",limits=c(0,20)) + 
  geom_pointrange(data = balkanotolia,aes(x=as.numeric(Age.midpoint.2026),xmin=Max.age, xmax=Min.age,y=Brachydonts),colour="black") + 
  geom_smooth(data=balkanotolia.bins,aes(x=Age,y=Brachydonts),colour="black",linewidth=1,se=FALSE,span=0.2) +
  geom_smooth(data=balkanotolia.bins,aes(x=Age,y=Hypsodonts),colour="darkred",linewidth=1,se=FALSE,span=0.2) + 
  theme_bw() + coord_geo(pos = list("bottom"), dat = list("epochs"),abbrv = FALSE, expand=TRUE) + theme(text=element_text(size=20))
balkanotolia.plot
centralasia.plot <- ggplot() + scale_x_reverse("Age (Ma)",limits=c(0,40)) + scale_y_continuous(name="Number of species",limits=c(0,20)) + 
  geom_pointrange(data = centralasia,aes(x=as.numeric(Age.midpoint.2026),xmin=Max.age, xmax=Min.age,y=Brachydonts),colour="black") +
  geom_smooth(data=centralasia.bins,aes(x=Age,y=Brachydonts),colour="black",linewidth=1,se=FALSE,span=0.1) +
  geom_smooth(data=centralasia.bins,aes(x=Age,y=Hypsodonts),colour="darkred",linewidth=1,se=FALSE,span=0.1) + 
  theme_bw() + coord_geo(pos = list("bottom"), dat = list("epochs"),abbrv = FALSE, expand=TRUE) + theme(text=element_text(size=20))
centralasia.plot
eastafrica.plot <- ggplot() + scale_x_reverse("Age (Ma)",limits=c(0,40)) + scale_y_continuous(name="Number of species",limits=c(0,20)) + 
  geom_pointrange(data = eastafrica,aes(x=as.numeric(Age.midpoint.2026),xmin=Max.age, xmax=Min.age,y=Brachydonts),colour="black") +
  geom_smooth(data=eastafrica.bins,aes(x=Age,y=Brachydonts),colour="black",linewidth=1,se=FALSE,span=0.1) +
  geom_smooth(data=eastafrica.bins,aes(x=Age,y=Hypsodonts),colour="darkred",linewidth=1,se=FALSE,span=0.1) + 
  theme_bw() + coord_geo(pos = list("bottom"), dat = list("epochs"),abbrv = FALSE, expand=TRUE) + theme(text=element_text(size=20))
eastafrica.plot
india.plot <- ggplot() + scale_x_reverse("Age (Ma)",limits=c(0,40)) + scale_y_continuous(name="Number of species",limits=c(0,20)) + 
  geom_pointrange(data = india,aes(x=as.numeric(Age.midpoint.2026),xmin=Max.age, xmax=Min.age,y=Brachydonts),colour="black") +
  geom_smooth(data=india.bins,aes(x=Age,y=Brachydonts),colour="black",linewidth=1,se=FALSE,span=0.1) +
  geom_smooth(data=india.bins,aes(x=Age,y=Hypsodonts),colour="darkred",linewidth=1,se=FALSE,span=0.1) + 
  theme_bw() + coord_geo(pos = list("bottom"), dat = list("epochs"),abbrv = FALSE, expand=TRUE) + theme(text=element_text(size=20))
india.plot
northam.plot <- ggplot() + scale_x_reverse("Age (Ma)",limits=c(0,40)) + scale_y_continuous(name="Number of species",limits=c(0,20)) + 
  geom_pointrange(data = northam,aes(x=as.numeric(midpoint),xmin=Max.age, xmax=Min.age,y=Brachydonts),colour="black") +
  geom_smooth(data=usa.bins,aes(x=Age,y=Brachydonts),colour="black",linewidth=1,se=FALSE,span=0.1) +
  geom_smooth(data=usa.bins,aes(x=Age,y=Hypsodonts),colour="darkred",linewidth=1,se=FALSE,span=0.1) + 
  theme_bw() + coord_geo(pos = list("bottom"), dat = list("epochs"),abbrv = FALSE, expand=TRUE) + theme(text=element_text(size=20))
northam.plot

#Plotting the simultaneity of smoothed curves
all.region.curves <- rbind(europe.bins,balkanotolia.bins,centralasia.bins,india.bins,eastafrica.bins,northam.bins)
simultaneity.plot <- ggplot() + scale_x_reverse("Age (Ma)") + coord_geo(pos = list("bottom"), dat = list("epochs"),abbrv = FALSE, expand=TRUE) + 
  geom_smooth(data=all.region.curves,aes(x=Age,y=Brachydonts,colour=Region),linewidth=1,se=FALSE) + 
  scale_colour_manual(values=c("#a20a00","#10a6c0","#118036","#f8be00","#21215f","#e87400")) +
  labs(y="Number of brachydonts") + theme_bw() +
  theme(text=element_text(size=20), legend.position="bottom")
simultaneity.plot

#Plotting number of non-hypsodonts in South America
southam.plot <- ggplot() + geom_pointrange(data = southam,aes(x=Age,xmin=Age.max, xmax=Age.min,y=NonHYP,colour=Type),size=1) + scale_colour_manual(values=c("#E1BE6A","#40B0A6")) + 
  scale_x_reverse("Age (Ma)") + coord_geo(pos = list("bottom"), dat = list("epochs"),abbrv = FALSE, expand=TRUE) + labs(y="Non-hypsodont species") + theme_bw() +
  theme(text=element_text(size=20), legend.position="bottom")
southam.plot <- southam.plot + 
  geom_smooth(data=subset(SouthAm.bins,SouthAm.bins$Type == "Extratropical"),aes(x=Age,y=Brachydonts),colour="#E1BE6A",linewidth=1,se=FALSE,span=0.2) + 
  geom_smooth(data=subset(SouthAm.bins,SouthAm.bins$Type == "Tropical"),aes(x=Age,y=Brachydonts),colour="#40B0A6",linewidth=1,se=FALSE,span=0.2) +
  geom_smooth(data=subset(SouthAm.bins,SouthAm.bins$Type == "Overall"),aes(x=Age,y=Brachydonts),colour="black",linewidth=1,se=FALSE,span=0.2)
southam.plot

#Riversleigh
riversleigh <- read.csv("Riversleigh herbivores.csv")
riversleigh.plot <- ggplot() + scale_x_reverse("Age (Ma)") + geom_pointrange(data = riversleigh,aes(x=Age,xmin=Max.Age, xmax=Min.Age,y=Arboreal),colour="#669f37",size=1) + 
  geom_pointrange(data = riversleigh,aes(x=Age,xmin=Max.Age, xmax=Min.Age,y=TerrBrowsers),colour="#7c5761",size=1) + 
  geom_pointrange(data = riversleigh,aes(x=Age,xmin=Max.Age, xmax=Min.Age,y=Browsers),colour="black",size=1) + 
  theme_bw() + coord_geo(pos = list("bottom"), dat = list("epochs"),abbrv = FALSE, expand=TRUE) + theme(text=element_text(size=20)) + 
  labs(y="Number of browser species")
riversleigh.plot

year.comps <- read.csv("Riversleigh year comparisons.csv")
order <- c("D Site", "White Hunter", "Camel Sputum","Creaser's Rampart","Dirk's Towers", "Neville's Garden","RSO Site","Upper Site","Wayne's Wok","AL90","Cleft of Ages","Gag Site","Henk's Hollow","Jim's Carousel","Keith's Chocky Block","Last Minute","Encore")
group.order <- c("Arboreal browsers","Terrestrial browsers","Hypsodonts and hypselodonts")
year.comps$Fauna <- factor(year.comps$Fauna, levels=order)
year.comps$Group <- factor(year.comps$Group, levels=group.order)
year.comps$Year <- as.factor(year.comps$Year)
riversleigh.comps <- ggplot() + geom_point(data=year.comps,aes(x=Fauna,y=Species,shape=Year,colour=Group,fill=Group),size=5) +
  scale_shape_manual(values=c(1,21)) + scale_colour_manual(values=c("#56c4b3","#ed4f39","#fcd80c")) + scale_fill_manual(values=c("#56c4b3","#ed4f39","#fcd80c")) + labs(y="Number of species") + 
  theme_bw() + theme(text=element_text(size=20),axis.text.x = element_text(angle=45,hjust=1),legend.position = c(0.,0.8))
riversleigh.comps

