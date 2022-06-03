install.packages("hexSticker")
library(hexSticker)


Fig_hex<-ggplot()+
  geom_jitter(data=res_long_trans%>%
                filter(Year%in%2000 & Age<=89 & lambda>0 & Sex%in% "Women"),
              aes(Age,(res),color=i, group=country), size=1, alpha=0.1, color="grey80")+

  geom_line(data=res_long_pioneer%>%
              filter(country%in%c("Sweden") & Year%in%2000 & Age<=89 & lambda>0 & Sex%in% "Women"),
            aes(Age,(res),color=i), size=1)+
  scale_color_manual(name="Revivorship times",values=c("#67001F","#D6604D", "#F4A582"))+
  facet_grid(.~Year_lab)+
  theme_pander(base_size = 18)+
  theme(legend.position = "none",
        strip.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
  labs(y=expression(paste("Number of Survivors Revived ", ~ (lx[i]))))+
  scale_x_continuous(breaks=c(0,15,30,45,60,75,90))+
  scale_y_continuous(breaks=c(0,5000,10000,15000,20000,25000,30000))+
  theme_transparent()


library(showtext)
## Loading Google fonts (http://www.google.com/fonts)
font_add_google("Gochi Hand", "gochi")
## Automatically use showtext to render text for future devices
showtext_auto()

sticker(Fig_hex, package="Revivorship", p_size=10, s_x=0.98, s_y=.8, s_width=1.3, s_height=1,
        filename="inst/figures/ggplot2.png", p_family = "gochi",  h_fill="#f9690e", h_color="#67001F")



install.packages("workflowr")
