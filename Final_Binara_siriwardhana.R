library(data.table) # for csv file fread
library(ggplot2)
install.packages("gridExtra") # plot graphs for column wise
library(gridExtra)

#Inserting home_assignment_data_pricing.csv file using fread
dataset <- fread("D:/Dalarna/2sem/data Visualization/Assignment/final Assignment/home_assignment_data_pricing.csv")

#I choose Cellphones and Mobile Speakers for future Analysis
dataset <- dataset[dataset$category=="Cellphones" |
                     dataset$category=="Mobile Speakers",]


# create a subsets using 2 category
d_cellphone<- subset(dataset, category == "Cellphones")
d_mobile_spek <-subset(dataset, category == "Mobile Speakers")


#Try remove duplicates in Store_id for find a unique store_id's

d_store_id_cell <- unique(d_cellphone$store_id)
d_store_id_mobspk <- unique(d_mobile_spek$store_id)

#after remove duplicate
#i try to find a stores who sell the both product
comm_sto_id <- d_store_id_cell[d_store_id_cell %in% d_store_id_mobspk]
print(comm_sto_id)

# choose below product id's from database
#cellphone id -3110664
#mobile_speaker id -3506551

#find store id that have enough records of the selected product_id
#below loop
for(store in comm_sto_id){
  if (length(d_cellphone[d_cellphone$store_id == store_id &
                          d_cellphone$product_id == 3110664,]$price)>500)
  {print('store')
   print(store)
   print('number of records')
   print(length(d_cellphone[d_cellphone$store_id == store & 
                              d_cellphone$product_id == 3110664.]$price))
    }
}
#list of store id's i choose below store id's for the common store list
common_store_list <- c(658,3303,130,1260,2085,106,4554,428,28603,525)

#based on the common_store_list i find a store for speaker
#using below loop
for (store in common_store_list) {
  print('store') 
  print(store)
  print(length(d_mobile_spek[d_mobile_spek$store_id == store &
                               d_mobile_spek$product_id==3506551,]$price))
  print("-------")
  }
#i choose this store id's for my future analysis store ids 3303,1260

#i try create separate variables to better visualization 

#cellphone id =3110664 store_id 3303,1260
summary(dataset[product_id ==3110664,]$price)
phone_product <- dataset[dataset$product_id==3110664,]
phone_product_store <-phone_product[phone_product$store_id %in% c(3303,1260),]
phone_product_store_3303 <- phone_product[store_id == 3303,]
phone_product_store_1260 <- phone_product[store_id == 1260,]


#mobile Speaker & store_id 3303,1260
summary(dataset[product_id ==3506551,]$price)
speaker_product <- dataset[dataset$product_id==3506551,]
speaker_product_store <- speaker_product[speaker_product$store_id %in% c(3303,1260),]
speaker_product_store_3303 <- speaker_product[store_id == '3303',]
speaker_product_store_1260 <- speaker_product[store_id == '1260',]


# get the store list except selected store
# in here i need to do the compare selected store and other stores
# for that i do the below method 
dataset2 <- dataset
dataset2$store_id <- as.character(dataset$store_id)
dataset2_exclude <-subset(dataset2,store_id !='3303' & store_id != '1260')

# in here i get the selected stores separately 
dataset2_include_sto_3303 <- dataset2[dataset2$store_id == '3303']
dataset2_include_sto_1260 <- dataset2[dataset2$store_id == '1260']

# plot the divided variable together, then i can see the price observation in each store
ggplot()+ 
  geom_bar(data = dataset2_exclude,aes(store_id),color='lightgray')+
  geom_bar(data = dataset2_include_sto_3303,aes(store_id, fill='3303'),color = 'red')+
  geom_bar(data = dataset2_include_sto_1260,aes(store_id, fill='1260'),color ='blue')+
  scale_y_continuous(labels = scales::comma)+
  scale_fill_manual(values = c("blue", "red"))+
  theme(legend.title = element_blank())+ 
  xlab('store_ids in these categories')+
  ylab('Price Observation')+
  ggtitle('Price observation of selected store')+ 
  theme(panel.background = element_rect(fill = "white"),
                                                        plot.title = element_text(size = 20))

#ggsave('price_observation_3303-1260', width = 20, height = 20, units = "cm")

# -------------------------------------------------------------------------------

install.packages("magrittr")# for grid arrange
library(magrittr)# for grid arrange
library(dplyr)# summaries 

# i try to analyse price for selected product at selected store 
# for that i crate new subset with relevant data
D_price_store <- subset(dataset,store_id %in% c(3303,1260) & product_id %in% c(3506551,3110664)
                        ,select = c(store_id,product_id,price,year,date))

# in this section i try to get a price different in both product in both store 
cell_price_1260 <- subset(D_price_store,product_id =="3110664" & store_id == "1260")
cell_price_3303 <- subset(D_price_store,product_id =="3110664" & store_id == "3303")
combine_Cell_dataset <- merge(cell_price_1260[,c("store_id","product_id","price","date")],
                         cell_price_3303[,c("store_id","product_id","price","date")], by ="date")
combine_Cell_dataset$price_dif <- combine_Cell_dataset$price.x -combine_Cell_dataset$price.y
summary(combine_Cell_dataset$price_dif)

speaker_price_1260 <- subset(D_price_store,product_id =="3506551" & store_id == "1260")
speaker_price_3303 <- subset(D_price_store,product_id =="3506551" & store_id == "3303")
combine_speaker_dataset <- merge(speaker_price_1260[,c("store_id","product_id","price","date")],
                                 speaker_price_3303[,c("store_id","product_id","price","date")], by ="date")
combine_speaker_dataset$price_dif <- combine_speaker_dataset$price.x -combine_speaker_dataset$price.y
summary(combine_speaker_dataset$price_dif)

summary_Cell <- summary(combine_Cell_dataset$price_dif)
summary_speaker <- summary(combine_speaker_dataset$price_dif)

summary_combined <- rbind(summary_Cell, summary_speaker)
data.frame(summary_combined)
#write.csv(summary_combined,file = "summary.csv", row.names = TRUE)
#--------------------------------------------------#

#plot for price analysis
#D_price_store <- D_price_store %>% arrange(store_id)
D_price_store$product_id <- as.factor(D_price_store$product_id)
ggplot(D_price_store, aes(x = date, y = price, colour = product_id)) + 
  geom_line()+
  geom_line(data = combine_Cell_dataset,aes(x=date,y=price_dif,colour ="Cellphone")) + 
  geom_line(data = combine_speaker_dataset,aes(x=date,y=price_dif,colour ="Speaker"))+
  scale_color_manual(values = c("3506551" ="blue","3110664"="red","Cellphone" = "green",
                                "Speaker" ="black"),
                     name= "Title",
                     labels =c("3110664(CellPhone)","3506551(Speaker)","Price_def_Cellphone"
                               ,"Price_def_Speaker"))+
  theme_bw() +
  theme(legend.title = element_blank(), 
        legend.position = "bottom", 
        legend.background = element_rect(fill = "transparent")) +
  ggtitle('Price Comparison of Mobile Speaker & Cellphone between stores 1260 & 3303')+
  facet_wrap(~store_id)


#ggsave('price_Analysis_3303-1260', width = 20, height = 20, units = "cm")

#--------------------------------------------------------------------------------------

# i need to check behavior of real price and CPI price in both product
# for that i try to plot like below 
## Comparing CPI and Real Price for Phone Product
cpi_cell <- ggplot(phone_product_store) +
  geom_path(data = phone_product_store_3303, aes(date, price, colour = "store 3303"),)+
  geom_path(data = phone_product_store_1260, aes(date, price, colour = "store 1260"),)+
  geom_path(data = phone_product_store, aes(date, cpi_adjusted_price, colour = "CPI Adjusted"), alpha = 0.3, size = 1)+
  xlab('Date') +
  ylab('Price(Cellphone)') +
  scale_color_manual(values = c("CPI Adjusted" = "gray", "store 1260" = "red","store 3303" = "blue"), 
                     name = "Legend Title", 
                     labels = c("CPI Adjusted", "Store 1260", "Store 3303")) +
  theme_bw() +
  theme(legend.title = element_blank(), 
        legend.position = "bottom", 
        legend.background = element_rect(fill = "transparent")) +
  ggtitle('Comparing Adjusted CPI and Real Price in Cellphone') +
  labs(colour = "Store ID")


## Comparing CPI and Real Price for Speaker Product

cpi_speaker <- ggplot(speaker_product_store) +
  geom_path(data = speaker_product_store_3303, aes(date, price, colour = "store 3303"),)+
  geom_path(data = speaker_product_store_1260, aes(date, price, colour = "store 1260"),)+
  geom_path(data = speaker_product_store, aes(date, cpi_adjusted_price, 
                                              colour = "CPI Adjusted"), alpha = 0.3, size = 1)+
  xlab('Date') +
  ylab('Price(Speaker)') +
  scale_color_manual(values = c("CPI Adjusted" = "gray",  "store 1260" = "red",
                                "store 3303" = "blue"), 
                     name = "Legend Title", 
                     labels = c("CPI Adjusted", "Store 1260", "Store 3303")) +
  theme_bw() +
  theme(legend.title = element_blank(), 
        legend.position = "bottom", 
        legend.background = element_rect(fill = "transparent")) +
  ggtitle('Comparing Adjusted CPI and Real Price in Speaker')

grid.arrange(cpi_cell,cpi_speaker, ncol = 2)

#ggsave('Compare_real_vs_CPI_3303-1260', width = 20, height = 20, units = "cm")
#-----------------------------------------------------------------------------

## i try to compare two product price of all the store and selected store
# for that i try to get a summaries for all the store price and plot

# cellphone summarization
  Cellphone_comp <- phone_product %>%  group_by(date) %>%
  summarise(min_price = min(price),
            max_price = max(price),
            mean_price = mean(price),
            median_price = median(price),
            sd_price= sd(price),
            price_item=price)

## speaker summarisation

speaker_comp <- speaker_product %>%  group_by(date) %>%
  summarise(min_price_s = min(price),
            max_price_s = max(price),
            mean_price_s = mean(price),
            median_price_s = median(price),
            sd_price_s= sd(price),
            price_item_s=price)

# compare Mean,sd and price of cellphone for all store and selected store  

cellphone <- ggplot() +
  geom_path(data = Cellphone_comp, aes(date, mean_price, colour = "Mean")) +
  geom_path(data = Cellphone_comp, aes(date, sd_price, colour = "sd_price")) +
  geom_path(data = Cellphone_comp, aes(date, price_item, colour = "Price"), alpha = 0.5) +
  geom_path(data = phone_product_store_3303, aes(date, price, colour = "3303")) +
  geom_path(data = phone_product_store_1260, aes(date, price, colour = "1260"))+
  xlab("Date") +
  ylab("Price") +
  scale_color_manual(values = c("1260" = "cyan","3303" = "black","Mean" = "red",
                                "Price" = "pink","sd_price" = "blue" ),
                     name = "",
                     labels = c( "1260","3303","Mean","Price", "sd_price" ))+
theme_bw() +
  theme(legend.title = element_blank(), 
        legend.position = "bottom", 
        legend.background = element_rect(fill = "transparent"))+
  ggtitle('Analysing cellphone SD,Mean and Price for store ')


# compare Mean,sd and price of speaker for all store and selected store  
speaker <- ggplot() +
  geom_path(data = speaker_comp, aes(date, mean_price_s, colour = "Mean")) +
  geom_path(data = speaker_comp, aes(date, sd_price_s, colour = "sd_price")) +
  geom_path(data = speaker_comp, aes(date, price_item_s, colour = "Price"), alpha = 0.5) +
  geom_path(data = speaker_product_store_3303, aes(date, price, colour = "3303")) +
  geom_path(data = speaker_product_store_1260, aes(date, price, colour = "1260"))+
  xlab("Date") +
  ylab("Price") +
  scale_color_manual(values = c("1260" = "cyan","3303" = "black","Mean" = "red",
                                "Price" = "pink","sd_price" = "blue" ),
                     name = "",
                     labels = c( "1260","3303","Mean","Price", "sd_price" ))+
  theme_bw() +
  theme(legend.title = element_blank(), 
        legend.position = "bottom", 
        legend.background = element_rect(fill = "transparent"))+
  ggtitle('Analysing Speaker SD,Mean and Price for store ')

grid.arrange(cellphone,speaker, ncol=2)

#ggsave('price_summary_3303-1260', width = 20, height = 20, units = "cm")

#compare cellphone price with max,min and mean in two store and all other store price

Cell_max_min_price <- ggplot() +
  geom_line(data = Cellphone_comp, aes(date, min_price, colour ="Min Price")) +
  geom_line(data = Cellphone_comp, aes(date, max_price, colour ="Max Price")) +
  geom_line(data = Cellphone_comp, aes(date, mean_price, colour ="Mean Price")) +
  geom_line(data = phone_product_store_3303, aes(date, price, colour ="3303")) +
  geom_line(data = phone_product_store_1260, aes(date, price, colour ="1260"))+
  xlab("Date")+
  ylab("Price(Cellphone)")+
  scale_color_manual(values = c("1260" = "red","3303" = "green","Max Price"="cyan",
                                "Mean Price" = "blue","Min Price" = "purple"),
                     labels = c("1260","3303","Max Price","Mean Price", "Min Price"))+
  theme_bw() +
  theme(legend.title = element_blank(), 
        legend.position = "bottom", 
        legend.background = element_rect(fill = "transparent"))+
  ggtitle('comparision of Cellphone price between two store')

#compare Speaker price with max,min and mean in two store and all other store price

speaker_max_min_price <-ggplot() +
  geom_line(data = speaker_comp, aes(date, min_price_s, colour ="Min Price")) +
  geom_line(data = speaker_comp, aes(date, max_price_s, colour ="Max Price")) +
  geom_line(data = speaker_comp, aes(date, mean_price_s, colour ="Mean Price")) +
  geom_line(data = speaker_product_store_3303, aes(date, price, colour ="3303")) +
  geom_line(data = speaker_product_store_1260, aes(date, price, colour ="1260"))+
  xlab("Date")+
  ylab("Price(speaker)")+
  scale_color_manual(values = c("1260" = "red","3303" = "green","Max Price"="cyan",
                                "Mean Price" = "blue","Min Price" = "purple"),
                     labels = c("1260","3303","Max Price","Mean Price", "Min Price"))+
  theme_bw() +
  theme(legend.title = element_blank(), 
        legend.position = "bottom", 
        legend.background = element_rect(fill = "transparent"))+
  ggtitle('comparision of Speaker price between two store')


grid.arrange(Cell_max_min_price,speaker_max_min_price, ncol=2)
#ggsave('price_Summary_3303-1260_2', width = 20, height = 20, units = "cm")

#-----------------------------------------------------------------------------------
#--------------------------------END------------------------------------------------
#-----------------------------------------------------------------------------------



