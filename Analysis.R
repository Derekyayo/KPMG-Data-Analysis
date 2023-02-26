##Import data using the readr library from the dplyr package 

##assign df as data object
df <- KPMG_DATA_Transaction

##view the first few columns in 
head(df)

Address <- KPMG_DATA_CustomerAddress
Demo <- KPMG_DATA_CustomerDemographic

rm(KPMG_DATA_CustomerDemographic)
rm(KPMG_DATA_CustomerAddress)

list_df = list(Demo,Address)
customer <- list_df %>% reduce(left_join, by = 'customer_id')

rm(KPMG_DATA_Transaction)

rm(customer$full_name)
customer$full_name <- paste0(customer$first_name, customer$last_name, sep= "-")
rm(Address,Demo)

customer <- subset(customer, select = -c(default))

customer$full_name <- paste(customer$first_name, customer$last_name)

customer <- subset(customer, select = -c(first_name, last_name))

apply(X = is.na(customer), MARGIN = 2, FUN = sum)

apply(X = is.na(df), MARGIN = 2, FUN = sum)

df <- df2
rm(df2)
apply(X = is.na(df), MARGIN = 2, FUN = sum)

str(df2)

df$transaction_date <- as.Date(df$transaction_date, "%m/%d/%Y")

str(df)

df$standard_cost <- as.numeric(df$standard_cost)

rm(Address)
rm(Demo)

rm(list_df)

head(df2)

class(df2$standard_cost)

df2[c('First', 'Last')] <- str_split_fixed(df2$standard_cost, '$', 2)
view(df2)

df2$standard_cost <- as.numeric(str_extract(df2$standard_cost, "[0-9]+"))

df2$First <- strsplit(df2$First,split = '$')


df<- subset(df, select = -c(standard_cost, product_first_sold_date))

apply(X = is.na(customer), MARGIN = 2, FUN = sum)


summary(df2)
summary(customer)

hist(df2$list_price)
summary(customer)
sum(duplicated(df2$transaction_id))

hist(customer$tenure)
hist(customer$bike_related_purchases)

customer <- customer %>% drop_na(country)

rm(cleaned)
rm(Demo)
rm(Address)

customer$date <- as.Date(customer$DOB, "%Y/%m/%d/")

customer <- customer%>%select(-c(date))

ggplot(df2) +
  aes(x = list_price) +
  geom_histogram(bins = 30L) +
  theme_minimal()

boxplot(df2$list_price,
        ylab = "Price"
)

ggplot(data = df) +
  geom_bar(mapping = aes(x = brand, fill=brand))


render()
#

ggplot(data = df, aes(Date, y= customer_id)) + geom_line()
library(hrbrthemes)

ggplot(data = df, aes(transaction_date, customer_id )) + geom_line()

ggplot(df, aes(x = transaction_date, y = list_price)) +            # Draw ggplot2 plot
  geom_line() 

ggplot(data = df) +
  geom_col(mapping = aes(x = brand, y = list_price, fill=brand))

ggplot(data = c) +
  geom_line(mapping = aes(x = transaction_date, y = list_price)) +
  scale_x_date(date_breaks = "2 month", date_labels = "%W")

ggplot(data = customer) +
  geom_col(mapping = aes(x = owns_car, y = bike_related_purchases, fill=deceased_indicator))

ggplot(data = customer) +
  geom_col(mapping = aes(x = wealth_segment, y = bike_related_purchases, fill=gender))

ggplot(data = customer) +
  geom_bar(mapping = aes(x = state, y = wealth_segment), stat = "count")

ggplot(data = df) +
  geom_line(mapping = aes(x = transaction_date, y = list_price)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b")
