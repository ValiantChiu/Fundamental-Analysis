library(httr)
library(magrittr)
library(tidyverse)
library(jsonlite)
library(rvest)
library(lubridate)

Time<-Sys.time()
Year <- Time %>% year -1911
Month <- format(Time-3600*24*30, '%m')


#stock list
Sys.setlocale("LC_ALL", "C")
stock_list_url<-"https://mops.twse.com.tw/mops/web/ajax_t51sb01?=1&step=1&firstin=1&TYPEK=sii&code="
target_page <- read_html(stock_list_url)
target <- target_page %>%
    html_nodes(xpath = "/html/body/table[2]") %>%
    html_table()
Sys.setlocale("LC_ALL", "cht")
stock_list <- target[[1]] %>% as_tibble() %>% select(`���q�N��`) #%>% select(`���q�N��`, ���~���O)
stock_list <- stock_list %>% filter(`���q�N��` != '���q�N��') %>% .$`���q�N��`



#Month Revenue

Sys.setlocale("LC_ALL", "C")
GetOneMonthRevenue <- function(Stock, Year, Month) {
    QueryDay <- Sys.time() %>% date %>% as.character
    target_link <- paste0("https://mops.twse.com.tw/mops/web/ajax_t05st10_ifrs?=1&step=1&firstin=1&off=1&keyword4=&code1=&TYPEK2=&checkbtn=&queryName=co_id&inpuType=co_id&TYPEK=all&isnew=false&co_id=", Stock, "&year=", Year, "&month=", Month, "")

    target_page <- read_html(target_link)
    target <- target_page %>%
    html_nodes(xpath = "/html/body/table[4]") %>%
    html_table()
    target <- target[[1]] %>% as_tibble()
    target %>% mutate(QueryDay, Year, Month, Stock)
}



OneMonthRevenue %>% filter(���� != '�W��ʤ���' & ���� != '�W����B') %>% spread(., key = `����`, value = `��~���J�b�B`)
#Sys.setlocale("LC_ALL", "cht")

#stock list -> if year month stock 


stock_list
MonthRevenueAll<-tibble()
for (stock in stock_list) {
    ResultOne<-""
    print(paste(stock))
    Sys.sleep(sample(3:6, size = 1))
    tryCatch(ResultOne <- GetOneMonthRevenue(stock, Year, Month), error = function(e) e)
    MonthRevenueAll <- rbind(MonthRevenueAll, ResultOne)
}