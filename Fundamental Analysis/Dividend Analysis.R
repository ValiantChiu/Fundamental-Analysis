library(httr)
library(magrittr)
library(tidyverse)
library(jsonlite)
library(rvest)
#library(RODBC)
library(lubridate)
#predict the dividend of this year before the year report out
#LOCAL <- odbcConnect("LOCAL")

GetFinReport <- function(Url, Year, Season) {
    ResultAll <- tibble(r1 = list(), r2 = list(), r3 = list(), r4 = list(), r5 = list(), r6 = list(), year = integer(), season = integer())
    year <- Year
    season <- Season
    url <- Url
    for (y in Year) {
        for (s in Season) {
            postData <- paste0("encodeURIComponent=1&step=1&firstin=1&off=1&isQuery=Y&TYPEK=sii&year=", y, "&season=", s, "")
            result <- POST(url, body = postData)
            html <- result %>% content(as = "text") %>% read_html()
            result <- html %>% html_table()

            ResultOne <- tibble(r1 = result[2], r2 = result[3], r3 = result[4], r4 = result[5], r5 = result[6], r6 = result[7])
            ResultOne <- ResultOne %>% mutate(year = y, season = s)
            ResultAll <- rbind(ResultAll, ResultOne)

        }
        print(y)
        Sys.sleep(sample(2:5, size = 1));
    }
    return(ResultAll)
}
SelectColumn <- function(Df, Type, Category) {

    if (Type == "BS") {
        if (Category == 1) { Df <- Df %>% select("公司代號", "現金及約當現金", "應收款項－淨額", "資產總額", "負債總額", "股本", "權益總額", "每股參考淨值") }
        if (Category == 2) { Df <- Df %>% select("公司代號", "流動資產", "非流動資產", "資產合計", "流動負債", "負債合計", "股本", "權益合計", "每股參考淨值") }
        if (Category == 3) { Df <- Df %>% select("公司代號", "流動資產", "非流動資產", "資產總額", "流動負債", "負債總額", "股本", "權益總額", "每股參考淨值") }
        if (Category == 4) { Df <- Df %>% select("公司代號", "現金及約當現金", "應收款項－淨額", "資產總額", "負債總額", "股本", "權益總額", "每股參考淨值") }
        if (Category == 5) { Df <- Df %>% select("公司代號", "現金及約當現金", "應收款項", "資產總額", "負債總額", "股本", "權益總額", "每股參考淨值") }
        if (Category == 6) { Df <- Df %>% select("公司代號", "流動資產", "非流動資產", "資產總額", "流動負債", "負債總額", "股本", "權益總額", "每股參考淨值") }
        } else {
            if (Category == 1) { Df <- Df %>% select("公司代號", "利息淨收益", "利息以外淨損益", "營業費用", "繼續營業單位本期稅後淨利（淨損）", "其他綜合損益（稅後）", "本期綜合損益總額（稅後）", "基本每股盈餘（元）") }
            if (Category == 2) { Df <- Df %>% select("公司代號", "收益", "支出及費用", "營業外損益", "本期其他綜合損益（稅後淨額）", "本期綜合損益總額", "基本每股盈餘（元）") }
            if (Category == 3) { Df <- Df %>% select("公司代號", "營業收入", "營業成本", "營業費用", "營業外收入及支出", "繼續營業單位本期淨利（淨損）", "其他綜合損益（淨額）", "本期綜合損益總額", "基本每股盈餘（元）") }
            if (Category == 4) { Df <- Df %>% select("公司代號", "利息淨收益", "利息以外淨收益", "營業費用", "本期稅後淨利（淨損）", "本期其他綜合損益（稅後淨額）", "本期綜合損益總額", "基本每股盈餘（元）") }
            if (Category == 5) { Df <- Df %>% select("公司代號", "營業收入", "營業成本", "營業費用", "營業外收入及支出", "本期淨利（淨損）", "其他綜合損益（稅後淨額）", "本期綜合損益總額", "基本每股盈餘（元）") }
            if (Category == 6) { Df <- Df %>% select("公司代號", "收入", "支出", "本期淨利（淨損）", "其他綜合損益", "本期綜合損益總額", "基本每股盈餘（元）") }

        }


    return(Df)
}
SelectCommonColumn <- function(Df, Name) {
    Df %>% select(Name)
}
AddTime <- function(Df, Year, Season) {
    Df %>% mutate(year = Year, season = Season)
}
GetCommonColumn <- function(Df) {
    common <- Df[[1]] %>% names
    if (length(Df) > 1) {
        for (i in 2:(length(Df))) {
            if (is.null(Df[[i]])) next;
            common <- common %>% intersect(names(Df[[i]]))
        }
    }
    common
}

Rbind <- function(Df) {
    Result <- Df[[1]]
    if (length(Df) > 1) {
        for (i in 2:length(Df)) {
            Result <- rbind(Result, Df[[i]])
        }
    }
    Result
}
AggregateReport <- function(Df) {
    Df <- Df %>% GetCommonName
    Df <- Df %>% mutate(r1 = pmap(list(r1, r1name), SelectCommonColumn))
    Df <- Df %>% mutate(r2 = pmap(list(r2, r2name), SelectCommonColumn))
    Df <- Df %>% mutate(r3 = pmap(list(r3, r3name), SelectCommonColumn))
    Df <- Df %>% mutate(r4 = pmap(list(r4, r4name), SelectCommonColumn))
    Df <- Df %>% mutate(r5 = pmap(list(r5, r5name), SelectCommonColumn))
    Df <- Df %>% mutate(r6 = pmap(list(r6, r6name), SelectCommonColumn))

    Df <- Df %>% mutate(r1 = pmap(list(r1, year, season), AddTime))
    Df <- Df %>% mutate(r2 = pmap(list(r2, year, season), AddTime))
    Df <- Df %>% mutate(r3 = pmap(list(r3, year, season), AddTime))
    Df <- Df %>% mutate(r4 = pmap(list(r4, year, season), AddTime))
    Df <- Df %>% mutate(r5 = pmap(list(r5, year, season), AddTime))
    Df <- Df %>% mutate(r6 = pmap(list(r6, year, season), AddTime))
    Df <- Df %>% .[, 1:6]
    r1 <- Df$r1 %>% Rbind
    names(r1) <- names(r1) %>% gsub(pattern = "（|）", replacement = "")
    r2 <- Df$r2 %>% Rbind
    names(r2) <- names(r2) %>% gsub(pattern = "（|）", replacement = "")
    r3 <- Df$r3 %>% Rbind
    names(r3) <- names(r3) %>% gsub(pattern = "（|）", replacement = "")
    r4 <- Df$r4 %>% Rbind
    names(r4) <- names(r4) %>% gsub(pattern = "（|）", replacement = "")
    r5 <- Df$r5 %>% Rbind
    names(r5) <- names(r5) %>% gsub(pattern = "（|）", replacement = "")
    r6 <- Df$r6 %>% Rbind
    names(r6) <- names(r6) %>% gsub(pattern = "（|）", replacement = "")
    list(r1, r2, r3, r4, r5, r6)
}
SaveReport <- function(Df, Type) {
    if (Type == 'IS') {
        ISName <- c("IS1", "IS2", "IS3", "IS4", "IS5", "IS6")
        for (i in 1:length(Df)) {
            sqlSave(LOCAL, Df[[i]], tablename = ISName[i], rownames = FALSE, append = TRUE)
        }
    } else {
        BSName <- c("BS1", "BS2", "BS3", "BS4", "BS5", "BS6")
        for (i in 1:length(Df)) {
            sqlSave(LOCAL, Df[[i]], tablename = BSName[i], rownames = FALSE, append = TRUE)
        }
    }
}

GetCommonName <- function(Df) {
    ColumnName <- Df %>% map(~GetCommonColumn(.))
    namecolumn <- c("r1name", "r2name", "r3name", "r4name", "r5name", "r6name")
    for (i in 1:6) {
        Df[, namecolumn[i]] <- tibble(list(ColumnName[[i]]))
    }
    Df
}


GetNumericReport <- function(Report) {

    for (i in 1:nrow(Report)) {
        Report[i,] <- as.numeric(gsub(pattern = ",", replacement = "", Report[i,]))
    }
    return(Report)
}

CleanData <- function(df) {
    df %>% map(~as.numeric(gsub(pattern = ",", replacement = "", .))) %>% as.tibble
}

#Income Statement
GetIS <- function(Year,Season) {
    Sys.setlocale("LC_ALL", "C")
    ISUrl <- "https://mops.twse.com.tw/mops/web/ajax_t163sb04"
    ISALL <- GetFinReport(ISUrl, Year, Season)
    IS <- ISALL %>% AggregateReport
    Sys.setlocale("LC_ALL", "cht")
    IS
}

#IS<-GetIS('107','03')
Sys.setlocale("LC_ALL", "cht")


#saveRDS(IS_last_year_3, file = "IS_last_year_3.rds")


IS_last_year_3 <- GetIS('107', '03')
IS_last_year_4 <- GetIS('107', '04')
IS_this_year_3 <- GetIS('108', '03')

IS_last_year_3_1 <- IS_last_year_3[[1]] %>% as.tibble %>% select("公司代號", "基本每股盈餘（元）", "year", "season") %>% CleanData %>% rename(Profit_last_year_3 = `基本每股盈餘（元）`)
IS_last_year_3_2 <- IS_last_year_3[[2]] %>% as.tibble %>% select("公司代號", "基本每股盈餘（元）", "year", "season") %>% CleanData %>% rename(Profit_last_year_3 = `基本每股盈餘（元）`)
IS_last_year_3_3 <- IS_last_year_3[[3]] %>% as.tibble %>% select("公司代號", "基本每股盈餘（元）", "year", "season") %>% CleanData %>% rename(Profit_last_year_3 = `基本每股盈餘（元）`)
IS_last_year_3_4 <- IS_last_year_3[[4]] %>% as.tibble %>% select("公司代號", "基本每股盈餘（元）", "year", "season") %>% CleanData %>% rename(Profit_last_year_3 = `基本每股盈餘（元）`)
IS_last_year_3_5 <- IS_last_year_3[[5]] %>% as.tibble %>% select("公司代號", "基本每股盈餘（元）", "year", "season") %>% CleanData %>% rename(Profit_last_year_3 = `基本每股盈餘（元）`)
IS_last_year_3_6 <- IS_last_year_3[[6]] %>% as.tibble %>% select("公司代號", "基本每股盈餘（元）", "year", "season") %>% CleanData %>% rename(Profit_last_year_3 = `基本每股盈餘（元）`)
IS_last_year_3 <- rbind(IS_last_year_3_1, IS_last_year_3_2, IS_last_year_3_3, IS_last_year_3_4, IS_last_year_3_5, IS_last_year_3_6) %>% select(公司代號, Profit_last_year_3)

IS_last_year_4_1 <- IS_last_year_4[[1]] %>% as.tibble %>% select("公司代號", "基本每股盈餘（元）", "year", "season") %>% CleanData %>% rename(Profit_last_year_4 = `基本每股盈餘（元）`)
IS_last_year_4_2 <- IS_last_year_4[[2]] %>% as.tibble %>% select("公司代號", "基本每股盈餘（元）", "year", "season") %>% CleanData %>% rename(Profit_last_year_4 = `基本每股盈餘（元）`)
IS_last_year_4_3 <- IS_last_year_4[[3]] %>% as.tibble %>% select("公司代號", "基本每股盈餘（元）", "year", "season") %>% CleanData %>% rename(Profit_last_year_4 = `基本每股盈餘（元）`)
IS_last_year_4_4 <- IS_last_year_4[[4]] %>% as.tibble %>% select("公司代號", "基本每股盈餘（元）", "year", "season") %>% CleanData %>% rename(Profit_last_year_4 = `基本每股盈餘（元）`)
IS_last_year_4_5 <- IS_last_year_4[[5]] %>% as.tibble %>% select("公司代號", "基本每股盈餘（元）", "year", "season") %>% CleanData %>% rename(Profit_last_year_4 = `基本每股盈餘（元）`)
IS_last_year_4_6 <- IS_last_year_4[[6]] %>% as.tibble %>% select("公司代號", "基本每股盈餘（元）", "year", "season") %>% CleanData %>% rename(Profit_last_year_4 = `基本每股盈餘（元）`)
IS_last_year_4 <- rbind(IS_last_year_4_1, IS_last_year_4_2, IS_last_year_4_3, IS_last_year_4_4, IS_last_year_4_5, IS_last_year_4_6) %>% select(公司代號, Profit_last_year_4)



IS_this_year_3_1 <- IS_this_year_3[[1]] %>% as.tibble %>% select("公司代號", "基本每股盈餘（元）", "year", "season") %>% CleanData %>% rename(Profit_this_year_3 = `基本每股盈餘（元）`)
IS_this_year_3_2 <- IS_this_year_3[[2]] %>% as.tibble %>% select("公司代號", "基本每股盈餘（元）", "year", "season") %>% CleanData %>% rename(Profit_this_year_3 = `基本每股盈餘（元）`)
IS_this_year_3_3 <- IS_this_year_3[[3]] %>% as.tibble %>% select("公司代號", "基本每股盈餘（元）", "year", "season") %>% CleanData %>% rename(Profit_this_year_3 = `基本每股盈餘（元）`)
IS_this_year_3_4 <- IS_this_year_3[[4]] %>% as.tibble %>% select("公司代號", "基本每股盈餘（元）", "year", "season") %>% CleanData %>% rename(Profit_this_year_3 = `基本每股盈餘（元）`)
IS_this_year_3_5 <- IS_this_year_3[[5]] %>% as.tibble %>% select("公司代號", "基本每股盈餘（元）", "year", "season") %>% CleanData %>% rename(Profit_this_year_3 = `基本每股盈餘（元）`)
IS_this_year_3_6 <- IS_this_year_3[[6]] %>% as.tibble %>% select("公司代號", "基本每股盈餘（元）", "year", "season") %>% CleanData %>% rename(Profit_this_year_3 = `基本每股盈餘（元）`)
IS_this_year_3 <- rbind(IS_this_year_3_1, IS_this_year_3_2, IS_this_year_3_3, IS_this_year_3_4, IS_this_year_3_5, IS_this_year_3_6) %>% select(公司代號, Profit_this_year_3)

ISReport <- IS_this_year_3 %>% left_join(IS_last_year_4) %>% left_join(IS_last_year_3)

ISReport %>% View

#Dividend

Dividend_last_year <- read.csv(file = "Dividend Data/Dividend107.csv", header = TRUE, row.names = NULL, encoding = "UTF-8", sep = ",", dec = ".", quote = "\"", comment.char = "") %>% as.tibble
names(Dividend_last_year) <- c("公司代號名稱", "資料來源", "期別", "董事會決議通過股利分派日", "股東會日期", "期初未分配盈餘or待彌補虧損元", "本期淨利淨損元", "可分配盈餘元", "分配後期末未分配盈餘元", "股東配發s盈餘分配之現金股利元股", "股東配發s法定盈餘公積and資本公積發放之現金元股", "股東配發s股東配發之現金股利總金額元", "股東配發s盈餘轉增資配股元股", "股東配發s法定盈餘公積and資本公積轉增資配股元股", "股東配發s股東配股總股數股", "普通股每股面額")
Dividend_last_year <- Dividend_last_year %>% select(公司代號名稱, 股東配發s盈餘分配之現金股利元股)
GetCompany <- function(string) {
    strsplit(as.character(string), split = "-") %>% .[[1]] %>% .[1] %>% trimws
}

Dividend_last_year <- Dividend_last_year %>% mutate(公司代號 = as.character(map(公司代號名稱, GetCompany))) %>% select(-公司代號名稱)
Dividend_last_year <- Dividend_last_year %>% mutate(股東配發s盈餘分配之現金股利元股 = 股東配發s盈餘分配之現金股利元股 %>% as.character %>% as.numeric)
Dividend_last_year <- Dividend_last_year %>% filter(!is.na(股東配發s盈餘分配之現金股利元股))

#Dividend Predict

DividendPredict<-ISReport %>% mutate(公司代號 = as.character(公司代號)) %>% left_join(Dividend_last_year)
DividendPredict <- DividendPredict %>% mutate(Profit_this_year_4_P = Profit_this_year_3 * Profit_last_year_4 / Profit_last_year_3, DivideRatio = 股東配發s盈餘分配之現金股利元股 / Profit_last_year_4)
DividendPredict <- DividendPredict %>% mutate(DividendPredict = Profit_this_year_4_P * DivideRatio)
saveRDS(DividendPredict,file = "Dividend Data/DividendPredict_2019.rds")
#DividendPredict<-readRDS("Dividend Data/DividendPredict.rds")

#Price
GetPrice <- function(Year, Month, Stock) {
    print(Stock)
    #Purl <- paste0("https://www.tpex.org.tw/web/stock/aftertrading/daily_trading_info/st43_result.php?l=zh-tw&d=", Year, "/", Month, "/01&stkno=", Stock, "")
    Purl <- paste0("https://www.twse.com.tw/exchangeReport/STOCK_DAY?response=json&date=", Year, "", Month, "01&stockNo=", Stock, "")
    Price <- POST(Purl)
    Price <- Price %>% content(as = "text") %>% fromJSON
    PriceData <- Price$data %>% as.tibble
    names(PriceData) <- Price$fields
    Sys.sleep(sample(4:8, size = 1))
    PriceData
}
DealWithDate <- function(Price) {
    Price %>% mutate(BuyDate = str_split(日期, "/")) %>%
        mutate(year = as.integer(map(BuyDate, ~ .[1])) + 1911,
           month = as.character(map(BuyDate, ~ .[2])),
           day = as.character(map(BuyDate, ~ .[3]))) %>%
           mutate(Date = ymd(paste0(year, month, day))) %>%
           select(-year, - month, - day, - 日期, - BuyDate)
}
DividendPredictPrice <- DividendPredict %>% mutate(Price = pmap(list('2020', '01', 公司代號), GetPrice))

#Price <- readRDS(file = "Price201902.rds")
#saveRDS(DividendPredictPrice, file = "Dividend Data/DividendPredictPrice_2019.rds")
DividendPredictPrice <- readRDS("Dividend Data/DividendPredictPrice.rds")
Price<-DividendPredictPrice %>% select(公司代號, Price) %>% unnest(Price) %>% group_by(公司代號) %>% filter(日期 == max(日期)) %>% select(公司代號, 收盤價)



#All Report

InterestRateReport <- DividendPredict %>% left_join(Price) %>% mutate(InterestRatePercent = DividendPredict / as.numeric(收盤價))

InterestRateReport %>% View
InterestRateReport %<>% mutate(InterestRateOld = 股東配發s盈餘分配之現金股利元股 / 收盤價) %>% arrange(desc(InterestRatePercent))

saveRDS(InterestRateReport, file = "Dividend Data/InterestRateReport_2019.rds")
write.csv(InterestRateReport, file = "Dividend Data/InterestRateReport.csv")
readRDS("Dividend Data/InterestRateReport.rds")
readRDS(file = "InterestRateReport.rds")