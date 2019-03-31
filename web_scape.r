library(rvest)

web_page <- read_html('https://medium.com/s/cointalk/coin-talk-59-flashbacks-and-fake-beards-a-crypto-2018-year-in-review-77ee1256aeb7')

main_page <- read_html('https://medium.com/s/cointalk/')

main_page %>% html_nodes("a") 

#p name="ff1b"


#<- web_page %>% html_nodes("p") %>% html_text()


main_page %>% html_nodes("a") %>% html_attr('href')

web_page %>% html_nodes("a") %>% html_attr('href')


#[29] "https://medium.com/tag/coin-talk/archive/2018/04"                                                                                                                                      
#[30] "https://medium.com/tag/coin-talk/archive/2018/07"

#https://medium.com/tag/coin-talk/archive/2018/01

ct_01 <- read_html('https://medium.com/tag/coin-talk/archive/2018/01')
ct_02 <- read_html('https://medium.com/tag/coin-talk/archive/2018/02')
ct_03 <- read_html('https://medium.com/tag/coin-talk/archive/2018/03')
ct_04 <- read_html('https://medium.com/tag/coin-talk/archive/2018/04')
ct_05 <- read_html('https://medium.com/tag/coin-talk/archive/2018/05')
ct_06 <- read_html('https://medium.com/tag/coin-talk/archive/2018/06')
ct_07 <- read_html('https://medium.com/tag/coin-talk/archive/2018/07')
ct_08 <- read_html('https://medium.com/tag/coin-talk/archive/2018/08')
ct_09 <- read_html('https://medium.com/tag/coin-talk/archive/2018/09')
ct_10 <- read_html('https://medium.com/tag/coin-talk/archive/2018/10')
ct_11 <- read_html('https://medium.com/tag/coin-talk/archive/2018/11')
ct_12 <- read_html('https://medium.com/tag/coin-talk/archive/2018/12')
ct_13 <- read_html('https://medium.com/tag/coin-talk/archive/2019/01')
ct_14 <- read_html('https://medium.com/tag/coin-talk/archive/2019/02')
ct_15 <- read_html('https://medium.com/tag/coin-talk/archive/2019/03')



hand_list<- c("https://medium.com/s/cointalk/coin-talk-2-korean-crypto-mania-and-the-kimchi-spread-8ab23de3e1d9","https://medium.com/s/cointalk/coin-talk-3-we-survived-the-crash-w-doug-kim-f3e2c7aebbc2","https://medium.com/s/cointalk/coin-talk-4-do-you-guys-even-believe-in-crypto-own-your-own-casino-5156fa9ff431","https://medium.com/s/cointalk/coin-talk-5-tether-usdt-is-the-new-disneybucks-ef94876ff3b7","https://medium.com/s/cointalk/coin-talk-6-how-low-can-you-go-an-interview-with-jameson-lopp-ab5adf8d3a97",
              "https://medium.com/s/cointalk/7-giancarlo-2020-senate-hearings-on-bitcoin-augurs-first-prediction-marketabout-324e2dd2229c",
              "https://medium.com/s/cointalk/coin-talk-8-interview-with-nocoiner-tech-journalist-adrian-chen-baf00bf05134",
              "https://medium.com/s/cointalk/coin-talk-8-interview-with-nocoiner-tech-journalist-adrian-chen-baf00bf05134",
              "https://medium.com/s/cointalk/9-privacy-coins-explained-a-deep-dive-into-monero-xmr-and-zcash-zec-5017b84a7ba3",
              "https://medium.com/s/cointalk/coin-talk-10-telegrams-ton-token-civil-brings-the-blockchain-to-journalism-w-maria-bustillos-913435b278c1",
              "https://medium.com/s/cointalk/coin-talk-12-what-is-an-ico-c94fa9c4226",
              "https://medium.com/s/cointalk/coin-talk-13-did-mt-goxs-japanese-trustee-personally-crash-bitcoin-62f62b65aefb",
              "https://medium.com/s/cointalk/coin-talk-15-whats-so-great-about-the-blockchain-e89c5c5f69b1","https://medium.com/s/cointalk/coin-talk-11-when-dictators-ico-a02bc72da6e1",
              "https://medium.com/s/cointalk/coin-talk-14-whats-so-great-about-the-blockchain-dedc38e9413b",
              "https://medium.com/s/cointalk/coin-talk-16-the-rise-and-fall-of-kang-lammer-americas-worst-crypto-traders-8f32bcb08273",
              "https://medium.com/s/cointalk/coin-talk-18-the-fundamentals-remain-strong-9a8226e2b802",
              "https://medium.com/s/cointalk/coin-talk-19-alt-spring-on-the-campus-quad-1deada76e091",
              "https://medium.com/s/cointalk/coin-talk-20-the-return-of-fomo-kang-w-doug-kim-d3a22a4a3d94","https://medium.com/s/cointalk/coin-talk-21-will-zrx-be-the-next-coin-added-to-coinbase-cedf82567a7c",
              "https://medium.com/s/cointalk/coin-talk-22-%EF%B8%8F-the-bitcoin-ransoming-of-aaronlammer-com-be3072fd2668",
              "https://medium.com/s/cointalk/coin-talk-23-are-ripple-xrp-fans-bots-59c9aa0e192b",
              "https://medium.com/s/cointalk/24-book-club-the-bitcoin-standard-part-1-a0b9ef7981eb",
              "https://medium.com/s/cointalk/coin-talk-25-fear-and-loathing-at-a-cryptokitty-auction-33abbcd65b57",
              "https://medium.com/s/cointalk/coin-talk-26-penguin-jetpacks-w-nathaniel-popper-from-the-new-york-times-44652b88526f",
              "https://medium.com/s/cointalk/coin-talk-27-bitcointopia-where-the-murder-rate-is-100-and-an-interview-with-a-real-xrp-fan-7761ef08d9f5",
              "https://medium.com/s/cointalk/coin-talk-28-book-club-the-bitcoin-standard-part-2-fc0c096583e9",
              "https://medium.com/s/cointalk/episode-29-%EF%B8%8F-r-i-p-sumokoin-eaf69e061178",
              "https://medium.com/s/cointalk/coin-talk-30-interview-with-a-miner-chris-dannen-of-iterative-capital-e36c66b4392b",
              "https://medium.com/s/cointalk/coin-talk-31-kidnapped-by-the-triads-and-other-shitcoin-adventures-abbbd3c53e10",
              "https://medium.com/s/cointalk/coin-talk-32-interview-with-saifedean-ammous-economist-and-author-of-the-bitcoin-standard-39652a6612e5",
              "https://medium.com/s/cointalk/33-how-tezos-went-awry-ed25fb7eab37",
              "https://medium.com/s/cointalk/coin-talk-34-the-real-slim-satoshi-2254252ee6ff",
              "https://medium.com/s/cointalk/coin-talk-35-coinbase-goes-shopping-82d529907499",
              "https://medium.com/s/cointalk/coin-talk-36-how-to-make-dumb-bets-on-augur-w-ledgerstatus-d38d7db33d8f",
              "https://medium.com/s/cointalk/coin-talk-37-decentralized-assassinations-and-pee-tape-markets-c2d062080c2f","https://medium.com/s/cointalk/coin-talk-38-no-market-no-bounce-ft-doug-kim-fc4ce3a86c16",
              "https://medium.com/s/cointalk/coin-talk-39-we-called-a-craigslist-crypto-consultant-58458081de0f","https://medium.com/s/cointalk/coin-talk-40-mailbag-vol-2-4f5cda373118","https://medium.com/s/cointalk/coin-talk-41-where-were-you-when-bcash-forked-c7f6bcd55a89",
              "https://medium.com/s/cointalk/coin-talk-42-excerptoshi-truthers-unite-4eda76b43b0b",
              "https://medium.com/s/cointalk/coin-talk-43-impressions-of-crypto-china-with-chris-dannen-e5e9f87f24e3",
              "https://medium.com/s/cointalk/coin-talk-45-the-psychology-of-mass-movements-with-tony-sheng-7b0376514372",
              "https://medium.com/s/cointalk/46-are-we-ready-to-callabottom-ad9de8faf634",
              "https://medium.com/s/cointalk/coin-talk-47-bitcoin-bugz-weed-stock-mania-1a37507c2313",
              "https://medium.com/s/cointalk/coin-talk-48-senate-hearing-jam-session-pt-1-with-zack-voell-cd1e2e7d62b5",
              "https://medium.com/s/cointalk/coin-talk-49-senate-hearing-jam-session-pt-2-w-brian-patrick-eha-de53e4ebe2b4",
              "https://medium.com/s/cointalk/coin-talk-50-if-vitalik-is-a-9-10-im-a-2-10-an-interview-with-the-new-yorker-s-nick-239364e3ebd5",
              "https://medium.com/s/cointalk/episode-51-but-what-do-you-really-think-about-civil-cvl-2b25c0e1f40e",
              "https://medium.com/s/cointalk/episode-52-hopium-is-a-helluva-drug-w-doug-kim-e0890a77c65b",
              "https://medium.com/s/cointalk/episode-54-are-we-crypto-influencers-4128e2cc7385",
              "https://medium.com/s/cointalk/coin-talk-54-brave-bat-b42293422bbf",
              "https://medium.com/s/cointalk/coin-talk-55-the-bull-case-for-initiative-q-9d7b34c1bda5",
              "https://medium.com/s/cointalk/coin-talk-56-bcash-2-electric-boogaloo-satoshis-vision-bb0389604f7f",
              "https://medium.com/s/cointalk/coin-talk-57-crypto-vs-journalism-cd4242706b33",
              "https://medium.com/s/cointalk/coin-talk-58-super-airdrop-festival-c33752c9c363",
              "https://medium.com/s/cointalk/coin-talk-59-flashbacks-and-fake-beards-a-crypto-2018-year-in-review-77ee1256aeb7",
              "https://medium.com/s/cointalk/coin-talk-60-live-on-the-10th-anniversary-of-the-mining-of-the-genesis-block-a687b7c2959f",
              "https://medium.com/s/cointalk/coin-talk-61-%EF%B8%8Fgrin-the-mimblewimble-protocol-75fe92ed41cf",
              "https://medium.com/s/cointalk/coin-talk-62-fyre-fest-was-the-original-airdrop-f56862e9118d",
              "https://medium.com/s/cointalk/coin-talk-63-ska-selling-out-the-bitcoin-etf-fd7f3a789842",
              "https://medium.com/s/cointalk/cointalk-64-aarons-farm-i-don-t-want-to-mine-in-game-rewards-at-93d47d22b0f9",
              "https://medium.com/s/cointalk/cointalk-65-slanted-and-enchanted-72a36c1d82b",
              "https://medium.com/s/cointalk/cointalk-%EF%B8%8F-66-hxro-is-a-crypto-game-that-is-definitely-not-gambling-47595a6c84d5",
              "https://medium.com/s/cointalk/episode-67-would-oprah-move-the-needle-5b20d9a7910d",
              "https://medium.com/s/cointalk/cointalk-%EF%B8%8F-68-%EF%B8%8F-hacking-team-wants-to-connect-on-linked-in-90220f283a57")


tct02 <- ct_02 %>%
  html_nodes("a") %>% 
  html_attr('href')  %>% as.tibble()

tct03 <- tct02 %>% 
  filter(str_detect(value,'coin-talk-') == TRUE)  
  
links <-tct03$value
  

  
first <- function(x){
  z = str_split(x, '\\?source') %>% unlist(.)
  y= unlist(z[[1]])
  print(y[1])
  return(y[1])}

link1<- map(links,first)


link_list <- function(html){
    tct02 <- html %>%
      html_nodes("a") %>% 
      html_attr('href')  %>% as.tibble()
    tct03 <- tct02 %>% 
      filter(str_detect(value,'coin-talk-') == TRUE)  
    links <-tct03$value
    link1<- map(links,first)
    link2 <- unlist(link1) 
}

ctl1 <- enframe(unique(link_list(ct_01)))

ctl1 <- rbind(ctl1,enframe(unique(link_list(ct_02))))
ctl1 <- rbind(ctl1,enframe(unique(link_list(ct_03))))
ctl1 <- rbind(ctl1,enframe(unique(link_list(ct_04))))
ctl1 <- rbind(ctl1,enframe(unique(link_list(ct_05))))
ctl1 <- rbind(ctl1,enframe(unique(link_list(ct_06))))
ctl1 <- rbind(ctl1,enframe(unique(link_list(ct_07))))
ctl1 <- rbind(ctl1,enframe(unique(link_list(ct_08))))
ctl1 <- rbind(ctl1,enframe(unique(link_list(ct_09))))
ctl1 <- rbind(ctl1,enframe(unique(link_list(ct_10))))
ctl1 <- rbind(ctl1,enframe(unique(link_list(ct_11))))
ctl1 <- rbind(ctl1,enframe(unique(link_list(ct_12))))
ctl1 <- rbind(ctl1,enframe(unique(link_list(ct_13))))
ctl1 <- rbind(ctl1,enframe(unique(link_list(ct_14))))
ctl1 <- rbind(ctl1,enframe(unique(link_list(ct_15))))

first <- function(x){
  z = str_split(x, ":") %>% unlist(.)
  y= unlist(z[[1]])
  print(y[1])
  return(y[1])}


second <- function(x){
  z = ifelse(str_detect(x,'Jay Kang:'),str_replace(x,'Jay Kang:', ""),x )
  y = ifelse(str_detect(z,'Aaron Lammer:'),str_replace(z,'Aaron Lammer:', ""),z )
  print(y)
  #y= unlist(z[[2]])
  #print(y)
  return(y)
}


testtesttest <- read_html(hand_list[[1]])


testtibble<- testtesttest %>% 
  html_nodes('p') %>% 
  html_text() %>% 
  as_tibble() %>% 
  filter(str_detect(value,"Jay Kang:")==TRUE |str_detect(value,"Aaron Lammer:")==TRUE) 
  
episode <- testtesttest %>% html_nodes('h1') %>% html_text()

text <- testtibble$value

new_text <- unlist(text)

test1<- map(new_text,first)
test2<- map(new_text,second)

conversation_df <- tibble(name = unlist(test1),text = test2)

conversation_df <-  conversation_df %>% mutate(linenumber=row_number(),episode=episode)

tidy_conversation <- conversation_df %>% 
  unnest_tokens(word, text) 

tidy_conversation <- tidy_conversation %>% 
  anti_join(stop_words)


tidy_conversation_main <- tidy_conversation

linkrange<- c(14:length(ctlinkunique))

for(number in linkrange){
  
testtesttest <- read_html(hand_list[[1]])

print(number)

testtibble<- testtesttest %>% 
  html_nodes('p') %>% 
  html_text() %>% 
  as_tibble() %>% 
  filter(str_detect(value,"Jay Kang:")==TRUE |str_detect(value,"Aaron Lammer:")) 

episode <- testtesttest %>% html_nodes('h1') %>% html_text()

text <- testtibble$value

new_text <- unlist(text)

test1<- map(new_text,first)
test2<- map(new_text,second)

conversation_df <- tibble(name = unlist(test1),text = test2)

conversation_df <-  conversation_df %>% mutate(linenumber=row_number(),episode=episode)

tidy_conversation <- conversation_df %>% 
  unnest_tokens(word, text) 

tidy_conversation <- tidy_conversation %>% 
  anti_join(stop_words)

tidy_conversation_main <- rbind(tidy_conversation_main,tidy_conversation)

}


hand_listnum <- c(2:67)

hand_list <- hand_list[c(-17,-18)]


for(number in hand_listnum){
  testtesttest <- read_html(hand_list[[number]])

    testtibble<- testtesttest %>% 
    html_nodes('p') %>% 
    html_text() %>% 
    as_tibble() %>% 
    filter(str_detect(value,"Jay Kang:")==TRUE |str_detect(value,"Aaron Lammer:")) 
  
  episode <- testtesttest %>% html_nodes('h1') %>% html_text()
  
  text <- testtibble$value
  
  new_text <- unlist(text)
  
  test1<- map(new_text,first)
  test2<- map(new_text,second)
  
  conversation_df <- tibble(name = unlist(test1),text = test2)
  
  conversation_df <-  conversation_df %>% mutate(linenumber=row_number(),episode=episode)
  
  tidy_conversation <- conversation_df %>% 
    unnest_tokens(word, text) 
  
  tidy_conversation <- tidy_conversation %>% 
    anti_join(stop_words)
  
  tidy_conversation_main <- rbind(tidy_conversation_main,tidy_conversation)
  
}

hand_list <- hand_list[20:60]
hand_listnum <- c(1:40)

for(number in hand_listnum){
  testtesttest <- read_html(hand_list[[number]])
  
  testtibble<- testtesttest %>% 
    html_nodes('p') %>% 
    html_text() %>% 
    as_tibble() %>% 
    filter(str_detect(value,"Jay Kang:")==TRUE |str_detect(value,"Aaron Lammer:")) 
  
  episode <- testtesttest %>% html_nodes('h1') %>% html_text()
  
  text <- testtibble$value
  
  new_text <- unlist(text)
  
  test1<- map(new_text,first)
  test2<- map(new_text,second)
  
  conversation_df <- tibble(name = unlist(test1),text = test2)
  
  conversation_df <-  conversation_df %>% mutate(linenumber=row_number(),episode=episode)
  
  tidy_conversation <- conversation_df %>% 
    unnest_tokens(word, text) 
  
  tidy_conversation <- tidy_conversation %>% 
    anti_join(stop_words)
  
  tidy_conversation_main <- rbind(tidy_conversation_main,tidy_conversation)
  
}


bing <- get_sentiments('bing')

tidy_conversation_main <- tidy_conversation_main %>%
  # With inner join, implement sentiment analysis using `bing`
  inner_join(bing)

tidy_conversation_main <- tidy_conversation_main %>% filter(word != 'grin')

tidy_conversation_main <- tidy_conversation_main %>% mutate(score = ifelse(sentiment == 'negative',0,1))
       


summary_of_episode <- left_join(tidy_conversation_main %>%  group_by(name,episode) %>%summarize(total=sum(score)),tidy_conversation_main %>% group_by(name,episode) %>% tally()) %>% mutate(sent_score = total/n)

summary_of_episode <- summary_of_episode %>% mutate(episode_number = ifelse(is.na(str_extract(episode,'[:digit:]{2}')),str_extract(episode,'[:digit:]'),str_extract(episode,'[:digit:]{2}')))


summary_of_episode$episode_number <- as.numeric(summary_of_episode$episode_number)

ggplot(summary_of_episode,aes(episode_number,sent_score,color=name)) +geom_point()




str_extract('Coin Talk #57: 🏓 Crypto vs. Journalism ','[:digit:]{2}')

ggplot(mpg, aes(displ, hwy, colour = class)) + 
  geom_point()
