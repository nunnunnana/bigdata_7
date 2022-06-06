# 감정사전 활용 하기
# 감정 분석iment analysis)
# 텍스트에 어떤 감정이 담겨있는지 분석 방법

# 감정 사전 불러오기
library(dplyr)
library(readr)
dic <- read_csv("knu_sentiment_lexicon.csv")

# 분석을 위한 라이브러리 불러오기 
library(stringr)

# 감정 사전 활용하기 ########################################################
# 긍정단어
dic %>%
  filter(polarity == 2) %>%
  arrange(word)

# 부정단어
dic %>%
  filter(polarity == -2) %>%
  arrange(word)

# 총 단어
dic %>%
  mutate(sentiment = ifelse(polarity >= 1, "pos",
                            ifelse(polarity <= -1, "neg", "neu"))) %>%
  count(sentiment)

# ㄱ  
df <- tibble(sentence = c("국비과정은 기간이 길다 그래서 힘들지만 실습 ㅅ 스스로 배우는 습관과 행동을 기를 수 있어서 좋다.",
                          "NCS 교재는 너무 구식이라 ㅅ 뒤 떨어져 있어서 불만이다. 내용도 부족하다. 하지만, 강사님께서 직접 구해오신 자료들이 그걸 만회 ㅎ 다행이다."))
df

# 텍스트를 단어 기준으로 토큰화: 감정 사전과 동 unnest_tokens(drop = F)
# 원문 제거하지 않기
# 단어가 어느 문장에서 추출됐는지 알수 있도록

library(tidytext)
df <- df %>%
  unnest_tokens(input = sentence,
                output = word,
                token = "words",
                drop = F)
df

# 단어에 감정 점수 부여하기
# dplyr::left_join() : word 기준 감정 사전 결합
# 없는 단어 polarity NA -> 0 부여
df <- df %>%
  left_join(dic, by = "word") %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity))
df

# 3. 문장별로 감정 점수 하
score_df <- df %>%
  group_by(sentence) %>%
  summarise(score = sum(polarity))
score_df

# 댓글 감정 분석 #######################################################

# 영화 '기생충' 관련 기사 댓글
# 기본적인 전처리

# 데이터 불러오기
raw_news_comment <- read_csv("news_comment_parasite.csv")


# 기본적인 저
install.packages("textclean")
library(textclean)
news_comment <- raw_news_comment %>%
  mutate(id = row_number(),
         reply = str_squish(replace_html(reply)))


# 데이터 구조 확인
glimpse(news_comment)

# 토큰화
word_comment <- news_comment %>%
  unnest_tokens(input = reply,
                output = word,
                token = "words",
                drop = F)
word_comment %>%
  select(word, reply)

# 감정 점수 부여
word_comment <- word_comment %>%
  left_join(dic, by = "word") %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity))
word_comment %>%
  select(word, polarity)


##자주 사용된 감정 단어 살펴###############################

# 1. 감정 분류하기
word_comment <- word_comment %>%
  mutate(sentiment = ifelse(polarity == 2, "pos",
                            ifelse(polarity == -2, "neg", "neu")))
word_comment %>%
  count(sentiment)

# 2. 막대 그래프 만들기
top10_sentiment <- word_comment %>%
  filter(sentiment != "neu") %>%
  count(sentiment, word) %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10)
top10_sentiment

# 막대 그래프 만들기
library(ggplot2)
ggplot(top10_sentiment, aes(x = reorder(word, n),
                            y = n,
                            fill = sentiment)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) +
  facet_wrap(~ sentiment, scales = "free") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(x = NULL) +
  theme(text = element_text(family = "NanumBarunGothicBold"))


# 댓글별 긍정 점수 구하고 댓글 살펴보기ᅵ#############################################3

# 1. 댓글별 감정 점수 구하기

score_comment <- word_comment %>% group_by(id, reply) %>% summarise(score = sum(polarity)) %>% ungroup()
score_comment %>% select(score, reply)

# 2. 감정 점수 높은 댓글 살펴보기
# 긍정 댓글
score_comment %>% select(score, reply) %>% arrange(-score)

# 부정 댓글
score_comment %>% select(score, reply) %>% arrange(score)

# 1. 감정 점수 빈도 구하기
score_comment %>% count(score)

# 2. 감정 분류하고 막대 그래프 만들기
# 감정 분류하기
score_comment <- score_comment %>%
  mutate(sentiment = ifelse(score >= 1, "pos",
                            ifelse(score <= -1, "neg", "neu")))
# 감정 빈도와 비율 구하기
frequency_score <- score_comment %>%
  count(sentiment) %>%
  mutate(ratio = n/sum(n)*100)
frequency_score

# 막대 그래프 만들기
ggplot(frequency_score, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.3) +
  scale_x_discrete(limits = c("pos", "neu", "neg"))

# 3. 비율 누계 그래프 만들기
df <- tibble(contry = c("Korea", "Korea", "Japen", "Japen"), # 축
             sex = c("M", "F", "M", "F"), # 누적막대
             ratio = c(60, 40, 30, 70)) # 값

ggplot(df, aes(x = contry, y = ratio, fill = sex)) +
  geom_col() +
  geom_text(aes(label = paste0(ratio, "%")), # % 표시
            position = position_stack(vjust = 0.5)) # 가운데 표시

# 댓글의 감정 비율로 누적 막대 그래프 만들기 #############################
# 더미 변수 생성
frequency_score$dummy <- 0
frequency_score

ggplot(frequency_score, aes(x = dummy, y = ratio, fill = sentiment)) +
  geom_col() +
  geom_text(aes(label = paste0(round(ratio, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  theme(axis.title.x = element_blank(), # x축 이름 삭제
        axis.text.x = element_blank(), # x축 값 삭제
        axis.ticks.x = element_blank()) # x축 눈금 삭제


# 감정 범주별 주요 단어 #############################################
# 1. 토큰화하고 두 글자 이상 한글 단어만 남기기

comment <- score_comment %>%
  unnest_tokens(input = reply, # 단어 기준 토근화
                output = word,
                token = "words",
                drop = F) %>%
  filter(str_detect(word, "[가-힣]") & # 한글 추출
           str_count(word) >= 2) # 두 자 이상 추출

# 감정 및 단어별 빈도 구하기
frequency_word <- comment %>%
  filter(str_count(word) >= 2) %>%
  count(sentiment, word, sort = T)
frequency_word

# 긍정 고빈도 단어
frequency_word %>%
  filter(sentiment == "pos")

# 부정 고빈도 단어
frequency_word %>%
  filter(sentiment == "neg")

# 1. 고르 오즈비 구
# wide form으로 변환
library(tidyr)
comment_wide <- frequency_word %>%
  filter(sentiment != "neu") %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = list(n = 0))
comment_wide

# ᄅ  구하기
comment_wide <- comment_wide %>%
  mutate(log_odds_ratio = log(((pos + 1) / (sum(pos + 1))) /
                                ((neg + 1) / (sum(neg + 1)))))
comment_wide

# 2. 로그 오즈비가 가장 큰 단어 10개씩 추출하기
top10 <- comment_wide %>%
  group_by(sentiment = ifelse(log_odds_ratio > 0, "pos", "neg")) %>%
  slice_max(abs(log_odds_ratio), n = 10, with_ties = F)
top10

# 3. 막대 그래프 만들기
# 막대 그래프 만들기
ggplot(top10, aes(x = reorder(word, log_odds_ratio),
                  y = log_odds_ratio,
                  fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL) +
  theme(text = element_text(family = "NanumBarunGothicBold"))



# 감정 사전 수정하기 ######################################################

# 감정 단어가 사용 살펴보기
# "소름", "미친" : 긍정적인 감정을 극적으로 표현한 단어
# "소름"이 사용된 댓글
score_comment %>%
  filter(str_detect(reply, "소름")) %>%
  select(reply)

# "미친"이 사용된 댓글
score_comment %>%
  filter(str_detect(reply, "미친")) %>%
  select(reply)

dic %>% filter(word %in% c("소름", "소름이", "미친"))

new_dic <- dic %>%
  mutate(polarity = ifelse(word %in% c("소름", "소름이", "미친"), 2, polarity))
new_dic %>% filter(word %in% c("소름", "소름이", "미친"))

# 수정한 사전으로 감정 점수 부여하기
new_word_comment <- word_comment %>%
  select(-polarity) %>%
  left_join(new_dic, by = "word") %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity))


# 댓글별 감정 점수 구하기
new_score_comment <- new_word_comment %>%
  group_by(id, reply) %>%
  summarise(score = sum(polarity)) %>%
  ungroup()
new_score_comment %>%
  select(score, reply) %>%
  arrange(-score)


# 1점 기준으로 긍정 중립 부정 분류
new_score_comment <- new_score_comment %>%
  mutate(sentiment = ifelse(score >= 1, "pos",
                            ifelse(score <= -1, "neg", "neu")))

# 원본 감정 사전 활용
score_comment %>%
  count(sentiment) %>%
  mutate(ratio = n/sum(n)*100)


# 수저 감정 사전 활용
new_score_comment %>%
  count(sentiment) %>%
  mutate(ratio = n/sum(n)*100)

word <- "소름|소름이|미친"

# 원본 감정 사전
score_comment %>%
  filter(str_detect(reply, word)) %>%
  count(sentiment)


# 수정한 감정 사전 활용
new_score_comment %>%
  filter(str_detect(reply, word)) %>%
  count(sentiment)

# 1. 두 글자 이상 한글 단어만 남기고 단어 빈도 구하기
# 토큰화 및 전처리
new_comment <- new_score_comment %>%
  unnest_tokens(input = reply,
                output = word,
                token = "words",
                drop = F) %>%
  filter(str_detect(word, "[가-힣]") &
           str_count(word) >= 2)


# 감정 및 단어별 빈도 구하기
new_frequency_word <- new_comment %>%
  count(sentiment, word, sort = T)

# 2. 로그 오즈비 구하기
# Wide form으로 변환
new_comment_wide <- new_frequency_word %>%
  filter(sentiment != "neu") %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = list(n = 0))


# 로그 오즈비 구하기
new_comment_wide <- new_comment_wide %>%
  mutate(log_odds_ratio = log(((pos + 1) / (sum(pos + 1))) /
                                ((neg + 1) / (sum(neg + 1)))))

# 3. 로그 오즈비가 큰 단어로 막대 그래프 만들기
new_top10 <- new_comment_wide %>%
  group_by(sentiment = ifelse(log_odds_ratio > 0, "pos", "neg")) %>%
  slice_max(abs(log_odds_ratio), n = 10, with_ties = F)


ggplot(new_top10, aes(x = reorder(word, log_odds_ratio),
                      y = log_odds_ratio,
                      fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL) +
  theme(text = element_text(family = "NanumBarunGothicBold"))

# 주요 단어가 사용된 댓글 살펴보기
new_score_comment %>%
  filter(sentiment == "pos" & str_detect(reply, "축하")) %>%
  select(reply)

