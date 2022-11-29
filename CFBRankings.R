
install.packages("tidyverse")
library("tidyverse")
library("dplyr")
library("readr")

NCAA_FB <- read_csv("https://raw.githubusercontent.com/MattC137/Open_Data/master/Data/Sports/NCAAF/NCAAF_Level_One.csv")
NCAA_Teams <- read_csv("https://raw.githubusercontent.com/MattC137/Open_Data/master/Data/Sports/NCAAF/NCAAF_Team_List.csv")

# Setup

NCAA_FB <- NCAA_FB %>% mutate(
  ELO = 0,
  Opp_ELO = 0,
  Result = ifelse(Result == "W", 1, Result),
  Result = ifelse(Result == "L", 0, Result),
  Result = ifelse(Result ==  "T", 0.5, Result), 
  Result = as.numeric(Result)
)
# Why is this not updating results with the correct numeric response regarding W, L

NCAA_Teams <- NCAA_Teams %>% mutate(
  ELO = ifelse(FBS == 1, 1500, 1200)
)

NCAA_FB_Future <- NCAA_FB %>% 
  filter(Played == FALSE, Game_ID != "Cancelled", Game_ID != "Postponed")

NCAA_FB <- NCAA_FB %>% 
  filter(Played == TRUE) %>% 
  arrange(Date, Game_ID)

# Elo Rating System

for(i in 1:nrow(NCAA_FB)){
  if(i %% 2 !=0) {

    print(i)
    Team_A <- NCAA_FB$Team[i]
    Team_B <- NCAA_FB$Team[i+1]
    Result_A <- NCAA_FB$Result[i]
    Result_B <- NCAA_FB$Result[i+1]
    ELO_A <- as.numeric(NCAA_Teams[NCAA_Teams$Team == Team_A, "ELO"])
    ELO_B <- as.numeric(NCAA_Teams[NCAA_Teams$Team == Team_B, "ELO"])
    
    # Load current ELO into main NCAA_FB dataset
    NCAA_FB$ELO[i] <- ELO_A
    NCAA_FB$ELO[i+1] <- ELO_B
    NCAA_FB$Opp_ELO[i] <- ELO_B
    NCAA_FB$Opp_ELO[i+1] <- ELO_A
    
    ## Update ELO

    R_A <- 10^(ELO_A/400)
    R_B <- 10^(ELO_B/400)

    E_A <- R_A/(R_A + R_B)
    E_B <- R_B/(R_A + R_B)

    Elo_Updated_A <- ELO_A + 40 * (Result_A - E_A)
    Elo_Updated_B <- ELO_B + 40 * (Result_B - E_B)
    
    #Update Team ELOs  
    NCAA_Teams[NCAA_Teams$Team == Team_A, "ELO"] <- Elo_Updated_A
    NCAA_Teams[NCAA_Teams$Team == Team_B, "ELO"] <- Elo_Updated_B
  }
}

View(NCAA_Teams %>% filter(FBS == 1) %>% arrange(desc(ELO)) %>% top_n(25))

# Naive wins based on ELO 

NCAA_FB <- NCAA_FB %>% mutate(
  ELO = as.numeric(ELO),
  Opp_ELO = as.numeric(Opp_ELO),
  Elo_Difference = ELO - Opp_ELO,
  Elo_Forecast_Pred = ifelse(ELO > Opp_ELO, 1, 0),
  Elo_Forecast_Result = ifelse(Elo_Forecast_Pred == Result, 1, 0)
)

Results_2019 <- NCAA_FB %>% filter(Season == 2019)
sum(Results_2019$Elo_Forecast_Result)/nrow(Results_2019)

### Spread Forecast

spread_lm_1 <- glm(Spread ~ Elo_Difference + Home, data = NCAA_FB %>% filter(Season > 2013, Season <= 2018))
NCAA_FB$Spread_Pred_1m_1 <- predict(spread_lm_1, newdata = NCAA_FB)
Results_2019$Spread_Pred_1m_1 <- predict(spread_lm_1, newdata = Results_2019)

# Win Probability 

win_prob_glm_1 <- glm(Result ~ Elo_Difference + Home, family = binomial, data = NCAA_FB %>% 
                        filter(Season > 2013, Season <= 2018))
NCAA_FB$win_prob_glm_1 <- predict(win_prob_glm_1, newdata = NCAA_FB, type = "response")
Results_2019$win_prob_glm_1 <- predict(win_prob_glm_1, newdata = Results_2019, type = "response")

# Evaluate 2019

Results_2019$win_prob_glm_1 <- ifelse(Results_2019$win_prob_glm_1 > 0.5, 1 ,0)
sum(Results_2019$win_prob_glm_1 == Results_2019$Result)/nrow(Results_2019)

# Forecast the upcoming week

NCAA_FB_Future$Date <- as.Date(NCAA_FB_Future$Date, origin = "1970-01-01")
NCAA_This_Week <- NCAA_FB_Future %>% filter(Date > "2020-12-30", Date <= "2021-01-02") %>% 
  select(Date, Season, Team, Opponent, Home, Neutral_Location, Game_ID)

NCAA_This_Week <- NCAA_This_Week %>% 
  left_join(NCAA_Teams %>% select(Team, ELO), by = c("Team" = "Team"))

NCAA_This_Week <- NCAA_This_Week %>% 
  left_join(NCAA_Teams %>% select(Team, ELO), by = c("Opponent" = "Team"))

names(NCAA_This_Week) <- c("Date", "Season", "Team", "Opponent", "Home", "Neutral Location", 
                           "Game_ID", "ELO", "Opp_ELO")

NCAA_This_Week <- NCAA_This_Week %>% mutate(
  Elo_Difference = ELO - Opp_ELO
)

NCAA_This_Week$win_prob_glm_1 <- predict(win_prob_glm_1, newdata = NCAA_This_Week, type = "response")
NCAA_This_Week$Spread_Pred_lm_1 <- predict(spread_lm_1, newdata = NCAA_This_Week)







