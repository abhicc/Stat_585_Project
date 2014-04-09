Draft of Report
========================================================
By Abhishek Chakraborty

I have obtained the data from www.espnsoccernet.com for the players at their national level performances as well their performances for their clubs(includes clubs playing in the leagues of England, Germany, Italy and Spain).I have segmented the players into Goalkeepers and Outfield players.This is because the statistics(columns) for the goalkeepers and the outfield players are different.
Then I have merged the datasets to obtain statistics for each player at their national level as well as club level.


```r
library(XML)
library(plyr)
library(ggplot2)

# Information(id's) of each club playing at each of the above mentioned
# leagues.We need the id's to get the data for each club,since id is the
# unique element that denotes a particular club in the website.
clubs <- read.csv("club teams.csv")
dim(clubs)  # There are 20 clubs in each of English,Italian and Spanish leagues and 18 in German league.
```

```
## [1] 78  3
```

```r
head(clubs)
```

```
##            Teams  id League
## 1        arsenal 359    epl
## 2    aston villa 362    epl
## 3   cardiff city 347    epl
## 4        chelsea 363    epl
## 5 crystal palace 384    epl
## 6        everton 368    epl
```

```r
# Id's for the national teams participating in World Cup 2014.
nations <- read.csv("national teams.csv")
head(nations)
```

```
##   National.Teams  id
## 1      Australia 628
## 2           Iran 469
## 3          Japan 627
## 4    South Korea 451
## 5        Algeria 624
## 6       Cameroon 656
```

```r

# Extracting the data for players of all nations participating in 2014 World
# Cup
nat_goalie <- nat_player <- NULL
for (i in nations$id) {
    url <- paste("http://espnfc.com/team/squad/_/id/", i, "/season/2013/league/all/brazil?cc=5901", 
        sep = "")
    table <- readHTMLTable(url)
    goalie <- table[[1]]
    players <- table[[2]]
    nat_goalie <- rbind(nat_goalie, data.frame(goalie, National.Teams = nations$National.Teams[nations$id == 
        i]))  #  Goalkeeping statistics
    nat_player <- rbind(nat_player, data.frame(players, National.Teams = nations$National.Teams[nations$id == 
        i]))  # Statistics for Outfield players
}

nat_player$G <- as.numeric(as.character(nat_player$G))
nat_player$GS <- as.numeric(as.character(nat_player$GS))
nat_player$SB <- as.numeric(as.character(nat_player$SB))

n.Goals <- ddply(nat_player, .(National.Teams), summarize, Goals.per.match = sum(G)/max(GS + 
    SB), Total.Goals = sum(G))

# Extracting the data for players of all clubs that are part of the
# English,German,Italian and Spanish leagues.
club_goalie <- club_player <- NULL
for (i in clubs$id) {
    url <- paste("http://espnfc.com/team/squad/_/id/", i, "/season/2013/league/all/manchester-united?cc=5901", 
        sep = "")
    table <- readHTMLTable(url)
    cgoalie <- table[[1]]
    cplayers <- table[[2]]
    club_goalie <- rbind(club_goalie, data.frame(cgoalie, Teams = clubs$Teams[clubs$id == 
        i], League = clubs$League[clubs$id == i]))  # Goalkeeping statistics
    club_player <- rbind(club_player, data.frame(cplayers, Teams = clubs$Teams[clubs$id == 
        i], League = clubs$League[clubs$id == i]))  # Statistics for Outfield players
}

goalie <- merge(nat_goalie, club_goalie, by = "NAME")
head(goalie)
```

```
##            NAME NUM.x GS.x SB.x SV.x GC.x FC.x FS.x YC.x RC.x W.x L.x D.x
## 1 Asmir Begovic     -    9    0   14   11    0    1    1    0   1   0   0
## 2    Ben Foster     -    0    1    0    0    0    0    0    0   0   0   0
## 3          Beto    22    2    0    2    2    0    2    0    0   1   0   0
## 4    Brad Guzan     -    1    1    4    5    0    0    0    0   0   0   0
## 5    Brad Jones     -    0    1    3    3    0    0    0    0   0   0   0
## 6 Claudio Bravo     -    9    0   31   10    1    3    1    0   0   0   0
##           National.Teams NUM.y GS.y SB.y SV.y GC.y FC.y FS.y YC.y RC.y W.y
## 1 Bosnia and Herzegovina     1   30    0  104   35    0    5    1    0   1
## 2                England     1   20    0   52   31    1    0    0    0   0
## 3               Portugal    13   36    0   90   47    0    7    3    0   8
## 4          United States     1   32    0   90   48    1    4    1    0   1
## 5              Australia     1    5    0    8    4    0    0    0    0   3
## 6                  Chile     1   36    0  121   54    4    7    3    0   2
##   L.y D.y         Teams  League
## 1   3   0         stoke     epl
## 2   2   0     west brom     epl
## 3   3   3       sevilla la liga
## 4   3   0   aston villa     epl
## 5   3   0     liverpool     epl
## 6   1   2 real sociedad la liga
```

```r
player <- merge(nat_player, club_player, by = "NAME")
head(player)
```

```
##               NAME NUM.x GS.x SB.x G.x SH.x SG.x A.x FC.x FS.x YC.x RC.x
## 1       Aaron Hunt     -    0    1   0    0    0   0    1    2    0    0
## 2     Aaron Lennon     -    0    1   0    0    0   0    0    0    0    0
## 3     Adam Lallana     -    2    1   0    3    2   1    9    5    0    0
## 4        Adil Rami     -    1    1   0    0    0   0    2    0    0    0
## 5        Adil Rami     -    1    1   0    0    0   0    2    0    0    0
## 6 Adlene Guedioura     -    3    1   0    0    0   0    0    0    2    0
##   National.Teams NUM.y GS.y SB.y G.y SH.y SG.y A.y FC.y FS.y YC.y RC.y
## 1        Germany    14   27    0   4   52   22   4   25   45    2    0
## 2        England     7   28    2   1   24    8   1    8   20    1    0
## 3        England    20   34    4  11   66   25   6   34   67    3    0
## 4         France     4    6    0   0    4    0   0    9    5    1    1
## 5         France    13   14    2   3    7    5   1   17   10    3    0
## 6        Algeria    31    4    4   0   13    3   0   10    4    0    0
##            Teams     League
## 1  werder bremen bundesliga
## 2      tottenham        epl
## 3    southampton        epl
## 4       valencia    la liga
## 5       ac milan    seria A
## 6 crystal palace        epl
```

```r

# .x denotes the performances for the national teams and .y for the clubs
player$G.x <- as.numeric(as.character(player$G.x))
player$G.y <- as.numeric(as.character(player$G.y))
player$GS.x <- as.numeric(as.character(player$GS.x))
player$SB.x <- as.numeric(as.character(player$SB.x))
player$GS.y <- as.numeric(as.character(player$GS.y))
player$SB.y <- as.numeric(as.character(player$SB.y))

Goals <- ddply(player, .(National.Teams), summarize, sum.N.G = sum(G.x)/max(GS.x + 
    SB.x), sum.C.G = sum(G.y)/max(GS.y + SB.y))

# I was just exploring the dataset.  I would be looking at information
# somewhat similar to the plot below.  This plots the goals per match of
# each team playing in 2014 edition in the past season(2013/14).It helps us
# to determine the kind of striking form that team is in going into the
# World cup.
qplot(x = reorder(National.Teams, Goals.per.match, sum), weight = Goals.per.match, 
    data = n.Goals, geom = "bar", ylab = "Goals per Match", xlab = "Teams") + 
    coord_flip()
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 

One of the problems still needed to be fixed is the occurrence of some players more than once,since they have played for more than one club in the past season.
I intend to construct a statistic for each player that will predict the impact he is possibly going to have in the World cup for his national team.This statistic will take into account the statistics denoting his performances for the club and national team in the past season.
I also plan to look a bit into some exploratory data analysis for the data we have at hand,like looking into the no. of goals per individual per match,no. of assissts,no. of fouls conceded etc. that will indicate how is the team or the player going to perform at 2014 World Cup.
