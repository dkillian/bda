# Bayesian Data Analysis
# NFL point spreads

df <- read_table("data/NFL point spreads.txt") %>%
    mutate(diff=favorite-underdog,
           sprd_diff = diff-spread,
           fav_win = ifelse(diff>0, 1,0),
           upset=ifelse(diff<0, 1,0),
           spread_trunc = ifelse(spread>14,14, spread))
head(df)

lapply(df[,c(1,5,6,10,11)], frq)

frq(df$spread)
frq(df$spread_trunc)

spr <- df %>%
    group_by(spread_trunc) %>%
    summarise(win=mean(fav_win),
              n=n(),
              se=std.error(fav_win)) %>%
    mutate(lower=win-1.96*se,
           upper=win+1.96*se,
           upper=ifelse(upper>1, 1, upper))

spr

ggplot(spr, aes(spread_trunc, win)) + 
    geom_errorbar(aes(ymin=lower, ymax=upper),
                  width=0,
                  color="dodgerblue2",
                  size=.8,
                  alpha=.7) + 
    geom_point(color="dodgerblue2",
               size=1.8,
               ) +
<<<<<<< HEAD
    stat_smooth(color="dodgerblue2",
                se=F) +
=======
    geom_label(aes(label=paste(round(win*100,0), "%", sep="")),
               size=2.5,
               color="white",
               fill="dodgerblue2",
               label.padding=unit(.1, "lines")) +
    #stat_smooth(color="dodgerblue2") +
>>>>>>> 01af6b18f845a3922eca4e467811627fdd9dd752
    scale_x_continuous(breaks=seq(0,20,1)) +
    scale_y_continuous(limits=c(0,1),
                       breaks=seq(0,1,.1),
                       labels=percent_format(accuracy=1),
                       sec.axis=dup_axis()) +
    labs(x="\nPoint spread", #\n(expected points (favorite) - expected points (underdog)",
         y="",
         title="Win probability by point spread",
         caption="37 spreads greater\nthan 14 coded to 14") +
    theme(axis.title.y=element_text(angle=0, vjust=.5))

ggsave("viz/win probability vs point spread.png",
       device="png",
       type="cairo",
       height=4,
       width=7.4)

lm(fav_win ~ spread + home + as.factor(week),
   data=df) %>%
    summary()

ggplot(df, aes(sprd_diff)) + 
    geom_vline(xintercept=0, color="grey60", size=2, alpha=.6) +
    geom_bar(#color="blue",
             fill="dodgerblue2",
             alpha=.5) + 
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    labs(x="\nOutcome - spread",
         y="",
         title="Difference between outcome and spread")

ggsave("viz/diff between outcome and spread.png",
       device="png",
       type="cairo",
       height=4,
       width=6)

psych::describe(df$sprd_diff)

dnorm(3.5, mean=0, sd=14)

set.seed(432)
ot <- rnorm(100, 0, 14)

ecdf_fun <- ecdf(ot)

ecdf_fun(3.5)

plot(ecdf(ot))

ecdf_fun(8.5)

tes <- tibble(spread=seq(0,14,.5),
              prob=ecdf_fun(seq(0,14,.5)))
tes

ggplot(tes, aes(spread, prob)) + 
    geom_point() +
    stat_smooth()

