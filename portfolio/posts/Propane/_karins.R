base <- ggplot(dat=filter(dWRS,hYearLbl %in% c("2020-21","2021-22","2022-23")),
                  mapping=aes(x=fDate)) +
  geom_line(mapping=aes(y=resWI),linewidth=1) +
  scale_y_continuous(expand=expansion(mult=0.1),limits=c(1,3),
                     labels=scales::label_dollar()) +
  scale_x_date(date_breaks="1 month",date_labels="%b",
               expand=expansion(mult=0.01)) +
  facet_wrap(vars(hYearLbl),scales="free_y",ncol=2,dir="v") +
  theme_main() +
  theme(axis.text.x=element_text(angle=90,vjust=0.3,hjust=1),
        axis.title=element_blank())

kdat <- tibble::tribble(
  ~hYearLbl, ~Type, ~Price,
  "2022-23","Lock-In",1.97,
  "2021-22","Lock-In",1.67,
  "2020-21","Lock-In",1.32,
  "2021-22","Winter",2.39,
  "2021-22","Winter",2.79,
  "2020-21","Winter",1.65
)

base +
  geom_hline(data=kdat,mapping=aes(yintercept=Price,color=Type),
             linetype="dashed",linewidth=0.75) +
  scale_color_manual(values=c("Winter"="red","Lock-In"="green3"),
                     limits=c("Winter","Lock-In")) +
  theme(legend.position=c(0.75,0.15),
        legend.title=element_blank(),
        legend.text=element_text(size=12))
