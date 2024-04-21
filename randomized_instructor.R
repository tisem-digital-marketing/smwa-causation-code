#' randomized.R
#' 
#' Code accompanying Lecture 3: Causation and Randomization
#' of Social Media and Web Analytics
#' at TiSEM in 2024
#' 
#' Instructor Version

# --- Load libraries --- #
library(readr)
library(dplyr)
library(ggplot2)
library(broom)
library(infer)
library(dagitty)
library(ggdag)

# --- Omitted variable bias DAG --- #
dag <- dagify(y ~ x + z,
  x ~ z,
  exposure = "x",
  outcome = "y"
)

ggdag(dag, 
      layout = "circle",
      node_size = 20,
      text_size = 10) +
    geom_dag_edges(edge_width = 2, 
                   #alpha = 0.4,
                   arrow_directed = grid::arrow(length = grid::unit(14, "pt"), type = "closed"),
                   arrow_bidirected = grid::arrow(length = grid::unit(14, "pt"), 
                                                  ends = "both", type ="closed"
                                                  ),
    
                   ) + 
    theme_dag_blank()

# --- Selection Bias DAG --- #
dag <- dagify(y ~ x,
              y ~~ z,
  x ~~ z,
  exposure = "x",
  outcome = "y"
)

ggdag(dag, 
      layout = "circle",
      node_size = 20,
      text_size = 10) +
    geom_dag_edges(edge_width = 2, 
                   #alpha = 0.4,
                   arrow_directed = grid::arrow(length = grid::unit(14, "pt"), type = "closed"),
                   arrow_bidirected = grid::arrow(length = grid::unit(14, "pt"), 
                                                  ends = "both", type ="closed"
                                                  ),
    
                   ) + 
    theme_dag_blank()

# --- Load and Inspect Charity Data --- #
charity <- 
    read_csv("data/exp2data.csv") %>%
    mutate(condition = if_else(condition ==1, 
                               "treatment", 
                               "control"
                               )
    )

head(charity, n = 5)


# --- Plot Clickthrough --- #
charity %>% 
    group_by(condition) %>%
    summarize(click = mean(clickthrough),
              std_err = sd(clickthrough) / sqrt(n())
              )%>%
    ggplot() +
    geom_bar(aes(x = condition, 
                 y = click), 
             stat = "identity", 
             fill = "skyblue", 
             alpha=0.7) +
    geom_errorbar(aes(x = condition, 
                      ymin = click-std_err, 
                      ymax = click+std_err), 
                  width = 0.4, 
                  colour = "orange", 
                  alpha = 0.9, 
                  size = 1.5) +
    scale_y_continuous(limits=c(.13,.155), 
                       oob = scales::squish) +
    theme_bw() + 
    ggtitle("Clickthrough Rate") +
    theme(text = element_text(size = 28),
          plot.title = element_text(hjust = 0.5))


charity %>% 
    group_by(condition) %>%
    summarize(recruit = mean(recruited),
              std_err = sd(clickthrough) / sqrt(n())
              )%>%
    ggplot() +
    geom_bar(aes(x = condition,
                 y = recruit), 
             stat = "identity", 
             fill = "skyblue", 
             alpha = 0.7) +
    geom_errorbar(aes(x = condition, 
                      ymin = recruit - std_err, 
                      ymax = recruit + std_err), 
                  width=0.4, 
                  colour="orange", 
                  alpha=0.9, 
                  size=1.5) +
    scale_y_continuous(limits=c(.025,.035), 
                       oob = scales::squish) +
    theme_bw() +
    ggtitle("Recruitment Rate") +
    theme(text = element_text(size = 28),
          plot.title = element_text(hjust = 0.5))

# --- Statistical Tests ---# 
charity %>% 
    group_by(condition) %>%
    summarize(click = mean(clickthrough),
              recruit = mean(recruited))

charity <-
    charity %>%
    mutate(clickthrough = as.logical(clickthrough),
           recruited = as.logical(recruited)
    )

# clickthru
prop_test(charity,
          clickthrough ~ condition
          )

# recruit
prop_test(charity,
          recruited ~ condition
          )

# --- Regression Based Inference --- #
clicks <- lm(clickthrough ~ condition, data = charity)
tidy(clicks)

recruit <- lm(recruited ~ condition, data = charity)
tidy(recruit)

