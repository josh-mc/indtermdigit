#Graphs and tables

#This scrip creates and saves figures 1 - 4 which are
#based on three sets of results for Monte Carlo simulations.
#It also creates tables 1 and 2.


#Setup####
knitr::opts_chunk$set(echo = FALSE)
options(dplyr.summarise.inform = FALSE)

devtools::load_all()
library(tidyverse)
library(gt)

exponential_all <- readRDS("paper/data/e_all.RDS") %>%
  rename(n = number,
         O = d_perm_p,
         AF = d_av_fre_p,
         X2 = d_chi_p,
         G2 = d_g2_p,
         FT = d_ft_p,
         RMS = d_rms_p) %>%
  mutate(distribution = "exponential",
         parameter = ifelse(parameter == 1, "Exp(1)",
                            ifelse(parameter == 0.5, "Exp(0.5)", "Exp(0.25)")),
         parameter = factor(parameter, levels = c("Exp(1)", "Exp(0.5)", "Exp(0.25)")),
         duplicates = duplicates / n)

uniform_all <- readRDS("paper/data/u_all.RDS") %>%
  rename(n = number,
         O = d_perm_p,
         AF = d_av_fre_p,
         X2 = d_chi_p,
         G2 = d_g2_p,
         FT = d_ft_p,
         RMS = d_rms_p) %>%
  mutate(parameter = ifelse(parameter == 5, "U(0, 5)",
                            ifelse(parameter == 10, "U(0, 10)", "U(0, 20)")),
         parameter = factor(parameter, levels = c("U(0, 5)", "U(0, 10)", "U(0, 20)")))

normal_all <- readRDS("paper/data/n_all.RDS") %>%
  rename(n = number,
         O = d_perm_p,
         AF = d_av_fre_p,
         X2 = d_chi_p,
         G2 = d_g2_p,
         FT = d_ft_p,
         RMS = d_rms_p) %>%
  mutate(parameter = ifelse(parameter == 1, "SD(1)",
                            ifelse(parameter == 2, "SD(2)", "SD(5)")),
         parameter = factor(parameter, levels = c("SD(1)", "SD(2)", "SD(5)")))


u_n_e <- bind_rows(normal_all, uniform_all, exponential_all) %>%
  mutate(duplicates = paste(duplicates * 100, "% duplicates", sep = ""),
         duplicates = factor(duplicates, levels = c("0% duplicates", "5% duplicates", "10% duplicates", "20% duplicates")),
         decimals = ifelse(decimals == 1, "1 decimal", "2 decimals"))

#Figure 1####

fig_1 <- u_n_e %>%
  filter(duplicates == "0% duplicates") %>%
  select(n, parameter, duplicates, distribution, decimals, O:RMS) %>%
  pivot_longer(cols = -c(n, parameter, duplicates, decimals, distribution)) %>%
  ggplot(aes(n, value, color = name)) +
  geom_point(aes(shape = name)) +
  geom_line() +
  geom_hline(yintercept = 0.05,
             linetype = 2) +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("Type I errors") +
  theme_minimal() +
  facet_wrap(~distribution + decimals + parameter,
             nrow = 3) +
  theme(strip.text.x = element_text(margin = margin(2, 0, 2, 0)),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank()
  )

fig_1

ggsave(filename = "paper/figures_tables/McCormick_Figure_1.pdf",
       plot = fig_1,
       dpi = 800,
       height = 10,
       width = 7.5,
       units = "in")

#Figure 2####

fig_2 <- u_n_e %>%
  filter(distribution == "normal",
         duplicates != "0% duplicates",
         !(parameter == "SD(1)" & decimals == "1 decimal")) %>%
  select(n, parameter, duplicates, decimals, O:RMS) %>%
  pivot_longer(cols = c(-n, -parameter, -duplicates, -decimals)) %>%
  ggplot(aes(n, value, color = name)) +
  geom_point(aes(shape = name)) +
  geom_line() +
  geom_hline(yintercept = 0.8,
             linetype = 2) +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("Power for truncated normal distributions") +
  theme_minimal() +
  facet_wrap(~duplicates + decimals + parameter,
             nrow = 3) +
  theme(strip.text.x = element_text(margin = margin(2, 0, 2, 0)),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank()
  )

fig_2

ggsave(filename = "paper/figures_tables/McCormick_Figure_2.pdf",
       plot = fig_2,
       dpi = 800,
       height = 10,
       width = 7.5,
       units = "in")

#Figure 3 ####

fig_3 <- u_n_e %>%
  filter(distribution == "exponential",
         duplicates != "0% duplicates") %>%
  select(n, parameter, duplicates, decimals, O:RMS) %>%
  pivot_longer(cols = c(-n, -parameter, -duplicates, -decimals)) %>%
  ggplot(aes(n, value, color = name)) +
  geom_point(aes(shape = name)) +
  geom_line() +
  geom_hline(yintercept = 0.8,
             linetype = 2) +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("Power for truncated exponential distributions") +
  theme_minimal() +
  facet_wrap(~duplicates + decimals + parameter,
             nrow = 3) +
  theme(strip.text.x = element_text(margin = margin(2, 0, 2, 0)),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank()
  )

fig_3

ggsave(filename = "paper/figures_tables/McCormick_Figure_3.pdf",
       plot = fig_3,
       dpi = 800,
       height = 10,
       width = 7.5,
       units = "in")

#Figure 4 ####

fig_4 <- u_n_e %>%
  filter(distribution == "uniform",
         duplicates != "0% duplicates") %>%
  select(n, parameter, duplicates, decimals, O:RMS) %>%
  pivot_longer(cols = c(-n, -parameter, -duplicates, -decimals)) %>%
  ggplot(aes(n, value, color = name)) +
  geom_point(aes(shape = name)) +
  geom_line() +
  geom_hline(yintercept = 0.8,
             linetype = 2) +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("Power for truncated uniform distributions") +
  theme_minimal() +
  facet_wrap(~duplicates + decimals + parameter,
             nrow = 3) +
  theme(strip.text.x = element_text(margin = margin(2, 0, 2, 0)),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank()
  )

fig_4

ggsave(filename = "paper/figures_tables/McCormick_Figure_4.pdf",
       plot = fig_4,
       dpi = 800,
       height = 10,
       width = 7.5,
       units = "in")

#Table 1 ####

Rows <- c(1, 2, "Total")
Zero <- c(0, 1, 1)
One <- c(2, 1, 3)
Two <- c(1, 0, 1)
Three <- c(2, 0, 2)
Four <- c(0, 1, 1)
Total <- c(5, 3, 8)

data.frame(Rows, Zero, One, Two, Three, Four, Total) %>%
  gt() %>%
  tab_header(
    title = "Contingecy table for toy data set"
  ) %>%
  tab_spanner(label = "Terminal Digit",
              columns = c(Zero:Four)) %>%
  cols_label(
    Rows = "Preceding Digits",
    Zero = "0",
    One = "1",
    Two = "2",
    Three = "3",
    Four = "4") %>%
  gtsave("paper/figures_tables/McCormick_Table_1.rtf")

#Table 2 ####

a <- u_n_e %>%
  filter(duplicates != "0% duplicates") %>%
  select(O:RMS) %>%
  summarise(across(everything(.), ~mean(.x))) %>%
  mutate(Decimals = "All") %>%
  select(Decimals, O:RMS)

b <- u_n_e %>%
  filter(duplicates != "0% duplicates",
         decimals == "1 decimal") %>%
  select(O:RMS) %>%
  summarise(across(everything(.), ~mean(.x))) %>%
  mutate(Decimals = "One") %>%
  select(Decimals, O:RMS)

c <- u_n_e %>%
  filter(duplicates != "0% duplicates",
         decimals == "2 decimals") %>%
  select(O:RMS) %>%
  summarise(across(everything(.), ~mean(.x))) %>%
  mutate(Decimals = "Two") %>%
  select(Decimals, O:RMS)

bind_rows(a, b, c) %>%
  gt() %>%
  tab_header(
    title = "Average statistical power"
  ) %>%
  fmt_number(
    columns = 2:7,
    decimals = 2
  ) %>% gtsave("paper/figures_tables/McCormick_Table_2.rtf")
