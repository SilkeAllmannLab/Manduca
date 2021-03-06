data = read.table("figure1/Juliette_data_VI.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)

view (data)

str(data)

ggplot(data = data, aes(x = genotype, y = weight, color = genotype)) +
  geom_boxplot() +
  labs(x = "Genotype CP",
       y = "Weight larvae (grams)",
       title = "Weight WT and KO m. sexta larvae feeding on N. attenuata plants") +
  geom_point((aes(color=genotype)), position=position_jitterdodge(), alpha=0.6, size=0.8) +
  theme_bw()

ggplot(data = data, aes(x = genotype, y = weight, color = genotype)) +
  facet_grid(~day, labeller = label_both) +
  geom_boxplot() +
  labs(x = "Genotype CP",
       y = "Weight larvae (grams)",
       title = "Weight WT and KO m. sexta larvae feeding on N. attenuata plants") +
  geom_point((aes(color=genotype)), position=position_jitterdodge(), alpha=0.6, size=0.8) +
  theme_bw()

ggplot(data = data, aes(x= day, y = weight, color = genotype)) +
  geom_line(aes(group = interaction(genotype, replicate), color = genotype)) +
  facet_grid(~block, labeller = label_both) +
  labs(x = "Age caterpillars (days)",
       y = "Weight larvae (grams)",
       title = "Weight WT and KO m. sexta larvae feeding on N. attenuata plants") +
  theme_bw()


