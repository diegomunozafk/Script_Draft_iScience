>library(httpuv)
>library(crosstable)
>library(flextable)
>library(writexl)
>library(dyplr)
>library(pwr)

>n1<-55
>n2<-88
>power <- pwr.t2n.test(n1 = n1, n2 = n2, d = 0.5, sig.level = 0.05, alternative = "two.sided")
print(power)
>ct <- crosstable(data = DATA, by = "group", test = T) %>% 
  as_flextable(keep_id =TRUE)
>ct <- crosstable(data = DATA, by = "group", test = TRUE) # Create the crosstable
>ft <- as_flextable(ct, keep_id = TRUE)
>df <- as.data.frame(ct) # Convert flextable to a data frame for exporting
>write_xlsx(df, path = "crosstable.xlsx") 
> DATA _cor <- cor(DATA [1:X],method="spearman",use='pairwise.complete.obs')
> DATA_cor 
>#install.packages("Hmisc") 
  >library(writexl)
>library(Hmisc)
>DATA_cor <- rcorr(as.matrix(DATA[, 1:58]), type="spearman") # Compute the correlation matrix and p-values
>cor_matrix <- DATA_cor$r # Extract the correlation coefficients and p-values
>p_matrix <- DATA_cor$P
> combined_matrix <- data.frame(Variable1 = rep(colnames(cor_matrix), times = ncol(cor_matrix)), Variable2 = rep(colnames(cor_matrix), each = ncol(cor_matrix)), Correlation = as.vector(cor_matrix),PValue = as.vector(p_matrix))
>write_xlsx(combined_matrix, path = "CORRELATIONS.xlsx") # Export the combined data frame to an Excel file
>#Graphic representation
  > install.packages(“ggplot2”)
>library(ggplot2)
>install.packages("extrafont")
>library(extrafont)
>ggplot(DATA, aes(x = RNA expression, y = variable1)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(x = "RNA expression", y = "variable1") +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title.x = element_text(family = "Times New Roman"),
    axis.title.y = element_text(family = "Times New Roman"),
    axis.line = element_line(color = "black", size = 0.5), 
    axis.ticks = element_line(color = "black") 
  )
ggsave("variable1.pdf", plot = last_plot(), device = "pdf")
> install.packages("sjPlot") 
>library(sjPlot)
> finalmodel<-glm(SDC3 ~ variable1 + variable2 + …, data= DATA, family = "binomial", epsilon = 1) # multivariate logistic regression model
>tab_model(finalmodel)
>AIC(finalmodel)
>adjustedfinalmodel <- step(modelofinal, direction = "backward")



