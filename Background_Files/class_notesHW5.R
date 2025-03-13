


## explore data
pairs(snijders)

GGally::ggpairs(snijders)

skimr::skim_without_charts(snijders)

snijders %>% select(-school) %>% cor(use = "pairwise.complete.obs") %>% 
  corrplot(
    method = "square",
    type = "lower",
    order = "hclust"
  )


naniar::miss_var_summary(snijders)
naniar::gg_miss_upset(snijders)

# Only include complete cases
cc_snijders <- snijders %>% na.omit()

cc_snijders %>% select(-school) %>% cor(use = "pairwise.complete.obs") %>% 
  corrplot(
    method = "square",
    type = "lower",
    order = "hclust"
  )


# Create centered vars
cc_snijders %>% mutate(
    ses_c = ses - mean(ses),
    iq_c = iq - mean(iq)
)  # but this centers around the mean of the entire dataset, not by school. 


# alternative is:
cc_snijders <- cc_snijders %>% mutate(
  ses_c = ses - mean(ses),
  iq_c = iq - mean(iq),
  .by = school # calculates the mean by school
)

# cc_snijders <- cc_snijders %>% mutate(
#   ses_c = ses - mean(ses),
#   iq_c = iq - mean(iq),
#   .keep = "used", # this just shows what the transformations are, keeps the vars used in the calculation
#   .by = school # calculates the mean by school
# )

# or also:
cc_snijders %>% group_by(school) %>% 
  mutate(
  ses_c = ses - mean(ses),
  iq_c = iq - mean(iq),
) # using group_by() attaches a permanent feature to the object. 
# When using .by =  inside mutate, as in example above, it doesnt attach any 
# new features to dataset. Only does the by for the specific calculation.


# we could also create the dataset at the school level, collapsing...
cc_snijders %>%  summarise(
  across(everything(), mean), 
  num_students = n(), #tot students per school
  .by = school
)
# this is the equivalent of doing a collapse in Stata



# calculating within school correlations
cc_snijders %>% summarise(
  correlation_ses = cor(test, ses_c),
  correlation_iq = cor(test, iq_c),
  .by = school
) %>%  skimr::skim_without_charts()


## Part 1.b

# we can also use a for-loop, which I will probably do in my HW, but he is also doing it through tidy

(lm_per_school <- cc_snijders %>% nest(.by = school) %>% 
  mutate(
    fit_lm = map(data, \(x) lm(test ~ ses_c + iq_c, data = x)),
    coef_info = map(fit_lm, broom::tidy), 
    model_diag = map(fit_lm, broom::glance), 
    ses_sch = map_dbl(data, \(x) mean(x$ses))
  ) %>% unnest(coef_info))



lm_per_school %>% left_join(sch_info)  # this is merging with school info 
# that Arend created before but I did not copy. So this will not run, but its the syntax for joining.





