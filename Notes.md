## Notes about Findings

Mean squared error % increase refers to the amount that the MSE increases when that variable is randomly shuffled/permuted. It seems from our RF that time spent yields a large % change in MSE and that it is very important in our prediction.

# Research questions

-   Is motivation relatively more predictive of course grades as compared to other online indicators of engagement?
    -   Which types of motivation are most predictive?
    -   Which types of trace measures are most predictive?

#general notes
-Mot variables include int/comp/utility
-Other variables include time spent, enrollment reason,frequ of discussion posts, quality of discussion posts (e.g. cognitive processing - liwc)
- we are including subject instead of course_ID bc 

# Main Findings

-   Overall prediction of RF model w/ motivation vs. without
-   Overall prediction of RF model w/ discussion board data and without

-   For each of those above, present mean residual values.

#Next steps 9.13.18
-   Josh will add a dataframe with 6-7 liwc summary stats 
-   Emily will re-run the models, generate the output - maybe convert to an R markdown
-   File > new markdown > from template (papaja)