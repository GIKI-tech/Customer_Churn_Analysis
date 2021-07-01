# Customer Churn Prediction in E-commerce

We are going to use the Brazilian [E-Commerce Public Dataset by Olist from Kaggle](https://www.kaggle.com/olistbr/brazilian-ecommerce) to predict customer churn.

To do this, two different data mining techniques were applied.

## Results

First, the EDA was done to find out the overall situation in the store in the past years. There was a significant drop of sales and amount of customers in 2018. The same trend is visible per all states in Brazil. Potential causes of this loss were discussed. One of them is that the rival Amazon took the market at the end of 2017.

The second important step was an actual prediction. 20\% of data was splited as  test data and other 80\% left as an actual data. Using data mining approach, the decision tree model was created with $94.8\% accuracy. In contrast, the random forest algorithm hit $97.9\% accuracy. Even though both models performed great, considering other factors as accuracy and sensitivity, random forest is definitely more accurate choice for prediction.



