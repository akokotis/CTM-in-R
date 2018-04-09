# Correlated Topic Modeling

## Background
Similar to Latent Dirichlet Analysis (LDA), CTM is a probabilistic approach to infer the latent topics of a document. Unlike LDA, which assumes independence between documents, CTM allows for latent topics to be correlated. I am using topic modeling to subset a collect of retail products (unstructured data) into like-itemed groups. I perform CTM on the name of the product and on the first 50 words of the product description (I assume that past 50 words is mostly noise). 

I run the CTM along with a K-Means and compare results. I've been getting better accuracy with the K-Means, which I will deep dive into investigating why.

## Assumptions of CTM
* Each document is comprised of multiple topics and each topic is made up of multiple words
* Order of words does not matter
* The word distribution per topic and topic distribution per document follow a logistic normal distribution

### Resources
R package [topicmodels](https://cran.r-project.org/web/packages/topicmodels/vignettes/topicmodels.pdf), white paper [Correlated Topic Models](http://people.ee.duke.edu/~lcarin/Blei2005CTM.pdf)
