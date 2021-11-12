The site for writting the MD is
<https://dillinger.io/>

# Working Path with Data

The CRoss Industry Standard Process for Data Mining (CRISP-DM) is a process model with six phases that naturally describes the data science life cycle. It’s like a set of guardrails to help you plan, organize, and implement your data science (or machine learning) project.<br/>

- Business understanding – What does the business need? <br/>
- Data understanding – What data do we have / need? Is it clean?<br/>
- Data preparation – How do we organize the data for modeling?<br/>
- Modeling – What modeling techniques should we apply?<br/>
- Evaluation – Which model best meets the business objectives?<br/>
- Deployment – How do stakeholders access the results?<br/>

Just like the follwoing picture. <br/>
![][image_ref_a32ff4ads]<br/>

![][image_ref_a32ff4ads2]<br/>

# First: Data Understanding

I merge the first and second steps in this section, at first we have to recognize our goal and question, and other procedures would be defined in the following steps.<br/>

- *Generate questions about your data*<br/>
- *Search for answers by visualising, transforming, and modelling your data*<br/>
- *Define object*<br/>

Defining your objective means coming up with a hypothesis and figuring how to test it. Start by asking: What business problem am I trying to solve? While this might sound straightforward, it can be trickier than it seems. For instance, your organization’s senior management might pose an issue, such as: “Why are we losing customers?” It’s possible, though, that this doesn’t get to the core of the problem. A data analyst’s job is to understand the business and its goals in enough depth that they can frame the problem the right way.<br/>

After that, we have to discuss and mention how this data was collected, and what kind of data we have, for instance, Flat Files (CSV, tsv), HTML, XML, JSON, Relational Databases, Non-Relational Databases, and APIs. <br/>
For more detail the I recommend taking a look at these links:
[Career Foundery](https://careerfoundry.com/en/blog/data-analytics/the-data-analysis-process-step-by-step/)<br/>

# Second: Exploratory Data Analysis

In this step, we have to go deep in the database, at first we have to deal with missing values, and after that outlier. However, sometimes we have to take a look more at data and figure out how we should deal with them. And we have to consider the best approach against missing value. <br/>

[Initial Exploratory](https://towardsdatascience.com/a-basic-guide-to-initial-and-exploratory-data-analysis-6d2577dfc242)
[Data analysis in kaggle](https://www.kaggle.com/ekami66/detailed-exploratory-data-analysis-with-python/notebook)
[R Script for data-visualisation](https://r4ds.had.co.nz/data-visualisation.html)

[image_ref_a32ff4ads2]: https://dpbnri2zg3lc2.cloudfront.net/en/wp-content/uploads/old-blog-uploads/the-data-analysis-process-1.jpg
[image_ref_a32ff4ads]: https://www.datascience-pm.com/wp-content/uploads/2021/02/CRISP-DM.png

## Data Understanding Steps

**Step 1**: relevant  data?<br />
**Step 2**: data sources?<br/> 
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; *All detail of data and what are they mean in R I can see S06 or Page 19 notebook* ...<br/>
**Step 3**: Data Understanding from Statistical Perspective<br />
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; In R I can see S06 [Statistical Perspective in R](https://github.com/mazibazi/FundamentalSteps/blob/40df868c8d04699b854850ceff8927bb87528487/S06/S06.R#L99) Page 19 notebook<br>
**Step 4**: Data Manipulation <br />
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; for this I can see Page 21 notebook or S07 [Data Manipulation in R](https://github.com/mazibazi/FundamentalSteps/blob/40df868c8d04699b854850ceff8927bb87528487/S07/S07.R#L11)
