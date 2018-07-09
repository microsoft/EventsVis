# *eventsVis*
A tool for analyzing and visualizing temporal descrete events.

#### This tools allows the user to inspect events in various forms, to see the interactions of events of various types by looking at them temporally, by looking at consecutive patterns and co-occurring patterns and by inspecting the overall duration or count of events througout time. 
##### It also allows simple manipulations of the data such as grouping of adjacent similar events and deciding on co-occurrence and consecutive events thresholds

This tool uses the [shiny framework](https://shiny.rstudio.com/) for visualizing events.
In order to run it, you need to have [R](https://mran.microsoft.com/download) and preferably [Rstudio](https://www.rstudio.com/products/rstudio/download/).

### EventsVis currently supports these visualizations:
* Timeline
* Distribution of events
* Consecutive events analysis
* Co-occurring events analysis
* SQL querying on dataset

### Future plans:
 * Context (time in day, day, holidays, season, weather, location and more)
 * Various sequential rules discovery algorithms
 * Common co-occurring and consecutive patterns discovery

You can see a live example of the tool on ShinyApps (a bit slow but works):
[https://omrimendels.shinyapps.io/eventsvis/]


## Visualizations:

### Timeline:
![Timeline](https://github.com/omri374/eventsVis/raw/master/img/timeline.png)

In addition, it allows you to group adjacent similar events together (to ignore the effect of multiple events calls that are actually one event in reality).


### Events distributions accross sessions:
![Distributions](https://github.com/omri374/eventsVis/raw/master/img/distributions.png)

### Consecutive events analysis
Which events occur after others? *Notice the cluster around kitchen activity*

![Consecutive](https://github.com/omri374/eventsVis/raw/master/img/consecutive.png)

Visualization: visNetwork

### Co-occurring events analysis
Which events co-occur together with others? *Notice the cluster/clique around kitchen activity and around living room activity*

![Consecutive](https://github.com/omri374/eventsVis/raw/master/img/cooccurring.png)

Visualization: visNetwork


### SQL querying: 
![SQL](https://github.com/omri374/eventsVis/raw/master/img/sql.png)



## Run the app locally:
Clone the github repo to your PC. Should work on Windows/Mac/Linux, tested on Windows
Option 1: Open the project in R studio, open the server.R file and click on the play button. 

Option 2: Install the 'shiny' package and call


    R -e "shiny::runApp('~/eventsVis')"

or change '~/eventsVis' to the app path.

#### Used packages:
- shiny
- shinydashboard
- googleVis
- data.table
- dplyr
- DT
- sqldf
- ggplot2
- visNetwork
- RColorBrewer


## Contributing

This project welcomes contributions and suggestions.  Most contributions require you to agree to a
Contributor License Agreement (CLA) declaring that you have the right to, and actually do, grant us
the rights to use your contribution. For details, visit https://cla.microsoft.com.

When you submit a pull request, a CLA-bot will automatically determine whether you need to provide
a CLA and decorate the PR appropriately (e.g., label, comment). Simply follow the instructions
provided by the bot. You will only need to do this once across all repos using our CLA.

This project has adopted the [Microsoft Open Source Code of Conduct](https://opensource.microsoft.com/codeofconduct/).
For more information see the [Code of Conduct FAQ](https://opensource.microsoft.com/codeofconduct/faq/) or
contact [opencode@microsoft.com](mailto:opencode@microsoft.com) with any additional questions or comments.


### Additional visualizations and interaction ideas are mostly welcome.
