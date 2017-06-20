Readme
======


Brief rundown of files:

* exploring_data.nb.html - the final report
* exploring_data.Rmd - raw markdown source.
* fetch_2015.R - script to fetch data.
* api_trying.R - my initial explorations
* data/ the data, extracted from the OpenFDA api

Fetching Data
-------------

To fetch the data, you will need to create a file called `api_key.R`, which should look like

```r
api_key <- "<insert key here>"
```

or you'll run up against rate limits.
