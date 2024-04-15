# wiNBS

## Working in NBS (wiNBS)

This package is facilitates the creation of RSelenium-based scripts to
control web browsers. It has a particular focus at this time on NBS, but
could be extended to other surveillance systems. This package is built
upon `RSelenium`. Refer to [the official
documentation](https://cran.r-project.org/web/packages/RSelenium/vignettes/basics.html)
for more guidance.

The functions in `wiNBS` create or utilize the global object `remDr`,
which represents the browser. Most functions do not return values, but
instead prompt `remDr` to perform certain actions which are commonly
used in NBS data edits. `RSelenium` functions are usually accessed using
`remDr$function()` and allow for script specific functionality not found
in this package. If you have commonly used snippets across scripts,
consider adding them to this package as functions.

A typical workflow is provided here:

1.  Open a browser

2.  Navigate to a page

3.  Interact with elements on the page

    1.  Find an element

    2.  Interact with it (extract info, click, send text, etc.)

    3.  Repeat as needed

4.  Close the browser

### Download this package

``` r
devtools::install_github("TDH-SSI-Hub/wiNBS")
library("wiNBS")
```

## Opening a Browser

Use `browser_open('Chrome')` or `browser_open('Firefox')` to open a
browser. Make sure whichever type of browser you run is installed on
your machine. Additional arguments to `browser_open()` specify if java
operations should be cleared, and which port to use. For Chrome, you may
also specify a specific version to use, and a place to download files
and printed pdfs.

Running the function does not return a value, but the browser object
`remDr` is created in the global environment.

## Logging in to NBS

To log in to NBS, you first need to set your password on your machine.
the `nbs_password_set('your_username','your_password')` function allows
you to securely store your NBS password using the `keyring` package.
When set using this method, your password is only accessible when run on
your machine, logged in with your credentials. If you share the script
with anyone else, they will not be able to access your password to log
in. After you run `nbs_password_set()` be sure to delete
`nbs_password_set()` so your credentials are not saved to the R file.
Once set, you may provide your username to
`nbs_password_get('your_username')` to retrieve your NBS password, but
this is not advised. Do not store your password in the global
environment.

Once you have set your password, you can use `nbs_load('your_username')`
to log in to NBS. This will navigate to NBS, log in with your username
and password, and select the production environment (can change using
the `environment` parameter).

``` r
# Open a browser, Chrome is the default
browser_open()

# Save your username
username<-'nbs_user23'

# Run once then delete
nbs_password_set(username,'fake_password23')

# Log in to NBS
nbs_load(username)
```

## General use functions

From any page, you can use `nbs_home_page()` to return to the NBS home
page. `nbs_page_is_legacy()` Should return T/F depending on if the page
is legacy or not. Some functions in this package do not accommodate
legacy pages. `nbs_pdf_print()` should work from most pages with a print
option. Currently, printing only works with Chrome browsers.

## Searching

From the NBS home page, you can use `nbs_search()` to find patients,
labs, investigations, etc.. By default, `nbs_search()` looks for
investigations, but you can specify any value from the event ID type
dropdown. Direct patient search is not supported at this time.
`nbs_search()` will take you to the patient landing page. To go to an
investigation directly, use `nbs_investigation_go_to()`. This function
will go directly to an investigation from anywhere in NBS (using the
search functionality if needed). If you are not logged in to NBS, this
function will call `nbs_load()` to log in (requires `username` object in
global environment).

``` r
# Go to patient with investigation CAS12345678
nbs_search('CAS12345678')

# Go to patient with lab OBS12345678
nbs_search('OBS12345678', ID_type = 'Lab ID')

# Go to investigation CAS12345678
nbs_investigation_go_to('CAS12345678')
```

## Queues

From the home page, you can load a queue using the partial link text
(see below). In the future, this can be made into a convenience function
as well. Once inside the queue, you can use `nbs_queue_filter()` to
activate the filters for any column that uses check boxes as options. By
default, the text found in `search_for` is sued to find an exact match
in the UI options. You may set `grepl=TRUE` to employ pattern matching
instead. Note that the `select_all` parameter affects if the ‘Select
All’ option gets clicked prior to selection of other options. ‘Select
All’ is selected by default in the UI, and `select_all` is `FALSE` by
default, so you must set `select_all` to `TRUE` in order to select only
the option(s) specified in `search_for`.

``` r
# From home page, go to DRR
remDr$findElement('partial link text','Document Requiring Review')$clickElement()

# For column 1 (document type), uncheck 'Select All', then select only the 'Lab Report' option
nbs_queue_filter(1,'Lab Report', select_all=T)

# For column 5 (description), uncheck 'Select All', then select labs with 'COV' in the description
nbs_queue_filter(5,'COV', grepl=T , select_all=T)

# For column 6 (jurisdiction), deselect labs that are 'Out of State' or 'Out of System'
nbs_queue_filter(6,'Out of', grepl=T , select_all=F)
```

## Labs

When inside a lab, you can use `nbs_lab_mark_as_reviewed()` to mark the
lab as reviewed, if possible. In the future, we can also add a function
which logs labs to be marked as reviewed in the Sandbox MAR table. This
would allow for bulk marking as reviewed via backend SQL scripts that
are scheduled to run 3x a day.

## Investigations

As mentioned previously, `nbs_investigation_go_to()` can be used to go
directly to an investigation. Once in the investigation, you can use
`nbs_page_metadata_get()` to import the metadata for the current
investigation. This metadata can be used to find the IDs for
`nbs_field_get()` and also passed as a parameter in order to speed up
the function. `nbs_field_get()` will return the current value, if any,
for a given field. This function only works from the ‘View
Investigation’ page, not the ‘Edit Investigation’ page, althought this
functionality may be added later.

To edit an investigation, you can use `nbs_investigation_edit()` to
enter the edit page. `nbs_field_set()` can be used to set the value of a
field from the investigation page. The page metadata can also be
provided as a parameter to speed up processing. `nbs_field_set()` can
check that the correct tab is selected before editing the field using
`check_tab=TRUE`. Organizing the order of edits to reduce unnecessary
tab checks/changes can greatly speed up scripts with lots of edits.
`nbs_field_set()` uses the page metadata to determine what type of field
is being edited, then calls the correct sub-function. When edits are
complete, use `nbs_investigation_submit()` to submit the changes.

``` r
# Load investigation
nbs_investigation_go_to('CAS12345678')

# Retrieve current county
old_county<-nbs_field_get('DEM165')

# Enter the edit page
nbs_investigation_edit()

# Set the county to Davidson
nbs_field_set('DEM165','Davidson County')

# Submit
nbs_investigation_submit()

# Retrieve new county
new_county<-nbs_field_get('DEM165')
```

## Future Work

- More convenience functions
- Add printing to Firefox
- Add checks after submit
- Increase functionality around labs/morbs/case reports.
- Direct patient search
