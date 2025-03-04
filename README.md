# wiNBS

## Working in NBS (wiNBS)

This package is facilitates the creation of RSelenium-based scripts to
control web browsers. It has a particular focus at this time on NBS, but
could be extended to other surveillance systems. This package is built
upon `RSelenium`. Refer to [the official
documentation](https://cran.r-project.org/web/packages/RSelenium/vignettes/basics.html)
for more guidance.

### Chromedriver fix

Automated downloads of new chromedrivers seems to be failing unless you
install the `wdman` package from a patched repository using the code
below.

``` r
devtools::install_github('ashbythorpe/wdman', ref='chromefix')
```

After downloading, you will need to edit the folder with your
chromedrivers. My chromedrivers are stored here:
`C:\Users\dcnumber\AppData\Local\binman\binman_chromedriver`. Find your
equivalent folder. Inside, you will find a ‘win32’ and/or ‘win64’
subfolder. Inside these are your chromedriver versions. In order to make
sure future downloads run automatically, do the following:

1.  Create a ‘win64’ folder if it does not exist.
2.  Delete the ‘win32’ folder.

## Workflow

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
environment. Once you set a password, you can update it by rerunning
`nbs_password_set()` or by going to your credential manager in Windows.

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
dropdown. `nbs_search()` will take you to the patient landing page. To
go to an investigation directly, use `nbs_investigation_go_to()`. This
function will go directly to an investigation from anywhere in NBS
(using the search functionality if needed). To search for a patient by
patient ID, use `nbs_patient_search()`.

``` r
# Go to patient with investigation CAS12345678
nbs_search('CAS12345678')

# Go to patient with lab OBS12345678
nbs_search('OBS12345678', ID_type = 'Lab ID')

# Go to investigation CAS12345678
nbs_investigation_go_to('CAS12345678')

nbs_patient_search('12345678')
```

## Queues

You can load a queue using `nbs_queue_load('queue name')`. Once inside
the queue, you can use `nbs_queue_filter()` to activate the filters for
any column that uses check boxes as options. By default, the text found
in `search_for` is used to find an exact match in the UI options. You
may set `grepl=TRUE` to employ pattern matching instead. Note that the
`select_all` parameter affects if the ‘Select All’ option gets clicked
prior to selection of other options. ‘Select All’ is selected by default
in the UI, and `select_all` is `FALSE` by default, so you must set
`select_all` to `TRUE` in order to select only the option(s) specified
in `search_for`.

``` r
# Go to DRR
nbs_queue_load('Document Requiring Review')

# For column 1 (document type), uncheck 'Select All', then select only the 'Lab Report' option
nbs_queue_filter(1,'Lab Report', select_all=T)

# For column 5 (description), uncheck 'Select All', then select labs with 'COV' in the description
nbs_queue_filter(5,'COV', grepl=T , select_all=T)

# For column 6 (jurisdiction), deselect labs that are 'Out of State' or 'Out of System'
nbs_queue_filter(6,'Out of', grepl=T , select_all=F)

# Get info on the 5th row shown
nbs_queue_row_info(5)

# Click into the first row shown
nbs_queue_row_click()

# Go back to the queue from inside a lab/case
nbs_queue_return()
```

## Reports

You can run a report using `nbs_report()`. This will go to the report
page and enter basic filters and column selections before exporting the
report (to the default location the browser is downloading to).

To set the basic filters, use the `basic` parameter with a named list of
elements to change and values to set them to. To enter text to a field,
use `'elementID'='some_text'`. To click an element, use
`'elementID'=NA`. To select dropdown options from an element, use
`'elementID'=c('option1','option2','option3')`. To select a single value
from a dropdown, you must either provide the value 3 times (the first 2
cancel each other out), or provide an additional fake value (which will
generate a warning message). To keep the default basic filters, set
`basic=list()`.

To select columns, provide a vector to `columns`, where a numeric vector
will select by location in the list, and a string vector will match on
title. Leaving the `columns` parameter as `NA` will select all columns,
and setting it to “” will select no columns (leaving the default
selection in place). Note that not all reports allow column selection.

The example below will select Covid-19 as the condition of interest,
click the “Select All” checkbox for counties to include, and set the
“from” and “to” fields to include previous month’s data.

``` r
# Selects all columns
nbs_report('Custom Report for Disease Counts by County'
           , basic=list('id_C_D01'= c('COVID-19','COVID-19','COVID-19')
                    , 'id_county_select_all'= NA
                    , 'id_T_T01a'= Sys.Date()-30
                    , 'id_T_T01b'= Sys.Date())
           )

# Selects age, city, and ID columns
nbs_report('Custom Report for Disease Counts by County'
           , basic = list('id_C_D01'= c('COVID-19','COVID-19','COVID-19')
                    , 'id_county_select_all'= NA
                    , 'id_T_T01a'= Sys.Date()-30
                    , 'id_T_T01b'= Sys.Date())
           , columns = c('Age Reported', 'City', 'Investigation ID')
           )
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
Investigation’ page, not the ‘Edit Investigation’ page, although this
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
complete, use `nbs_investigation_submit()` to submit the changes. This
function does return text from the global message status bar (green if
submission is successful, red if there are errors).

``` r
# Load investigation
nbs_investigation_go_to('CAS12345678')

# Retrieve current county
old_county<-nbs_field_get('DEM165')

# Enter the edit page
nbs_investigation_edit()

# Set the county to Davidson
nbs_field_set('DEM165','Davidson County')

# Submit, log submission status
df$submission[i]<-nbs_investigation_submit()

# Retrieve new county
new_county<-nbs_field_get('DEM165')
```

## User Management

**For admin use only.** You can use the `nbs_user_*` family of functions
to perform basic user management navigation and edits. Go to a user
profile by using `nbs_user_management('dc1234567','view')` or
`nbs_user_management('dc1234567','edit')` depending on if you want to
view or edit the info. On the edit page, you can use
`nbs_user_set_status('Inactive')` or `nbs_user_set_status('Active')` to
change the user status, then use `nbs_user_submit()` to save the
changes.

``` r
# Inactivate a list of users
users<-c('dc11111','dc22222','dc33333')
for (u in users){
nbs_user_management(u,'edit')
nbs_user_set_status('Inactive')
nbs_user_submit()
}
```

## Bulk updates

Editing fields in investigations in bulk is a common task.
`nbs_bulk_updates()` streamlines this by applying boiler-plate code to
specified data and processing algorithms. This function contains useful
handler code for:

1.  Loop infrastructure
2.  Metadata specification
3.  Error handling and re-login
4.  Pre- and post-checks of data
5.  Skipping submission of open cases
6.  Time logging
7.  Printing progress summaries
8.  Logging data at intervals

### Bulk Setup

Below is an example of some data that needs to be updated. The data has
the local ID for the investigations that need to be edited, the
investigation start date (which will not be used for anything, but is
just hanging out), and the PID column (which has data that we need to
update in NBS).

| inv_local_id | inv_start_date | PID     |
|:-------------|:---------------|:--------|
| CAS11111111  | 1/1/2024       | Yes     |
| CAS22222222  | 1/2/2024       | No      |
| CAS33333333  | 1/3/2024       | No      |
| CAS44444444  | 1/4/2024       | Unknown |

To process these cases, we need to create an algorithm that will specify
which fields in NBS need to be edited and how. To create an algorithm,
you can use `nbs_bulk_template('algorithm.csv')` to create a blank
template in the working directory. Below is an example of a filled out
template:

| Group     | Field               | Tab             | ID     | Overwrite | Value_type | Value                         |
|:----------|:--------------------|:----------------|:-------|:----------|:-----------|:------------------------------|
| Case Info | Confirmation Method | Case Info       | INV161 | FALSE     | String     | Laboratory Confirmed          |
| Case Info | Case Status         | Case Info       | INV163 | FALSE     | String     | Confirmed                     |
| Case Info | PID                 | Case Info       | INV179 | TRUE      | R Code     | data\$PID\[i\]                |
| Closure   | Date Closed         | Case Management | NBS196 | TRUE      | R Code     | format(Sys.Date(),‘%m/%d/%Y’) |
| Closure   | Closed by           | Case Management | NBS197 | TRUE      | Quick Code | nbs-bot                       |

This algorithm specifies that for each of the 4 investigations, we will
make 5 changes. In this example, “Confirmation Method” and “Case Status”
are set to not overwrite existing values, so if NBS already has data for
those fields, nothing will change. The ID column must have the question
ID pulled from the page metadata (see `nbs_page_metadata_get()` and
`page_metadata`). Value_type and Value determine what the NBS field gets
changed to. If the Value_type is “String”, then the field will be set to
the exact test in Value. If Value_type is “Quick Code”, the same thing
happens, but quick codes behave differently in NBS and we need to denote
that so things process correctly. If the Value_type is “R Code”, the
code in Value is parsed before entry. In the above example, the PID
field in NBS will be set to the value from the PID column in the data,
so each investigation can have a different response there (unlike “Case
Status” which is set to turn every investigation to “Confirmed”). Note
that we need to index the column using `[i]` because `i` is the value
that the function uses to iterate over each row of the data. Similarly,
“Date Closed” is also set using R code, but we want the value to just be
the date the code was run, so we don’t need to use `i`.

Here is a table explaining each column in the algorithm template:

| Column     | Required    | Options                                 | Description                                                                                                                                                                                                                                 |
|:-----------|:------------|:----------------------------------------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Group      | No          | Free text                               | Just there to help visually group things when looking at the csv. No code impact.                                                                                                                                                           |
| Field      | Recommended | Free text                               | The field description. Used when printing mismatches, but no real code impact.                                                                                                                                                              |
| Tab        | Recommended | Free text                               | Used to determine when to switch tabs. Rows with a tab different from the row above will prompt a tab switch. Grouping elements on the same tab together speeds up edits.                                                                   |
| ID         | Yes         | Question IDs from metadata              | ID pull from the page metadata ID for a field                                                                                                                                                                                               |
| Overwrite  | Yes         | T/F                                     | Should existing data be overwritten?                                                                                                                                                                                                        |
| Value_type | Yes         | One of “String”, “R Code”, “Quick Code” | If “String” or “Quick Code”, then the value column will be put into the field (quick codes behave a little differently in the code though). If “R Code”, the value column will be parsed and evaluated to determine what goes in the field. |
| Value      | Yes         | String, R code, or a quick code         | A string or R code snippet to determine what goes into the field. Interpreted according to Value_type column.                                                                                                                               |

You can run `nbs_bulk_updates()` with no algorithm if you only want to
edit and resubmit investigations without making any changes (which oddly
enough can be useful). A summary of all parameters is below.

| parameter    | Required    | Options                                                                      | Default                      | Description                                                                                                                                                                                                             |
|:-------------|:------------|:-----------------------------------------------------------------------------|:-----------------------------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| data         | Yes         | A dataframe of cases, or a csv filename to such data                         |                              | Each row of this data should be a case that needs editing                                                                                                                                                               |
| algorithm    | No          | NA, an algorithm dataframe , or a csv filename to and algorithm              | NA                           | If NA, the case will be edited and submitted with no changes                                                                                                                                                            |
| metadata     | No          | NA, a dataframe of metadata, or a page name                                  | NA                           | If NA, the metadata will be pulled after searching for each investigation. This is slower than specifying the metadata via the parameter, but allows for multipl page types to be edited in the same run.               |
| username     | Recommended | NA, your username                                                            | NA                           | If this is specified, the code will be able to log back in to NBS after encountering an error (to continue processing the rest of the data).                                                                            |
| environment  | Recommended | NBS Production’, NBS Staging’, etc.                                          | NBS Production               | The NBS environment the edits will occur in                                                                                                                                                                             |
| cancel_open  | No          | T/F                                                                          | F                            | After all edits have been made, the investigation status is noted. If cancel_open=T, cases that are open will not be submitted.                                                                                         |
| id_col       | Recommended | NA, column name for ID from data                                             | NA                           | If NA, the code will try to detect the correct column in the data to use as the local ID.                                                                                                                               |
| uid_col      | No          | NA, column name for UID from data                                            | NA                           | If present, this column will be used to navigate to the investigation page.                                                                                                                                             |
| log_file     | Recommended | Name for the csv output                                                      | NA                           | The function returns the dataframe, but logging the output is recommended for realiability.                                                                                                                             |
| log_every    | No          | Number                                                                       | 100                          | Logging and summary printing will occur at multiples of this value.                                                                                                                                                     |
| message_vars | No          | Column names from the original data file, or those created during processing | status, mismatches, and time | After each row has finished processing, these columns will be printed to console as a visual check. In addition to data columns, you can specify columns from the output file such as “is_closed”, or “correct_INV179”. |

### Usage

Here is an example of code to update the data referenced above. All the
cases are the same condition, so we can specify the metadata and
increase the speed of the edits. I also specify that any case that is
open should not be edited, and provide file names and my username.

``` r
nbs_bulk_updates(  data='PID_updates.csv'
                 , algorithm='PID_alg.csv'
                 , metadata='PD_STD_Investigation'
                 , username='dc123456'
                 , cancel_open=T
                 , log_file='PID_log.csv'
                 )
```

After the edits complete, the log file will look something like this
(some field columns excluded for brevity):

| inv_local_id | inv_start_date | PID     | old_INV161           | new_INV161           | check_INV161         | correct_INV161 | old_INV179 | new_INV179 | check_INV179 | correct_INV179 | status                                                         | mismatches | time          | is_closed |
|:-------------|:---------------|:--------|:---------------------|:---------------------|:---------------------|:---------------|:-----------|:-----------|:-------------|:---------------|:---------------------------------------------------------------|-----------:|:--------------|:----------|
| CAS11111111  | 1/1/2024       | Yes     | Clinical             | Laboratory Confirmed | Laboratory Confirmed | TRUE           | Unknown    | Yes        | Yes          | TRUE           | Investigation has been successfully saved in the system.       |          0 | 20.23 seconds | TRUE      |
| CAS22222222  | 1/2/2024       | No      | Laboratory Confirmed | Laboratory Confirmed | Laboratory Confirmed | TRUE           | No         | No         | No           | TRUE           | Please fix the following errors: Dispositioned By is required. |          0 | 21.32 seconds | TRUE      |
| CAS33333333  | 1/3/2024       | No      |                      | Laboratory Confirmed |                      | FALSE          | Unknown    | No         | Unknown      | FALSE          | Investigation has been successfully saved in the system.       |          2 | 19.12 seconds | TRUE      |
| CAS44444444  | 1/4/2024       | Unknown | Laboratory Confirmed | Clinical             |                      | NA             | Unknown    | Unknown    |              | NA             |                                                                |         NA | 22 seconds    | FALSE     |

The first 3 columns are the originals from the data. The rest of the
columns are created by the script. Each field from the algorithm is
given 4 associated columns, then some other log columns are included at
the end. These example results can be interpreted as follows:

1.  CAS11111111’s edits worked perfectly
2.  CAS22222222’s edits failed to submit because the case had a missing
    required field.
3.  CAS33333333 was submitted, but the values did not update correctly
    for the 2 shown fields.
4.  CAS44444444 was had an investigation status of ‘Open’, so was not
    submitted. The “Confirmation Method” is wrong, but “PID” is already
    the correct value.

The table below breaks down how to interpret each set of columns.

| Group             | Columns               | Examples                          | Description                                                                    |
|:------------------|:----------------------|:----------------------------------|:-------------------------------------------------------------------------------|
| Original data     | Original data columns | inv_local_id, inv_start_date, PID | Everything in the original data.frame is included                              |
| Field checks      | old\_\[field ID\]     | old_INV161, old_INV179            | Values of the fields before the edit button is clicked                         |
| Field checks      | new\_\[field ID\]     | new_INV161, new_INV179            | Values desired upon submission                                                 |
| Field checks      | check\_\[field ID\]   | check_INV161, check_INV179        | Values after submission (blank if not submitted)                               |
| Field checks      | correct\_\[field ID\] | correct_INV161, correct_INV179    | T/F if new\_ and check\_ match (blank if not submitted)                        |
| Submission Checks | status                | status                            | Text from the feedback bar upon submission (blank if not submitted)            |
| Submission Checks | mismatch              | mismatch                          | Number of fields where new\_ does not equal check\_ (blank if not submitted)   |
| Submission Checks | time                  | time                              | How long did that row take to process?                                         |
| Submission Checks | is_closed             | is_closed                         | Was the case closed prior after edits were made and before submit was clicked? |

## Future Work

- More convenience functions
- Increase functionality around labs/morbs/case reports.
