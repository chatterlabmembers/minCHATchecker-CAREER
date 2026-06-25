# OHS-minCHAT-Checker

Allows annotators to automatically check for basic minCHAT errors adapted for OHS transcriptions so that they can manually fix those errors and submit them.

**This script won't catch all the errors!!**

* It only catches errors as described below
* It might even catch some "errors" that are _in reality_ perfectly fine

It is up to you humans to fix those as needed! This is just a tool to help annotators to check the basic ACLEW Annotation Scheme and minCHAT standards used in their annotation files.

> [!WARNING]
> To properly check each file, you need to **(a) open the file in ELAN** and **(b) export a text version of it** for use with the checker.


## Instructions

1. Open the .eaf file you want to check in ELAN. Export the .eaf file as a tab-delimited text file (`File > Export As > Tab-delimited text`. When you do this, make sure that you are only selecting *speaker and their dependent tiers* and using the correct *time format* (ms)); see figure below.
    * ELAN will likely ask you to find the media (e.g., .wav) file. Select `Cancel` and proceed unless you want to listen to the media while checking for formatting errors. You can also add the media file back later by going to  `Edit > Linked Files...` and adding it there.
    * When you click to export the .txt file it will ask you about encoding: the default setting of `UTF-8` is the correct one for our tool.

2. Go to [OHS transcription error spotter](https://middycasillas.shinyapps.io/minCHATchecker-CAREER/).

3. Upload the .txt file and click `Submit`.

4. The checker generates a list of possible errors and warnings on capitalizations and hyphenations in one .csv file. To view the list of errors and warnings, download the spreadsheet for details. Remember, this took flags potential errors, so it is YOUR job to determine whether there are real errors!
    * For warnings on capitalizations and hyphenations, make sure they are stylized according to minCHAT standards.

### What does the OHS checker look for?

It checks to see whether...

* parent and dependent tier formatting
* there are too many or too few annotations
* there are empty annotations
* the closed-vocabulary annotation values in xds tiers are valid
* contiguous capitalized letters
* extra spaces
* punctuation usage and transcriptions have too few or too many terminal markers
* syntax of angle + square brackets is in the following pattern: **\<blabla\> [=! blabla]** (verbs must be in third-person singular tense)
* use of @ follows one of the following patterns: **blabla@s:eng**, **blabla@l**, or **blabla@c**
* use of underscore is sandwiched between letters
* use of **&=** or **\[: blabla]** is prohibited in the current workflow
* use of numerical digits in transcriptions

### What doesn't the OHS checker look for?

Here's a non-exhaustive list...

* spelling...anywhere
* xxx vs. yyy
* the proper use of capital letters of hyphenated words; but it does flag these for manual review
* the proper use of hyphens to indicate cut-off/restarted speech (e.g., he-, -in)
* matching speaker names across related tiers
* inner tier structure (i.e., correct hierarchical set-up; requires XML)
