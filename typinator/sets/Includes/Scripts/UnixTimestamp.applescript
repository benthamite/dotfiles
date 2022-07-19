-- Version 1.0, (C) Ergonis Software, 2012-01-19
-- Feel free to modify the script for your own use, but leave the copyright notice intact.

-- current date as Unix time stamp
set baseDate to current date
set day of baseDate to 1
set month of baseDate to 1
set year of baseDate to 1970
set time of baseDate to 0
return ((current date) - baseDate - (time to GMT)) as miles as string
