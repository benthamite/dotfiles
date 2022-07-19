-- Version 1.0, (C) Ergonis Software, 2012-01-19
-- Feel free to modify the script for your own use, but leave the copyright notice intact.

-- for testing in the AppleScript Editor
return expand("") -- all items in separate lines

on expand(separator)
	
	tell application "Finder"
		set theSelection to get selection as list
		set theResult to ""
		if length of separator = 0 then set separator to linefeed
		repeat with theItem in theSelection
			if length of theResult > 0 then set theResult to theResult & separator
			set theResult to theResult & POSIX path of (theItem as alias)
		end repeat
		return theResult
	end tell
	
end expand
