-- Version 1.0, (C) Ergonis Software, 2012-01-19
-- Feel free to modify the script for your own use, but leave the copyright notice intact.

tell application "Finder"
	try
		return POSIX path of (target of first Finder window as alias)
	on error
		return POSIX path of (desktop as alias)
	end try
end tell
