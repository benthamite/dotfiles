-- Version 1.2, (C) Ergonis Software, 2020-07-07
-- Feel free to modify the script for your own use, but leave the copyright notice intact.
-- V1.0 (2012-01-19): Original version with http://automation.whatismyip.com/n09230945.asp
-- V1.1 (2016-05-03): Changed to http://icanhazip.com
-- V1.2 (2020-07-07): The script now correctly uses icanhazip.com.

-- The following public services return a short reply that contains just the public IP address:
--		http://icanhazip.com
--		http://ifconfig.me/ip

-- For the future (after 1.2):
--		http://whatismyip.akamai.com
--		https://ipecho.net/plain
--		http://ident.me

-- See also:
--		https://gtalug.org/pipermail/talk/2018-January/005815.html
--		https://raw.githubusercontent.com/mattst/fetchip/master/source_urls_list


try
	set curlCMD to Â
		"curl --stderr /dev/null \"http://icanhazip.com\""
	set myIP to (do shell script curlCMD)
	return myIP
on error (msg)
	return msg
end try
