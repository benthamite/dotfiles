#!/bin/sh
printf "%s - checking github tlon-team/pass repo for remote changes\n" "$(date "+%F %T")"
cd "/Users/$USER/.password-store/tlon"
git pull
