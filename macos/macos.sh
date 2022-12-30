#!/usr/bin/env bash

# ~/.macos — https://mths.be/macos

# Close any open System Preferences panes, to prevent them from overriding
# settings we’re about to change
osascript -e 'tell application "System Preferences" to quit'

# Ask for the administrator password upfront
sudo -v

# Keep-alive: update existing `sudo` time stamp until `.macos` has finished
while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done 2>/dev/null &

sudo nvram SystemAudioVolume=" "
echo 'Disable the sound effects on boot'

defaults write NSGlobalDomain NSUseAnimatedFocusRing -bool false
echo 'Disable the over-the-top focus ring animation'

defaults write com.apple.LaunchServices LSQuarantine -bool false
echo 'Disable the “Are you sure you want to open this application?” dialog'

sudo defaults write /Library/Preferences/com.apple.loginwindow AdminHostInfo HostName
echo 'Reveal IP address, hostname, OS version, etc. when clicking the clock in the login window'

launchctl unload -w /System/Library/LaunchAgents/com.apple.notificationcenterui.plist 2> /dev/null
echo 'Disable Notification Center and remove the menu bar icon'

defaults write NSGlobalDomain NSAutomaticCapitalizationEnabled -bool false
echo 'Disable automatic capitalization'

defaults write NSGlobalDomain AppleKeyboardUIMode -int 3
echo 'Enable full keyboard access for all controls (e.g. enable Tab in modal dialogs)'

defaults write com.apple.universalaccess closeViewZoomFollowsFocus -bool true
echo 'Follow the keyboard focus while zoomed in'

# defaults write NSGlobalDomain ApplePressAndHoldEnabled -bool false
# echo 'Disable press-and-hold for keys in favor of key repeat'

defaults write NSGlobalDomain KeyRepeat -int 1
defaults write NSGlobalDomain InitialKeyRepeat -int 10
echo 'Set a blazingly fast keyboard repeat rate'

defaults write NSGlobalDomain AppleLanguages -array "en" "nl"
defaults write NSGlobalDomain AppleLocale -string "en_GB@currency=EUR"
defaults write NSGlobalDomain AppleMeasurementUnits -string "Centimeters"
defaults write NSGlobalDomain AppleMetricUnits -bool true
echo 'Set language and text formats'

sudo systemsetup -settimezone " America/Mexico_City" > /dev/null
echo 'Set the timezone; see `sudo systemsetup -listtimezones` for other values'
# America/Argentina/Buenos_Aires
# Europe/London
# Europe/Madrid

#launchctl unload -w /System/Library/LaunchAgents/com.apple.rcd.plist 2> /dev/null
echo 'Stop iTunes from responding to the keyboard media keys'

defaults write com.apple.screencapture location -string "${HOME}/Downloads"
echo 'Save screenshots to the downloads folder'

defaults write com.apple.screencapture type -string "png"
echo 'Save screenshots in PNG format (other options: BMP, GIF, JPG, PDF, TIFF)'

defaults write com.apple.screencapture disable-shadow -bool true
echo 'Disable shadow in screenshots'

defaults write com.apple.finder QuitMenuItem -bool true
echo 'Finder: allow quitting via ⌘ + Q; doing so will also hide desktop icons'

defaults write com.apple.finder DisableAllAnimations -bool true
echo 'Finder: disable window animations and Get Info animations'

defaults write com.apple.finder NewWindowTarget -string "PfLo"
defaults write com.apple.finder NewWindowTargetPath -string "file://${HOME}/"
echo 'Set Desktop as the default location for new Finder windows.'

# Show icons for hard drives, servers, and removable media on the desktop
defaults write com.apple.finder ShowExternalHardDrivesOnDesktop -bool true
defaults write com.apple.finder ShowHardDrivesOnDesktop -bool true
defaults write com.apple.finder ShowMountedServersOnDesktop -bool true
defaults write com.apple.finder ShowRemovableMediaOnDesktop -bool true

#defaults write com.apple.finder AppleShowAllFiles -bool true
# echo 'Finder: show hidden files by default'

defaults write NSGlobalDomain AppleShowAllExtensions -bool true
echo 'Finder: show all filename extensions'

defaults write com.apple.finder _FXShowPosixPathInTitle -bool true
echo 'Display full POSIX path as Finder window title'

defaults write com.apple.finder _FXSortFoldersFirst -bool true
echo 'Keep folders on top when sorting by name'

defaults write com.apple.finder FXDefaultSearchScope -string "SCcf"
echo 'When performing a search, search the current folder by default'

defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false
echo 'Disable the warning when changing a file extension'

defaults write NSGlobalDomain com.apple.springing.enabled -bool true
echo 'Enable spring loading for directories'

defaults write NSGlobalDomain com.apple.springing.delay -float 0
echo 'Remove the spring loading delay for directories'

defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true
defaults write com.apple.desktopservices DSDontWriteUSBStores -bool true
echo 'Avoid creating .DS_Store files on network or USB volumes'

# Show item info near icons on the desktop and in other icon views
/usr/libexec/PlistBuddy -c "Set :DesktopViewSettings:IconViewSettings:showItemInfo true" ~/Library/Preferences/com.apple.finder.plist
/usr/libexec/PlistBuddy -c "Set :FK_StandardViewSettings:IconViewSettings:showItemInfo true" ~/Library/Preferences/com.apple.finder.plist
/usr/libexec/PlistBuddy -c "Set :StandardViewSettings:IconViewSettings:showItemInfo true" ~/Library/Preferences/com.apple.finder.plist

# Show item info to the right of the icons on the desktop
/usr/libexec/PlistBuddy -c "Set DesktopViewSettings:IconViewSettings:labelOnBottom false" ~/Library/Preferences/com.apple.finder.plist

# Enable snap-to-grid for icons on the desktop and in other icon views
/usr/libexec/PlistBuddy -c "Set :DesktopViewSettings:IconViewSettings:arrangeBy grid" ~/Library/Preferences/com.apple.finder.plist
/usr/libexec/PlistBuddy -c "Set :FK_StandardViewSettings:IconViewSettings:arrangeBy grid" ~/Library/Preferences/com.apple.finder.plist
/usr/libexec/PlistBuddy -c "Set :StandardViewSettings:IconViewSettings:arrangeBy grid" ~/Library/Preferences/com.apple.finder.plist

# Increase grid spacing for icons on the desktop and in other icon views
/usr/libexec/PlistBuddy -c "Set :DesktopViewSettings:IconViewSettings:gridSpacing 100" ~/Library/Preferences/com.apple.finder.plist
/usr/libexec/PlistBuddy -c "Set :FK_StandardViewSettings:IconViewSettings:gridSpacing 100" ~/Library/Preferences/com.apple.finder.plist
/usr/libexec/PlistBuddy -c "Set :StandardViewSettings:IconViewSettings:gridSpacing 100" ~/Library/Preferences/com.apple.finder.plist

# Increase the size of icons on the desktop and in other icon views
/usr/libexec/PlistBuddy -c "Set :DesktopViewSettings:IconViewSettings:iconSize 80" ~/Library/Preferences/com.apple.finder.plist
/usr/libexec/PlistBuddy -c "Set :FK_StandardViewSettings:IconViewSettings:iconSize 80" ~/Library/Preferences/com.apple.finder.plist
/usr/libexec/PlistBuddy -c "Set :StandardViewSettings:IconViewSettings:iconSize 80" ~/Library/Preferences/com.apple.finder.plist

defaults write com.apple.finder FXPreferredViewStyle -string "Nlsv"
echo 'Use list view in all Finder windows by default'

defaults write com.apple.finder WarnOnEmptyTrash -bool false
echo 'Disable the warning before emptying the Trash'

chflags nohidden ~/Library && xattr -d com.apple.FinderInfo ~/Library
echo 'Show the ~/Library folder'

sudo chflags nohidden /Volumes
echo 'Show the /Volumes folder'

defaults write com.apple.finder FXInfoPanesExpanded -dict \
	General -bool true \
	OpenWith -bool true \
	Privileges -bool true
echo 'Expand the following File Info panes: “General”, “Open with”, and “Sharing & Permissions”'

defaults write com.apple.TimeMachine DoNotOfferNewDisksForBackup -bool true
echo 'Prevent Time Machine from prompting to use new hard drives as backup volume'

hash tmutil &> /dev/null && sudo tmutil disablelocal
echo 'Disable local Time Machine backups'

defaults write com.apple.ActivityMonitor OpenMainWindow -bool true
echo 'Show the main window when launching Activity Monitor'

defaults write com.apple.ActivityMonitor IconType -int 5
echo 'Visualize CPU usage in the Activity Monitor Dock icon'

defaults write com.apple.ActivityMonitor ShowCategory -int 0
echo 'Show all processes in Activity Monitor'

defaults write com.apple.ActivityMonitor SortColumn -string "CPUUsage"
defaults write com.apple.ActivityMonitor SortDirection -int 0
echo 'Sort Activity Monitor results by CPU usage'

defaults write com.apple.TextEdit RichText -int 0
echo 'Use plain text mode for new TextEdit documents'

defaults write com.apple.TextEdit PlainTextEncoding -int 4
defaults write com.apple.TextEdit PlainTextEncodingForWrite -int 4
echo 'Open and save files as UTF-8 in TextEdit'

defaults write com.apple.DiskUtility DUDebugMenuEnabled -bool true
defaults write com.apple.DiskUtility advanced-image-options -bool true
echo 'Enable the debug menu in Disk Utility'

defaults write com.apple.appstore WebKitDeveloperExtras -bool true
echo 'Enable the WebKit Developer Tools in the Mac App Store'

defaults write com.apple.appstore ShowDebugMenu -bool true
echo 'Enable Debug Menu in the Mac App Store'

defaults write com.apple.SoftwareUpdate AutomaticCheckEnabled -bool true
echo 'Enable the automatic update check'

defaults write com.apple.SoftwareUpdate ScheduleFrequency -int 1
echo 'Check for software updates daily, not just once per week'

defaults write com.apple.SoftwareUpdate AutomaticDownload -int 1
echo 'Download newly available updates in background'

defaults write com.apple.SoftwareUpdate CriticalUpdateInstall -int 1
echo 'Install System data files & security updates'

defaults write com.apple.commerce AutoUpdate -bool true
echo 'Turn on app auto-update'

defaults write com.apple.commerce AutoUpdateRestartRequired -bool true
echo 'Allow the App Store to reboot machine on macOS updates'

defaults -currentHost write com.apple.ImageCapture disableHotPlug -bool true
echo 'Prevent Photos from opening automatically when devices are plugged in'

defaults write com.google.Chrome DisablePrintPreview -bool true
defaults write com.google.Chrome.canary DisablePrintPreview -bool true
echo 'Use the system-native print preview dialog'

defaults write com.google.Chrome PMPrintingExpandedStateForPrint2 -bool true
defaults write com.google.Chrome.canary PMPrintingExpandedStateForPrint2 -bool true
echo 'Expand the print dialog by default'

for app in "Activity Monitor" \
	"Address Book" \
	"Calendar" \
	"cfprefsd" \
	"Contacts" \
	"Dock" \
	"Finder" \
	"Google Chrome Canary" \
	"Google Chrome" \
	"Mail" \
	"Messages" \
	"Opera" \
	"Photos" \
	"Safari" \
	"SizeUp" \
	"Spectacle" \
	"SystemUIServer" \
	"Terminal" \
	"Transmission" \
	"Tweetbot" \
	"Twitter" \
	"iCal"; do
	killall "${app}" &> /dev/null
done
echo "Done. Note that some of these changes require a logout/restart to take effect."
