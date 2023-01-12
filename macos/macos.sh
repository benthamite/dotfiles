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

defaults write com.apple.PowerChime ChimeOnAllHardware -bool false;killall PowerChime
echo 'Disable the Chime Sound Effect on Power Cable Connect'

defaults write com.apple.systemsound “com.apple.sound.uiaudio.enabled” -int 0
echo 'Disable screenshot sound'

defaults write -g NSAutomaticWindowAnimationsEnabled -bool false
defaults write -g NSScrollAnimationEnabled -bool false
defaults write -g NSWindowResizeTime -float 0.001
defaults write -g QLPanelAnimationDuration -float 0
defaults write -g NSScrollViewRubberbanding -bool false
defaults write -g NSDocumentRevisionsWindowTransformAnimation -bool false
defaults write -g NSToolbarFullScreenAnimationDuration -float 0
defaults write -g NSBrowserColumnAnimationSpeedMultiplier -float 0
defaults write com.apple.dock autohide-time-modifier -float 0
defaults write com.apple.dock autohide-delay -float 0
defaults write com.apple.dock expose-animation-duration -float 0
defaults write com.apple.dock springboard-show-duration -float 0
defaults write com.apple.dock springboard-hide-duration -float 0
defaults write com.apple.dock springboard-page-duration -float 0
defaults write com.apple.finder DisableAllAnimations -bool true
defaults write com.apple.Mail DisableSendAnimations -bool true
defaults write com.apple.Mail DisableReplyAnimations -bool true
defaults write NSGlobalDomain NSUseAnimatedFocusRing -bool false
echo 'Disable animations'

defaults write -g NSAutomaticWindowAnimationsEnabled -bool false
echo 'Fast opening and closing windows and popovers'

# https://robservatory.com/speed-up-your-mac-via-hidden-prefs/
defaults write NSGlobalDomain NSWindowResizeTimes 0.001
echo 'Sped up dialogue boxes'

sudo spctl - master-disable
echo 'Allow apps downloaded from "Anywhere" allowed to be opened.'

defaults write com.apple.CrashReporter DialogType none
echo 'Do not ask to send crash reports'

sudo defaults write /Library/Preferences/com.apple.loginwindow AdminHostInfo HostName
echo 'Reveal IP address, hostname, OS version, etc. when clicking the clock in the login window'

launchctl unload -w /System/Library/LaunchAgents/com.apple.notificationcenterui.plist 2> /dev/null
echo 'Disable Notification Center and remove the menu bar icon'

defaults write NSGlobalDomain NSAutomaticCapitalizationEnabled -bool false
echo 'Disable automatic capitalization'

defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 70 '<dict><key>enabled</key><false/></dict>'
echo 'disable word definition'

defaults write NSGlobalDomain AppleKeyboardUIMode -int 3
echo 'Enable full keyboard access for all controls (e.g. enable Tab in modal dialogs)'

defaults write com.apple.universalaccess closeViewZoomFollowsFocus -bool true
echo 'Follow the keyboard focus while zoomed in'

# defaults write NSGlobalDomain ApplePressAndHoldEnabled -bool false
# echo 'Disable press-and-hold for keys in favor of key repeat'

# The step values that correspond to the sliders on the GUI are as follow (lower equals faster):
# KeyRepeat: 120, 90, 60, 30, 12, 6, 2
# InitialKeyRepeat: 120, 94, 68, 35, 25, 15
defaults write NSGlobalDomain KeyRepeat -int 2
defaults write NSGlobalDomain InitialKeyRepeat -int 15
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

launchctl unload -w /System/Library/LaunchAgents/com.apple.rcd.plist 2> /dev/null
echo 'Stop iTunes from responding to the keyboard media keys'

defaults write -g WebAutomaticTextReplacementEnableds -bool false
echo 'Disable macOS/iOS text expansion'

defaults write -g NSUserKeyEquivalents -dict-add "Emoji & Symbols" "\0"
echo 'Disable emoji panel shortcut'

defaults write com.apple.screencapture location -string "${HOME}/Downloads"
echo 'Save screenshots to the downloads folder'

defaults write com.apple.screencapture type -string "png"
echo 'Save screenshots in PNG format (other options: BMP, GIF, JPG, PDF, TIFF)'

defaults write com.apple.screencapture disable-shadow -bool true
echo 'Disable screenshot shadow effect'

defaults write com.apple.finder QuitMenuItem -bool true
echo 'In Finder, allow quitting via ⌘ + Q; doing so will also hide desktop icons'

defaults write com.apple.finder NewWindowTarget -string "PfLo"
defaults write com.apple.finder NewWindowTargetPath -string "file://${HOME}/"
echo 'Set Desktop as the default location for new Finder windows.'

# Show icons for hard drives, servers, and removable media on the desktop
defaults write com.apple.finder ShowExternalHardDrivesOnDesktop -bool true
defaults write com.apple.finder ShowHardDrivesOnDesktop -bool true
defaults write com.apple.finder ShowMountedServersOnDesktop -bool true
defaults write com.apple.finder ShowRemovableMediaOnDesktop -bool true

defaults write com.apple.finder AppleShowAllFiles -bool true
echo 'Finder: show hidden files by default'

defaults write NSGlobalDomain AppleShowAllExtensions -bool true
echo 'In Finder, show all filename extensions'

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

sudo chflags nohidden /Volumes
echo 'Show the /Volumes folder'

defaults write com.apple.finder FXInfoPanesExpanded -dict \
	General -bool true \
	OpenWith -bool true \
	Privileges -bool true
echo 'Expand the following File Info panes: “General”, “Open with”, and “Sharing & Permissions”'

defaults write com.apple.dock mouse-over-hilite-stack -bool true
echo 'Enable highlight hover effect for the grid view of a stack (Dock)'

defaults write com.apple.dock tilesize -int 36
echo 'Set the icon size of Dock items to 36 pixels'

defaults write com.apple.dock mineffect -string "scale"
echo 'Change minimize/maximize window effect'

defaults write com.apple.dock minimize-to-application -bool true
echo 'Minimize windows into their application’s icon'

defaults write com.apple.dock enable-spring-load-actions-on-all-items -bool true
echo 'Enable spring loading for all Dock items'

defaults write com.apple.dock show-process-indicators -bool true
echo 'Show indicator lights for open applications in the Dock'

defaults write com.apple.dock persistent-apps -array
echo 'Wipe all (default) app icons from the Dock'

defaults write com.apple.dock static-only -bool true
echo 'Show only open applications in the Dock'

defaults write com.apple.dock launchanim -bool false
echo 'Don’t animate opening applications from the Dock'

defaults write com.apple.dock expose-group-by-app -bool false
echo 'Don’t group windows by application in Mission Control'

defaults write com.apple.dashboard mcx-disabled -bool true
echo 'Disable Dashboard'

defaults write com.apple.dock dashboard-in-overlay -bool true
echo 'Don’t show Dashboard as a Space'

defaults write com.apple.dock mru-spaces -bool false
echo 'Don’t automatically rearrange Spaces based on most recent use'

defaults write com.apple.dock autohide-delay -float 0
echo 'Remove the auto-hiding Dock delay'

defaults write com.apple.dock autohide -bool true
echo 'Automatically hide and show the Dock'

defaults write com.apple.dock showhidden -bool true
echo 'Make Dock icons of hidden applications translucent'

defaults write com.apple.dock show-recents -bool false
echo 'Don’t show recent applications in Dock'

#defaults write com.apple.dock showLaunchpadGestureEnabled -int 0
echo 'Disable the Launchpad gesture (pinch with thumb and three fingers)'

find "${HOME}/Library/Application Support/Dock" -name "*-*.db" -maxdepth 1 -delete
echo 'Reset Launchpad, but keep the desktop wallpaper intact'

defaults write com.apple.TimeMachine DoNotOfferNewDisksForBackup -bool true
echo 'Prevent Time Machine from prompting to use new hard drives as backup volume'

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

defaults write com.google.Chrome PMPrintingExpandedStateForPrint2 -bool true
defaults write com.google.Chrome.canary PMPrintingExpandedStateForPrint2 -bool true
echo 'Expand the print dialog by default'

defaults write org.m0k.transmission WarningDonate -bool false
echo 'Hide Transmission app donate message'
