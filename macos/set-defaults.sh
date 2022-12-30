#!/usr/bin/env bash

# ~/.macos — https://mths.be/macos

# Close any open System Preferences panes, to prevent them from overriding
# settings we’re about to change
osascript -e 'tell application "System Preferences" to quit'

# Ask for the administrator password upfront
sudo -v

# Keep-alive: update existing `sudo` time stamp until `.macos` has finished
while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done 2>/dev/null &

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
echo 'Disable animations'

defaults write com.apple.PowerChime ChimeOnNoHardware -bool true && killall PowerChime
echo 'disable power chime'

defaults write com.apple.finder QuitMenuItem -bool true
killall Finder
echo 'enable quit option on Finder'

defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 70 '<dict><key>enabled</key><false/></dict>'
echo 'disable word definition'

# https://www.howtogeek.com/267463/how-to-enable-key-repeating-in-macos.
defaults write -g ApplePressAndHoldEnabled -bool false
echo 'Fast key repeat. Requires restart.'

# https://twitter.com/jordwalke/status/1230582824224165888 fast repeat.
defaults write NSGlobalDomain KeyRepeat -int 0.02
echo 'Set a blazingly fast keyboard repeat rate'

echo "Allow quitting Finder via ⌘ + Q; doing so will also hide desktop icons"
defaults write com.apple.finder QuitMenuItem -bool true

echo "Disable the “Are you sure you want to open this application?” dialog"
defaults write com.apple.LaunchServices LSQuarantine -bool false

echo "Disable the warning when changing a file extension"
defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false

defaults write -g NSAutomaticWindowAnimationsEnabled -bool false
echo 'Fast opening and closing windows and popovers'

# https://robservatory.com/speed-up-your-mac-via-hidden-prefs/
defaults write NSGlobalDomain NSWindowResizeTime 0.001
echo 'Sped up dialogue boxes'

defaults write org.m0k.transmission WarningDonate -bool false
echo 'Hide Transmission app donate message'

defaults write -g WebAutomaticTextReplacementEnabled -bool false
echo 'Disable macOS/iOS text expansion'

# https://apple.stackexchange.com/a/270625
defaults write com.apple.CrashReporter DialogType none
echo 'Do not ask to send crash reports'

defaults write com.apple.screencapture type -string "png"
echo 'Save screenshots in PNG format (other options: BMP, GIF, JPG, PDF, TIFF)'

defaults write com.apple.dock autohide-delay -float 0
echo 'Remove the auto-hiding Dock delay'

defaults write com.apple.dock autohide-time-modifier -float 0
echo 'Remove the animation when hiding/showing the Dock'

defaults write com.apple.dock autohide -bool true
echo 'Automatically hide and show the Dock'


