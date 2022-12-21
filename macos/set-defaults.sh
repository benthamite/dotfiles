#!/bin/sh

# Set my preferred macOS defaults.
# Inspired by: https://github.com/nikitavoloboev/dotfiles/blob/master/macos/set-defaults.sh
# See also: https://github.com/mathiasbynens/dotfiles
# and: https://gist.github.com/brandonb927/3195465

# If not ran on macOS, exit
if [ "$(uname -s)" != "Darwin" ]; then
	exit 0
fi

set +e

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
defaults write NSGlobalDomain KeyRepeat -int 1

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

echo 'Some commands here require restart! Please do that for them to take effect.'

