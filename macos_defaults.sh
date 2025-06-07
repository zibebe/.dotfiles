#!/usr/bin/env sh

# deactivate Control+Cmd+d because it is used by emacs for `down-list`
defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 70 '<dict><key>enabled</key><false/></dict>'
