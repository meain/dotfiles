#!/bin/bash
BEFORE=diskutil info / | grep "Free Space"
echo $BEFORE
echo ''

echo 'brew cleanup'
brew cleanup

echo 'gem cleanup'
gem cleanup

echo 'rm -rf "${HOME}/Library/Caches/CocoaPods"'
rm -rf "${HOME}/Library/Caches/CocoaPods"

echo 'xcrun simctl delete unavailable'
xcrun simctl delete unavailable

echo 'xcode logs'
rm -rf ~/Library/Developer/Xcode/Archives
rm -rf ~/Library/Developer/Xcode/DerivedData
rm -rf ~~/Library/Developer/Xcode/iOS Device Logs/

echo 'docker volume prune'
docker volume prune


echo ''
echo 'Before'
echo $BEFORE
echo 'After'
diskutil info / | grep "Free Space"
