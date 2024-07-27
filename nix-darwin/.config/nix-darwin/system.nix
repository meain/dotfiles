{
  NSGlobalDomain = {
    "com.apple.mouse.tapBehavior" = 1;

    AppleShowScrollBars = "Automatic"; # show scroll bars for mouse
    AppleScrollerPagingBehavior = true; # jump to the click spot
    AppleShowAllExtensions = false; # do not show all file extensions (gets ugly in spotlight)

    KeyRepeat = 2; # fast key repeat
    InitialKeyRepeat = 20; # delay before key repeat

    NSAutomaticCapitalizationEnabled = false; # disable auto capitalization
    NSAutomaticDashSubstitutionEnabled = false; # disable auto dash substitution
    NSAutomaticPeriodSubstitutionEnabled = false; # disable auto period substitution
    NSAutomaticQuoteSubstitutionEnabled = false; # disable auto quote substitution
    NSAutomaticSpellingCorrectionEnabled = false; # disable auto spelling correction

    NSAutomaticWindowAnimationsEnabled = true; # enable window animations
    NSDisableAutomaticTermination = false; # enable automatic termination
    NSDocumentSaveNewDocumentsToCloud = false; # save to disk by default

    NSTableViewDefaultSizeMode = 2; # medium row size
    NSWindowResizeTime = 0.001; # fastest window resize
    ApplePressAndHoldEnabled = false; # disable press and hold (for accents)

    "com.apple.swipescrolldirection" = false;
  };

  dock = {
    autohide = true; # autohide dock
    mru-spaces = false; # do not rearrange spaces based on most recent use
    orientation = "right"; # dock on right side
    show-recents = false; # do not show recently closed apps
    tilesize = 20; # smal icon size
    magnification = true; # magnify icons on hover

    # set hot corners
    wvous-tl-corner = 2;
    wvous-tr-corner = 2;
    wvous-bl-corner = 1;
    wvous-br-corner = 12;

    persistent-apps = [ ];
    # persistent-others = ["~/Desktop" "~/Downloads"];
  };

  finder = {
    # AppleShowAllFiles = true; # show hidden files
    CreateDesktop = false; # do not show icons on desktop
    FXDefaultSearchScope = "SCcf"; # search current folder by default
    ShowPathbar = true; # show path bar
    ShowStatusBar = true; # show status bar
    FXEnableExtensionChangeWarning = false; # do not warn when changing file extensions
    FXPreferredViewStyle = "clmv"; # column view
  };

  menuExtraClock = {
    ShowDate = 1; # show date in menu bar
    ShowSeconds = true; # show seconds in menu bar
  };

  screencapture = {
    location = "~/Documents/Screenshots"; # will have to create it manually
  };

  universalaccess = {
    closeViewScrollWheelToggle = true; # enable zoom with scroll wheel
    reduceMotion = true; # disable animations
    reduceTransparency = true; # disable transparency
  };
}
