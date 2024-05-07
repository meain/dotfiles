{ pkgs, ppkgs, firefox-addons, defaultFont, rbm }:
{
  enable = true;
  package = ppkgs.firefox-darwin;
  profiles.meain = {
    search = {
      default = "DuckDuckGo";
      force = true;
      engines = {
        # don't need these default ones
        "Amazon.com".metaData.hidden = true;
        "Bing".metaData.hidden = true;
        "eBay".metaData.hidden = true;

        "DuckDuckGo" = {
          urls = [{
            template = "https://duckduckgo.com";
            params = [
              { name = "q"; value = "{searchTerms}"; }
            ];
          }];
          definedAliases = [ ",d" ];
        };
        "Google" = {
          urls = [{
            template = "https://google.com/search";
            params = [
              { name = "q"; value = "{searchTerms}"; }
            ];
          }];
          definedAliases = [ ",g" ];
        };
        "Nix Packages" = {
          urls = [{
            template = "https://search.nixos.org/packages";
            params = [
              { name = "type"; value = "packages"; }
              { name = "query"; value = "{searchTerms}"; }
            ];
          }];
          definedAliases = [ ",ns" ];
        };
        "YouTube" = {
          urls = [{
            template = "https://www.youtube.com/results";
            params = [
              { name = "search_query"; value = "{searchTerms}"; }
            ];
          }];
          definedAliases = [ ",yt" ];
        };
        "Wikipedia" = {
          urls = [{
            template = "https://en.wikipedia.org/wiki/Special:Search";
            params = [
              { name = "search"; value = "{searchTerms}"; }
            ];
          }];
          definedAliases = [ ",w" ];
        };
        "DockerHub" = {
          urls = [{
            template = "https://hub.docker.com/search";
            params = [
              { name = "q"; value = "{searchTerms}"; }
            ];
          }];
          definedAliases = [ ",dh" ];
        };
        "GitHub" = {
          urls = [{
            template = "https://github.com/search";
            params = [
              { name = "q"; value = "{searchTerms}"; }
            ];
          }];
          definedAliases = [ ",gh" ];
        };
      };
    };

    bookmarks = [
      {
        name = "GitHub Notifications";
        keyword = "gn";
        url = "https://github.com/notifications";
      }
      {
        name = "Calendar";
        keyword = "gcal";
        url = "https://calendar.google.com/calendar/r";
      }
      {
        name = "Email";
        keyword = "gmail";
        url = "https://mail.google.com/mail/u/0/#inbox";
      }
      {
        name = "Outlook Calendar";
        keyword = "ocal";
        url = "https://outlook.office.com/calendar/view/month";
      }
      {
        name = "Outlook Mail";
        keyword = "omail";
        url = "https://outlook.office.com/mail/inbox";
      }
      {
        name = "Syncthing";
        keyword = "sync";
        url = "http://localhost:8384/";
      }
    ] ++ rbm;

    settings = {
      "dom.security.https_only_mode" = true; # force https
      "browser.download.panel.shown" = true; # show download panel
      "identity.fxaccounts.enabled" = false; # disable firefox accounts
      "signon.rememberSignons" = false; # disable saving passwords
      "extensions.pocket.enabled" = false; # disable pocket
      "app.shield.optoutstudies.enabled" = false; # disable shield studies
      "app.update.auto" = false; # disable auto update
      "browser.bookmarks.restore_default_bookmarks" = false; # don't restore default bookmarks
      "browser.quitShortcut.disabled" = true; # disable ctrl+q
      "browser.shell.checkDefaultBrowser" = false; # don't check if default browser

      # download handling
      "browser.download.dir" = "/home/meain/down"; # default download dir
      "browser.startup.page" = 3; # restore previous session

      # ui changes
      "browser.aboutConfig.showWarning" = false; # disable warning about about:config
      "browser.compactmode.show" = true; # enable compact mode
      "browser.uidensity" = 1;
      "general.autoScroll" = true; # enable autoscroll
      "browser.tabs.firefox-view" = false; # enable firefox view
      "browser.toolbars.bookmarks.visibility" = "never"; # hide bookmarks toolbar
      "media.videocontrols.picture-in-picture.video-toggle.enabled" = false; # disable picture in picture button
      "startup.homepage_welcome_url" = ""; # disable welcome page
      "browser.newtabpage.enabled" = false; # disable new tab page
      # "toolkit.legacyUserProfileCustomizations.stylesheets" = true; # enable userChrome.css
      "full-screen-api.ignore-widgets" = true; # fullscreen within window

      # privacy
      "browser.contentblocking.category" = "custom"; # set tracking protection to custom
      "browser.discovery.enabled" = false; # disable discovery
      "browser.search.suggest.enabled" = false; # disable search suggestions
      "browser.protections_panel.infoMessage.seen" = true; # disable tracking protection info

      # let me close and open tabs without confirmation
      "browser.tabs.closeWindowWithLastTab" = false; # don't close window when last tab is closed
      "browser.tabs.loadBookmarksInTabs" = true; # open bookmarks in new tab
      "browser.tabs.loadDivertedInBackground" = false; # open new tab in background
      "browser.tabs.loadInBackground" = true; # open new tab in background
      "browser.tabs.warnOnClose" = false; # don't warn when closing multiple tabs
      "browser.tabs.warnOnCloseOtherTabs" = false; # don't warn when closing multiple tabs
      "browser.tabs.warnOnOpen" = false; # don't warn when opening multiple tabs
      "browser.tabs.warnOnQuit" = false; # don't warn when closing multiple tabs

      # other
      "devtools.cache.disabled" = true; # disable caching in devtools
      "devtools.toolbox.host" = "right"; # move devtools to right
      # "browser.ssb.enabled" = true; # enable site specific browser
      "media.autoplay.default" = 0; # enable autoplay on open
      "media.ffmpeg.vaapi.enabled" = true; # enable hardware acceleration
      "media.rdd-vpx.enabled" = true; # enable hardware acceleration

      # override fonts (Set tracking protection to custom without "Suspected fingerprinters")
      "font.minimum-size.x-western" = 13;
      "font.size.fixed.x-western" = 15;
      "font.size.monospace.x-western" = 15;
      "font.size.variable.x-western" = 15;
      "font.name.monospace.x-western" = "${defaultFont}";
      "font.name.sans-serif.x-western" = "${defaultFont}";
      "font.name.serif.x-western" = "${defaultFont}";
      "browser.display.use_document_fonts" = 0;

      # do not open a tab in a new window
      # ascentpayroll.net open link in a new without without any
      # chrome and I can't even use my password manager
      # https://support.mozilla.org/eu/questions/1151067?&mobile=1
      "browser.link.open_newwindow.restriction" = 0;
    };

    userChrome = ''
      /* some css */
    '';

    # Configuring addons can be done via
    # https://discourse.nixos.org/t/declare-firefox-extensions-and-settings/36265/7
    extensions = with firefox-addons.packages."${pkgs.system}"; [
      # https://gitlab.com/rycee/nur-expressions/-/blob/master/pkgs/firefox-addons/addons.json?ref_type=heads
      bitwarden
      clearurls
      darkreader
      decentraleyes
      greasemonkey
      multi-account-containers
      privacy-badger
      redirector
      refined-github
      return-youtube-dislikes
      sidebery
      sponsorblock
      stylus
      ublock-origin
      vimium
      wallabagger
      youtube-shorts-block
      # youtube-recommended-videos

      # personally packaged
      ppkgs.firefox-addons.awesome-rss
      ppkgs.firefox-addons.aw-watcher-web
      ppkgs.firefox-addons.containerise
      ppkgs.firefox-addons.ghostpage
      ppkgs.firefox-addons.mastodon4-redirect
      ppkgs.firefox-addons.nattynote
      ppkgs.firefox-addons.netflix-prime-auto-skip
      ppkgs.firefox-addons.notifications-preview-github
      ppkgs.firefox-addons.smartreader
      ppkgs.firefox-addons.unofficial-hypothesis
      ppkgs.firefox-addons.watchmarker-for-youtube
      # ppkgs.firefox-addons.a-n-i-m-a-t-e-d-kitty-cat
      # ppkgs.firefox-addons.global-speed
      ppkgs.firefox-addons.try-another-search-engine
    ];

  };
}
