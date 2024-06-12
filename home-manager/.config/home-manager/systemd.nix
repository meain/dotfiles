{
  services = {
    activitywatch = {
      Unit.Description = "Start ActivityWatch";
      Service.Type = "simple";
      Service.ExecStart = "${pkgs.activitywatch}/bin/aw-server";
      Install.WantedBy = [ "default.target" ];
      Service.Restart = "on-failure";
      Service.RestartSec = 5;
    };

    activitywatch-afk = {
      Unit.Description = "Start ActivityWatch AFK";
      Service.Type = "simple";
      Service.ExecStart = "${pkgs.activitywatch}/bin/aw-watcher-afk";
      Install.WantedBy = [ "default.target" ];
      Service.Restart = "on-failure";
      Service.RestartSec = 5;
    };

    activitywatch-window = {
      Unit.Description = "Start ActivityWatch Window";
      Service.Type = "simple";
      Service.ExecStart = "${pkgs.activitywatch}/bin/aw-watcher-window";
      Install.WantedBy = [ "default.target" ];
      Service.Restart = "on-failure";
      Service.RestartSec = 5;
    };
  };


  services.kopia-ui = {
    Unit.Description = "Kopia UI";
    Service.Type = "simple";
    Service.ExecStart = "kopia-ui"; # currently not installed via nix
    Install.WantedBy = [ "default.target" ];
    Service.Restart = "on-failure";
    Service.RestartSec = 5;
  };

  # startServices = true;  # enabling this increases switch time a lot
  services = {
    mpd = utils.ss-simple { cmd = "mpd --no-daemon"; wait = 3; };
    mpd-mpris = utils.ss-simple { cmd = "${pkgs.mpd-mpris}/bin/mpd-mpris"; wait = 3; }; # playerctl for mpd
    clipmenud = utils.ss-simple { cmd = "clipmenud"; wait = 3; };
    sxhkd = utils.ss-simple { cmd = "${pkgs.sxhkd}/bin/sxhkd"; wait = 3; };
    qutebrowser = utils.ss-simple { cmd = "qutebrowser"; wait = 3; };
    emacs = utils.ss-simple { cmd = "emacs --fg-daemon"; wait = 1; };
    logseq = utils.ss-simple { cmd = "/var/lib/flatpak/app/com.logseq.Logseq/current/active/export/bin/com.logseq.Logseq"; wait = 1; };
    floatingterm = utils.ss-simple { cmd = "sakura --name floatingterm -x \"tt floating\""; wait = 1; };
    mail-watcher = utils.ss-simple { cmd = "find /home/meain/.local/share/mail/.notmuch/xapian|entr -n ,shellout-update"; wait = 5; };
  };

  # code/note sync
  services.note-sync = utils.ss-git-sync { dir = "/home/meain/.local/share/notes"; };
  timers.note-sync = utils.timer-daily;
  services.ledger-sync = utils.ss-git-sync { dir = "/home/meain/.local/share/ledger"; };
  timers.ledger-sync = utils.timer-daily;
  services.journal-sync = utils.ss-git-sync { dir = "/home/meain/.local/share/journal"; };
  timers.journal-sync = utils.timer-daily;
  services.logseq-sync = utils.ss-git-sync { dir = "/home/meain/.local/share/logseq"; };
  timers.logseq-sync = utils.timer-daily;

  # syncing things
  services.email-sync = utils.ss-timer { cmd = ",mail-sync"; };
  timers.email-sync = utils.timer-min { min = "15"; };
  services.weather-pull = utils.ss-timer { cmd = ",weather-current"; };
  timers.weather-pull = utils.timer-min { min = "30"; };
  services.battery-check = utils.ss-timer { cmd = ",low-battery-notify"; };
  timers.battery-check = utils.timer-min { min = "5"; };
  services.update-sct = utils.ss-timer { cmd = ",update-sct"; };
  timers.update-sct = utils.timer-min { min = "30"; };
  services.update-calendar = utils.ss-timer { cmd = ",upcoming-events"; };
  timers.update-calendar = utils.timer-min { min = "10"; }; # actual pull is hourly
  services.mscripts-backup = utils.ss-timer { cmd = ",projects-config-backup"; };
  timers.mscripts-backup = utils.timer-daily;

  # regular cleanup
  services.cleanup-downloads = utils.ss-cleanup { dir = "/home/meain/down"; };
  timers.cleanup-downloads = utils.timer-daily;
  services.cleanup-scratch = utils.ss-cleanup { dir = "/home/meain/.local/share/scratch"; };
  timers.cleanup-scratch = utils.timer-daily;

  # other
  services.drink-water = utils.ss-timer { cmd = ",drink-water-notify"; };
  timers.drink-water = utils.timer-min { min = "40"; };
}
