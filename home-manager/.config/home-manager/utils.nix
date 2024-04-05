{ pkgs }: {
  ss-simple = { cmd, wait }: {
    Service.Type = "simple";
    Service.ExecStart = "${pkgs.zsh}/bin/zsh -ic '${cmd}'";
    Install.WantedBy = [ "default.target" ];
    Service.Restart = "always";
    Service.RestartSec = wait;
  };
  ss-timer = { cmd }: {
    Service.Type = "oneshot";
    Service.ExecStart = "${pkgs.zsh}/bin/zsh -ic '${cmd}'";
    Install.WantedBy = [ "default.target" ];
  };
  ss-git-sync = { dir }: {
    Service.Type = "oneshot";
    Service.WorkingDirectory = dir;
    Service.ExecStart = "${pkgs.zsh}/bin/zsh -ic ',git-auto-sync'";
    Install.WantedBy = [ "default.target" ];
  };
  ss-cleanup = { dir }: {
    Service.Type = "oneshot";
    Service.WorkingDirectory = dir;
    Service.ExecStart = "${pkgs.zsh}/bin/zsh -ic ',cleanup-folder'";
    Install.WantedBy = [ "default.target" ];
  };
  timer-min = { min }: {
    Timer.OnCalendar = "*:0/${min}";
    Timer.Persistent = true;
    Install.WantedBy = [ "timers.target" ];
  };
  timer-daily = {
    Timer.OnCalendar = "*-*-* *:00:00";
    Timer.Persistent = true;
    Install.WantedBy = [ "timers.target" ];
  };

  # generate github bookmarks
  gh-bookmarks = { repo, basename, basekeyword }:
    [
      {
        name = basename;
        tags = [ "github" ];
        keyword = basekeyword;
        url = "https://github.com/" + repo;
      }
      {
        name = basename + " Issues";
        tags = [ "github" ];
        keyword = basekeyword + "i";
        url = "https://github.com/" + repo + "/issues";
      }
      {
        name = basename + " Issues Assigned to me";
        tags = [ "github" ];
        keyword = basekeyword + "im";
        url = "https://github.com/" + repo + "/issues/assigned/@me";
      }
      {
        name = basename + " Pull Requests";
        tags = [ "github" ];
        keyword = basekeyword + "p";
        url = "https://github.com/" + repo + "/pulls";
      }
      {
        name = basename + " Pull Requests Assigned to me";
        tags = [ "github" ];
        keyword = basekeyword + "pm";
        url = "https://github.com/" + repo + "/pulls/assigned/@me";
      }
      {
        name = basename + " Pull Requests Review Requested to me";
        tags = [ "github" ];
        keyword = basekeyword + "pr";
        url = "https://github.com/" + repo + "/pulls/review-requested/@me";
      }
      {
        name = basename + " Pull Requests Review Requested to me and not approved";
        tags = [ "github" ];
        keyword = basekeyword + "prr";
        url = "https://github.com/" + repo + "/pulls?q=is%3Aopen+is%3Apr+review-requested%3A%40me+-review%3Aapproved";
      }
      {
        name = basename + " Actions";
        tags = [ "github" ];
        keyword = basekeyword + "a";
        url = "https://github.com/" + repo + "/actions";
      }
      {
        name = basename + " Queue";
        tags = [ "github" ];
        keyword = basekeyword + "q";
        url = "https://github.com/" + repo + "/queue/main";
      }
      {
        name = basename + " Queue";
        tags = [ "github" ];
        keyword = basekeyword + "qq";
        url = "https://github.com/" + repo + "/queue/master";
      }
    ];
}
