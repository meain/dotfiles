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
}
