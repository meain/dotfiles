if application "Spotify" is running then
  tell application "Spotify"
    set theName to name of the current track
    set theArtist to artist of the current track
    set theAlbum to album of the current track
    set theUrl to spotify url of the current track

    set playerState to player state
    if playerState is playing then
      set playIcon to "▶"
    end if
    if playerState is paused then
      set playIcon to "❙❙"
    end if

    try
      return playIcon & " " & theName & " by " & theArtist
    on error err
    end try
  end tell
end if
