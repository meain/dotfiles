let output = '';
const musicSources = ['Spotify', 'iTunes']
for(source in musicSources){
    if ( Application(musicSources[source]).running() ){
        const track = Application(musicSources[source]).currentTrack;
        const artist = track.artist();
        const title = track.name();
        output = (`♫ ${title} - ${artist}`).substr(0, 50);
        break
    }
}
