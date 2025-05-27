# miso-audio-test

Build and run:

```
nix develop
make
make serve
```

Try online:

- [https://juliendehos.github.io/miso-audio-test/](https://juliendehos.github.io/miso-audio-test/)

References:

- [HTML DOM Audio Object](https://www.w3schools.com/jsref/dom_obj_audio.asp)
- [Web Audio API](https://developer.mozilla.org/en-US/docs/Web/API/Web_Audio_API)

Audio Object Properties:

- [ ] audioTracks 	        Returns an AudioTrackList object representing available audio tracks
- [ ] autoplay 	            Sets or returns whether the audio should start playing as soon as it is ready
- [ ] buffered 	            Returns a TimeRanges object representing the buffered parts of an audio
- [ ] controller 	        Returns the MediaController object representing the current media controller of an audio
- [ ] controls 	            Sets or returns whether an audio should have controls displayed (play/pause etc)
- [ ] crossOrigin 	        Sets or returns the CORS settings of an audio
- [ ] currentSrc 	        Returns the URL of the current audio
- [ ] currentTime 	        Sets or returns the current playback position in an audio (in seconds)
- [ ] defaultMuted 	        Sets or returns whether the audio should be muted by default
- [ ] defaultPlaybackRate 	Sets or returns whether the default playback speed of the audio
- [ ] duration 	            Returns the length of an audio(in seconds)
- [x] ended 	            Returns whether the playback of the audio has ended
- [ ] error 	            Returns a MediaError object representing the error state of the audio
- [ ] loop 	                Sets or returns whether the audio should start playing over again, every time it is finished
- [ ] mediaGroup 	        Sets or returns the name of the media group the audio(s) is a part of
- [ ] muted 	            Sets or returns whether the sound should be turned off
- [ ] networkState 	        Returns the current network state of an audio
- [x] paused 	            Returns whether an audio is paused
- [ ] playbackRate 	        Sets or returns the speed of the audio playback
- [x] played 	            Returns a TimeRanges object representing the played parts of the audio
- [ ] preload 	            Sets or returns the value of the preload attribute of an audio
- [ ] readyState 	        Returns the current ready state of an audio
- [ ] seekable 	            Returns a TimeRanges object representing the seekable parts of an audio
- [ ] seeking 	            Returns whether the user is currently seeking in the audio
- [ ] src 	                Sets or returns the value of the src attribute of an audio
- [ ] textTracks 	        Returns a TextTrackList object representing the available text tracks
- [ ] volume 	            Sets or returns the audio volume of an audio

Audio Object Methods:

- [ ] addTextTrack() 	Adds a new text track to the audio
- [ ] canPlayType() 	Checks whether the browser can play the specified audio type
- [ ] fastSeek() 	    Seeks to a specified time in the audio player
- [ ] getStartDate() 	Returns a new Date object, representing the current timeline offset
- [ ] load() 	        Re-loads the audio element
- [x] play() 	        Starts playing the audio
- [x] pause() 	        Pauses the currently playing audio

