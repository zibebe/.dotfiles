function cuesplit
    for f in *.flac; mv $f $f.bak; end
    and cuebreakpoints *.cue | shnsplit -o flac *.flac.bak
    and cuetag.sh *.cue split-track*.flac
    and rm *.flac.bak
end
