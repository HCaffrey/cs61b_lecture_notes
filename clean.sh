#!/bin/bash
for f in $( ls | grep "notes[0-9]\+.org" ); do
     rm $f
done
