#!/bin/bash

if [[ $- != *i* ]] ; then
    # Shell is non-interactive.  Be done now!
    return
fi

[ -n "$ENV" ] && . "$ENV"
