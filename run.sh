#!/bin/sh

sbcl --load "jupiter-candle.asd" --eval "(ql:quickload :jupiter-candle)" --eval "(jupiter-candle:main")
