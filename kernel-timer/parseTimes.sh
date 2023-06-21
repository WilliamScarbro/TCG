#!/bin/bash

echo $1 | sed "s/),(/\n/g" | sed "s/(//g" | sed "s/)//g" | sed "s/,/;/g"
