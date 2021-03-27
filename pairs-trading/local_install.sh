#!/bin/bash

set -o errexit

R CMD INSTALL --no-multiarch --with-keep.source datatools
R CMD INSTALL --no-multiarch --with-keep.source strategy