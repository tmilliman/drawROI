### Dockerized drawROI:

This is a fork of the [PhenoCam drawROI tool](https://github.com/bnasr/drawROI)
with modifications for running in a docker container.

Changes:

* remove unneccessary files and modify .gitignore so they aren't picked up
* modify global.R removing any unnecessary hacks
* modify global.R with assumption images are at /data/archive
