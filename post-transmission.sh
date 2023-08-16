#!/bin/sh

# TR_APP_VERSIO
# TR_TIME_LOCALTIME
# TR_TORRENT_DIR
# TR_TORRENT_HASH
# TR_TORRENT_ID
# TR_TORRENT_NAME

if [ -z ${TRANSMISSION_DETAILS+x} ]; then
  tr -t $TR_TORRENT_ID -r >> ~/transmission-log.log
fi
