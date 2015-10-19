#!/bin/bash
log() {
  echo "$(date "+%b %d %H:%M:%S") photobox_import: $@" >> /var/log/photobox
}

readonly photoregex='.*\(jpg\|raf\|cr2\)'
readonly target=$(mktemp -u /srv/photos/pending/$(date +%Y-%m-%d).XXX)

mnt=$(mktemp -d)
mount "$DEVNAME" "$mnt"
if [[ -d "$mnt/DCIM" && \
     -n $(find "$mnt/DCIM" -iregex "$photoregex" -print -quit) ]]; then
  umask 000
  mkdir -p "$target.importing"
  rsync -avx "$mnt"/DCIM/*/* "$target.importing/"
  mv "$target.importing" "$target"
fi
umount "$mnt"
