#!/bin/bash

readonly backup_root=/tmp/backup
readonly pending_root=/tmp/pending
readonly photoregex='.*\(jpg\|raf\|cr2\)'

#readonly backup_root=/btrfs/staging/import_tmp
#readonly pending_root=/btrfs/storage/pending-photos
readonly input="$1"

make_unique() {
    if [[ ! -d "$1" ]]; then
        echo "$1"
    else
        extension=1
        while [[ -d "${1}-${extension}" ]]; do
            extension=$(($extension + 1))
        done
        echo "${1}-${extension}"
    fi
}

readonly backup_path="$(make_unique $backup_root/$(date +%Y-%m-%d)-$(basename $input))"
readonly pending_path="$(make_unique $pending_root/$(date +%Y-%m-%d))"

(
    flock -x -w 120 200 || exit 1
    mkdir -p "$backup_path"
    rsync -Hvax "$input/." "$backup_path/."
    echo "Contents backed up to: $backup_path"
    echo "pending_path: $pending_path"

    if [[ -d "$backup_path/DCIM" && \
          -n $(find "$backup_path/DCIM" -iregex "$photoregex" -print -quit) ]]; then
        echo "Found photos"
        echo mkdir -p "${pending_path}.importing"
        echo rsync -avx "$backup_path"/DCIM/*/* "${pending_path}.importing/"
        echo mv "${pending_path}.importing" "${pending_path}"
    fi

    echo "Deleting original files"
    rsync -Hvax --remove-source-files "$input/." "$backup_path/."
    echo umount "${input}"
) 200>/tmp/photobox_import2.lock | systemd-cat -t photobox_import
