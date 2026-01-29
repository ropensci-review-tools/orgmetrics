#!/bin/bash

echo "Extracting data for all repos in 'packages.json' ..."
cd /repo
Rscript -e 'orgmetrics::orgmetrics_deploy_r_univ()'

if [ ! -d "quarto" ]; then
    echo "Error: quarto directory not created."
    exit 1
fi

# Check if any volume is mounted by looking for additional mount points
if grep "^/dev/" /etc/mtab | grep -v "\ /etc/" | grep -q " "; then
    # Find the first mounted volume to copy to
    VOLUME_PATH=$(grep "^/dev/" /etc/mtab | grep -v "\ /etc/" | awk '{print $2}' | head -1)
    if [ -n "$VOLUME_PATH" ]; then
        cp -r quarto/* "$VOLUME_PATH/"
        echo "Quarto directory copied to $VOLUME_PATH"
    fi
fi

cd quarto
quarto publish gh-pages
cd ..
