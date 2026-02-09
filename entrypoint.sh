#!/bin/bash

echo "Setting up git config  ..."
echo "GITHUB_TOKEN='$GITHUB_TOKEN'" > ~/.Renviron \
    && echo "GITHUB_PAT='$GITHUB_TOKEN'" >> ~/.Renviron \
    && git config --global user.name "$GIT_USER_NAME" \
    && git config --global user.email "$GIT_USER_EMAIL" \
    && git config --global credential.helper store \
    && echo "https://x-access-token:$GITHUB_TOKEN@github.com" > ~/.git-credentials \
    && chmod 600 ~/.git-credentials \
    && git clone "$GIT_REMOTE_URL" repo

echo "Extracting data for all repos in 'packages.json' ..."
cd /repo
Rscript -e 'orgmetrics::orgmetrics_deploy_r_univ()'

if [ ! -d "quarto" ]; then
    echo "Error: quarto directory not created."
    exit 1
fi

# Check if any volume is mounted by looking for additional mount points
if grep "^/dev/" /etc/mtab | grep -v "\ /etc/" | grep " "; then
    # Find the first mounted volume to copy to
    VOLUME_PATH=$(grep "^/dev/" /etc/mtab | grep -v "\ /etc/" | awk '{print $2}' | head -1)
    if [ -n "$VOLUME_PATH" ]; then
        mkdir -p "$VOLUME_PATH/quarto"
        cp -r quarto/* "$VOLUME_PATH/quarto/."
        echo "Quarto directory copied to $VOLUME_PATH/quarto"
    fi
fi

if [ "$QUARTO_PUBLISH" = "true" ]; then
    cd quarto
    quarto publish "$QUARTO_PROVIDER" --no-prompt
    cd ..
fi
