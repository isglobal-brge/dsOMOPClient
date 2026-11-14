#!/usr/bin/env bash
set -euo pipefail
read -r host port < <(python3 /workspace/astra/runpod/deckpod.py ssh-target)
ssh -i "$HOME/.ssh/runpod_cover" -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null -o LogLevel=ERROR -p "$port" "root@$host" "cd /root/rt/wt/book/dsOMOPClient && R_LIBS=/root/rt/wt/book/lib R_LIBS_USER=/root/rt/labels-deps $*"
