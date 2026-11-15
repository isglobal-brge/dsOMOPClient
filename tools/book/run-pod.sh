#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../.."
read -r host port < <(python3 /workspace/astra/runpod/deckpod.py ssh-target)
opts=(-i "$HOME/.ssh/runpod_cover" -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null -o LogLevel=ERROR -p "$port")
remote=/root/rt/wt/book
server_source=${BOOK_SERVER_SOURCE:-/workspace/dsomop/wt/book/dsOMOP}
client_source=${BOOK_CLIENT_SOURCE:-/workspace/dsomop/wt/book/dsOMOPClient}
ssh "${opts[@]}" "root@$host" "mkdir -p $remote/dsOMOPClient/tools/book/evidence $remote/runtime-server $remote/runtime-client $remote/lib"
rsync -az --delete --exclude=.git --exclude=docs --exclude=tools/book/evidence -e "ssh -i $HOME/.ssh/runpod_cover -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null -o LogLevel=ERROR -p $port" ./ "root@$host:$remote/dsOMOPClient/"
rsync -az --delete --exclude=.git --exclude=docs --exclude=tools/book/evidence -e "ssh -i $HOME/.ssh/runpod_cover -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null -o LogLevel=ERROR -p $port" "$server_source/" "root@$host:$remote/runtime-server/"
rsync -az --delete --exclude=.git --exclude=docs --exclude=tools/book/evidence -e "ssh -i $HOME/.ssh/runpod_cover -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null -o LogLevel=ERROR -p $port" "$client_source/" "root@$host:$remote/runtime-client/"
if [[ "${1:-}" != precompute ]]; then
ssh "${opts[@]}" "root@$host" "R CMD INSTALL -l $remote/lib $remote/runtime-server > $remote/install-server.log 2>&1 && R CMD INSTALL -l $remote/lib $remote/runtime-client > $remote/install-client.log 2>&1"
fi
ssh "${opts[@]}" "root@$host" "test -f $remote/practice-data/manifest.json && test -f $remote/practice-data/gibleed.sqlite && test -f $remote/practice-data/numeric.sqlite || (cd $remote/dsOMOPClient && R_LIBS=$remote/lib R_LIBS_USER=/root/rt/labels-deps Rscript tools/book/load-practice.R $remote/practice-data)"
ssh "${opts[@]}" "root@$host" "cd $remote/dsOMOPClient && R_LIBS=$remote/lib R_LIBS_USER=/root/rt/labels-deps Rscript tools/book/precompute.R $(printf %q "${2:-book-}")" 2>&1 | tee tools/book/evidence/precompute.log
rsync -az --include='*/' --include='*.Rmd' --exclude='*' -e "ssh -i $HOME/.ssh/runpod_cover -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null -o LogLevel=ERROR -p $port" "root@$host:$remote/dsOMOPClient/vignettes/" vignettes/
rsync -az -e "ssh -i $HOME/.ssh/runpod_cover -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null -o LogLevel=ERROR -p $port" "root@$host:$remote/dsOMOPClient/tools/book/evidence/session-info.txt" tools/book/evidence/
