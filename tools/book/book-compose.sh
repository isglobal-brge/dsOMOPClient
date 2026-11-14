#!/usr/bin/env bash
# Keep the book mount when managing the optional easy-opal stack.
set -euo pipefail
book_instance_dir="${BOOK_OPAL_INSTANCE_DIR:-$HOME/.easy-opal/instances/book}"
book_work_dir="$(cd "$(dirname "$0")/../.." && pwd)"
book_data_dir="$(cd "$book_work_dir/../practice-data" && pwd)"
export BOOK_DATA_DIR="$book_data_dir"
exec docker compose --project-name book \
  -f "$book_instance_dir/docker-compose.yml" \
  -f "$book_work_dir/tools/book/book-rock.yml" "$@"
