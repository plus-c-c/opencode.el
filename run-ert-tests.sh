#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"

mapfile -d '' test_files < <(
  find "$repo_root" -type f \
    \( -name '*-test.el' -o -name '*-tests.el' \) \
    -not -path "$repo_root/.git/*" \
    -print0
)

if ((${#test_files[@]} == 0)); then
  printf 'No ERT test files found.\n' >&2
  exit 1
fi

elisp="(progn
  (add-to-list 'load-path \"${repo_root//\\/\\\\}\")"

for test_file in "${test_files[@]}"; do
  escaped="${test_file//\\/\\\\}"
  escaped="${escaped//\"/\\\"}"
  elisp+="
  (load \"$escaped\" nil nil t)"
done

elisp+="
  (let ((stats (ert-run-tests-batch t)))
    (if (zerop (ert-stats-completed-unexpected stats))
        \"PASS\"
      \"FAIL\")))"

result="$(emacsclient --eval "$elisp")"

if [[ "$result" == '"PASS"' ]]; then
    exit 0
else
    echo "Tests failed" >&2
    exit 1
fi
