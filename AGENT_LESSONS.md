Workarounds in this environment:

- `cabal` commands can fail writing `~/.cache/cabal/logs/build.log`. Add `--build-log=.cache/cabal/logs/build.log` (and ensure `.cache/cabal/logs` exists) to avoid the permission error.
- `cabal build` can still exit 1 at the end in this harness after a successful compile because it tries to write `~/.cache/cabal/logs/build.log`; prefer `cabal test ... --build-log=.cache/cabal/logs/build.log` for verified builds, or treat the built artifacts as authoritative if the only tail failure is that log write.
- Integration tests can fail with `Network.Socket.socket: permission denied (Operation not permitted)` in the sandbox; rerun with escalated permissions to allow socket access.
- System tests require Docker/network access; rerun with escalated permissions if sandboxed.
- `stylish-haskell -ri -c stylish.yaml .` sometimes exits 1 with no output here; rerun with `-v` (`stylish-haskell -rvi -c stylish.yaml .`) to get diagnostics.
- `stylish-haskell` may not be on the ambient PATH under `rtk`; run it through the dev shell, e.g. `rtk nix develop -c stylish-haskell -rvi -c stylish.yaml .`.
- `nix flake check` can exceed the default command timeout; rerun with a longer timeout in this harness.
- GHC 8.8 / older `bytestring` does not expose `Data.ByteString.dropWhileEnd`; use a local compatibility helper built from `BS.reverse . BS.dropWhile predicate . BS.reverse`.
- nix commands only sees tracked files; new modules must live under tracked paths or be added by the user.
- `nix flake check path:.` includes an existing `dist-newstyle` in the read-only Nix source and Cabal then fails writing its cache. Temporarily move that generated directory outside the worktree, run the check, and restore it.
- system tests can intermittently time out; rerunning `cabal test system-tests` usually succeeds (failure logs are left under `/tmp/nix-shell.*` by `TestSupport`).
- running multiple `cabal test` commands in parallel against the same worktree can race in `dist-newstyle` and fail with missing registration/cache files; run Cabal test suites serially.
- `testcontainers` `waitForLogLine` can miss very early readiness lines from fast-starting containers like NATS in this suite; using `withFollowLogs` plus an explicit poll of the captured log file is more reliable than relying on the built-in log wait.
- `nix develop` may need to update `~/.cache/nix/fetcher-cache-v3.sqlite` after dependency changes; if the workspace sandbox makes that cache read-only, rerun the command with escalated permissions.
- The `tls` 2.x dependency uses the `crypton-x509-*` package family. Use `crypton-x509-system`, not `x509-system`, so its `CertificateStore` type matches `TLS.sharedCAStore`.
- Socket-heavy reconnect tests can make a non-threaded GHC test executable fail with `file descriptor ... out of range for select`; compile the integration suite with `-threaded` so it uses the scalable I/O manager.
- `gh pr edit` can fail while querying deprecated classic project cards; update the pull request with `gh api --method PATCH repos/<owner>/<repo>/pulls/<number> -F body=@<file>` instead.
