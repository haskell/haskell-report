{ owner  ? "NixOS"
, repo   ? "nixpkgs-channels"
, rev    ? "f3fa5a101eb74a10f6c53b504e18b42bacccbaeb"
, sha256 ? "0mf8n78xhni57f5pgc5rc7l3sgnar95998gi06a1anf8gqnrrhn1"
}:

import (builtins.fetchTarball {
  url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
  inherit sha256;
}) {}
