# History is nice in shells, let's have that
export ERL_AFLAGS="-kernel shell_history enabled"

# EDTS needs to know where erlexec is
export PATH=$PATH:$(dirname "$(which erl)")/../lib/erlang/erts-10.4/bin/

# Use nix set-up
use_nix
