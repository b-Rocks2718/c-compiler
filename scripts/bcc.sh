
function bcc {
    python3 scripts/bcc.py $@
}

function basm {
    python3 scripts/basm.py $@
}

function bemu {
    cargo run -- $@
}